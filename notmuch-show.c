/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2009 Carl Worth
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include "notmuch-client.h"

static void
format_message_text (unused (const void *ctx),
		     notmuch_message_t *message,
		     int indent);
static void
format_headers_text (const void *ctx,
		     notmuch_message_t *message);

static void
format_headers_message_part_text (GMimeMessage *message);

static void
format_part_start_text (GMimeObject *part,
			int *part_count);

static void
format_part_content_text (GMimeObject *part);

static void
format_part_end_text (GMimeObject *part);

static const notmuch_show_format_t format_text = {
    "",
	"\fmessage{ ", format_message_text,
	    "\fheader{\n", format_headers_text, format_headers_message_part_text, "\fheader}\n",
	    "\fbody{\n",
	        format_part_start_text,
	        NULL,
	        NULL,
	        format_part_content_text,
	        format_part_end_text,
	        "",
	    "\fbody}\n",
	"\fmessage}\n", "",
    ""
};

static void
format_message_json (const void *ctx,
		     notmuch_message_t *message,
		     unused (int indent));
static void
format_headers_json (const void *ctx,
		     notmuch_message_t *message);

static void
format_headers_message_part_json (GMimeMessage *message);

static void
format_part_start_json (unused (GMimeObject *part),
			int *part_count);

static void
format_part_encstatus_json (int status);

static void
format_part_sigstatus_json (const GMimeSignatureValidity* validity);

static void
format_part_content_json (GMimeObject *part);

static void
format_part_end_json (GMimeObject *part);

static const notmuch_show_format_t format_json = {
    "[",
	"{", format_message_json,
	    "\"headers\": {", format_headers_json, format_headers_message_part_json, "}",
	    ", \"body\": [",
	        format_part_start_json,
	        format_part_encstatus_json,
	        format_part_sigstatus_json,
	        format_part_content_json,
	        format_part_end_json,
	        ", ",
	    "]",
	"}", ", ",
    "]"
};

static void
format_message_mbox (const void *ctx,
		     notmuch_message_t *message,
		     unused (int indent));

static const notmuch_show_format_t format_mbox = {
    "",
        "", format_message_mbox,
            "", NULL, NULL, "",
            "",
                NULL,
                NULL,
                NULL,
                NULL,
                NULL,
                "",
            "",
        "", "",
    ""
};

static void
format_part_content_raw (GMimeObject *part);

static const notmuch_show_format_t format_raw = {
    "",
	"", NULL,
	    "", NULL, format_headers_message_part_text, "\n",
            "",
                NULL,
                NULL,
                NULL,
                format_part_content_raw,
                NULL,
                "",
            "",
	"", "",
    ""
};

static const char *
_get_tags_as_string (const void *ctx, notmuch_message_t *message)
{
    notmuch_tags_t *tags;
    int first = 1;
    const char *tag;
    char *result;

    result = talloc_strdup (ctx, "");
    if (result == NULL)
	return NULL;

    for (tags = notmuch_message_get_tags (message);
	 notmuch_tags_valid (tags);
	 notmuch_tags_move_to_next (tags))
    {
	tag = notmuch_tags_get (tags);

	result = talloc_asprintf_append (result, "%s%s",
					 first ? "" : " ", tag);
	first = 0;
    }

    return result;
}

/* Get a nice, single-line summary of message. */
static const char *
_get_one_line_summary (const void *ctx, notmuch_message_t *message)
{
    const char *from;
    time_t date;
    const char *relative_date;
    const char *tags;

    from = notmuch_message_get_header (message, "from");

    date = notmuch_message_get_date (message);
    relative_date = notmuch_time_relative_date (ctx, date);

    tags = _get_tags_as_string (ctx, message);

    return talloc_asprintf (ctx, "%s (%s) (%s)",
			    from, relative_date, tags);
}

static void
format_message_text (unused (const void *ctx), notmuch_message_t *message, int indent)
{
    printf ("id:%s depth:%d match:%d filename:%s\n",
	    notmuch_message_get_message_id (message),
	    indent,
	    notmuch_message_get_flag (message, NOTMUCH_MESSAGE_FLAG_MATCH),
	    notmuch_message_get_filename (message));
}

static void
format_message_json (const void *ctx, notmuch_message_t *message, unused (int indent))
{
    notmuch_tags_t *tags;
    int first = 1;
    void *ctx_quote = talloc_new (ctx);
    time_t date;
    const char *relative_date;

    date = notmuch_message_get_date (message);
    relative_date = notmuch_time_relative_date (ctx, date);

    printf ("\"id\": %s, \"match\": %s, \"filename\": %s, \"timestamp\": %ld, \"date_relative\": \"%s\", \"tags\": [",
	    json_quote_str (ctx_quote, notmuch_message_get_message_id (message)),
	    notmuch_message_get_flag (message, NOTMUCH_MESSAGE_FLAG_MATCH) ? "true" : "false",
	    json_quote_str (ctx_quote, notmuch_message_get_filename (message)),
	    date, relative_date);

    for (tags = notmuch_message_get_tags (message);
	 notmuch_tags_valid (tags);
	 notmuch_tags_move_to_next (tags))
    {
         printf("%s%s", first ? "" : ",",
               json_quote_str (ctx_quote, notmuch_tags_get (tags)));
         first = 0;
    }
    printf("], ");
    talloc_free (ctx_quote);
}

/* Extract just the email address from the contents of a From:
 * header. */
static const char *
_extract_email_address (const void *ctx, const char *from)
{
    InternetAddressList *addresses;
    InternetAddress *address;
    InternetAddressMailbox *mailbox;
    const char *email = "MAILER-DAEMON";

    addresses = internet_address_list_parse_string (from);

    /* Bail if there is no address here. */
    if (addresses == NULL || internet_address_list_length (addresses) < 1)
	goto DONE;

    /* Otherwise, just use the first address. */
    address = internet_address_list_get_address (addresses, 0);

    /* The From header should never contain an address group rather
     * than a mailbox. So bail if it does. */
    if (! INTERNET_ADDRESS_IS_MAILBOX (address))
	goto DONE;

    mailbox = INTERNET_ADDRESS_MAILBOX (address);
    email = internet_address_mailbox_get_addr (mailbox);
    email = talloc_strdup (ctx, email);

  DONE:
    if (addresses)
	g_object_unref (addresses);

    return email;
   }

/* Return 1 if 'line' is an mbox From_ line---that is, a line
 * beginning with zero or more '>' characters followed by the
 * characters 'F', 'r', 'o', 'm', and space.
 *
 * Any characters at all may appear after that in the line.
 */
static int
_is_from_line (const char *line)
{
    const char *s = line;

    if (line == NULL)
	return 0;

    while (*s == '>')
	s++;

    if (STRNCMP_LITERAL (s, "From ") == 0)
	return 1;
    else
	return 0;
}

/* Print a message in "mboxrd" format as documented, for example,
 * here:
 *
 * http://qmail.org/qmail-manual-html/man5/mbox.html
 */
static void
format_message_mbox (const void *ctx,
		     notmuch_message_t *message,
		     unused (int indent))
{
    const char *filename;
    FILE *file;
    const char *from;

    time_t date;
    struct tm date_gmtime;
    char date_asctime[26];

    char *line = NULL;
    size_t line_size;
    ssize_t line_len;

    filename = notmuch_message_get_filename (message);
    file = fopen (filename, "r");
    if (file == NULL) {
	fprintf (stderr, "Failed to open %s: %s\n",
		 filename, strerror (errno));
	return;
    }

    from = notmuch_message_get_header (message, "from");
    from = _extract_email_address (ctx, from);

    date = notmuch_message_get_date (message);
    gmtime_r (&date, &date_gmtime);
    asctime_r (&date_gmtime, date_asctime);

    printf ("From %s %s", from, date_asctime);

    while ((line_len = getline (&line, &line_size, file)) != -1 ) {
	if (_is_from_line (line))
	    putchar ('>');
	printf ("%s", line);
    }

    printf ("\n");

    fclose (file);
}


static void
format_headers_text (const void *ctx, notmuch_message_t *message)
{
    const char *headers[] = {
	"Subject", "From", "To", "Cc", "Bcc", "Date"
    };
    const char *name, *value;
    unsigned int i;

    printf ("%s\n", _get_one_line_summary (ctx, message));

    for (i = 0; i < ARRAY_SIZE (headers); i++) {
	name = headers[i];
	value = notmuch_message_get_header (message, name);
	if (value && strlen (value))
	    printf ("%s: %s\n", name, value);
    }
}

static void
format_headers_message_part_text (GMimeMessage *message)
{
    InternetAddressList *recipients;
    const char *recipients_string;

    printf ("From: %s\n", g_mime_message_get_sender (message));
    recipients = g_mime_message_get_recipients (message, GMIME_RECIPIENT_TYPE_TO);
    recipients_string = internet_address_list_to_string (recipients, 0);
    if (recipients_string)
	printf ("To: %s\n",
		recipients_string);
    recipients = g_mime_message_get_recipients (message, GMIME_RECIPIENT_TYPE_CC);
    recipients_string = internet_address_list_to_string (recipients, 0);
    if (recipients_string)
	printf ("Cc: %s\n",
		recipients_string);
    printf ("Subject: %s\n", g_mime_message_get_subject (message));
    printf ("Date: %s\n", g_mime_message_get_date_as_string (message));
}

static void
format_headers_json (const void *ctx, notmuch_message_t *message)
{
    const char *headers[] = {
	"Subject", "From", "To", "Cc", "Bcc", "Date"
    };
    const char *name, *value;
    unsigned int i;
    int first_header = 1;
    void *ctx_quote = talloc_new (ctx);

    for (i = 0; i < ARRAY_SIZE (headers); i++) {
	name = headers[i];
	value = notmuch_message_get_header (message, name);
	if (value)
	{
	    if (!first_header)
		fputs (", ", stdout);
	    first_header = 0;

	    printf ("%s: %s",
		    json_quote_str (ctx_quote, name),
		    json_quote_str (ctx_quote, value));
	}
    }

    talloc_free (ctx_quote);
}

static void
format_headers_message_part_json (GMimeMessage *message)
{
    void *ctx = talloc_new (NULL);
    void *ctx_quote = talloc_new (ctx);
    InternetAddressList *recipients;
    const char *recipients_string;

    printf ("%s: %s",
	    json_quote_str (ctx_quote, "From"),
	    json_quote_str (ctx_quote, g_mime_message_get_sender (message)));
    recipients = g_mime_message_get_recipients (message, GMIME_RECIPIENT_TYPE_TO);
    recipients_string = internet_address_list_to_string (recipients, 0);
    if (recipients_string)
	printf (", %s: %s",
		json_quote_str (ctx_quote, "To"),
		json_quote_str (ctx_quote, recipients_string));
    recipients = g_mime_message_get_recipients (message, GMIME_RECIPIENT_TYPE_CC);
    recipients_string = internet_address_list_to_string (recipients, 0);
    if (recipients_string)
	printf (", %s: %s",
		json_quote_str (ctx_quote, "Cc"),
		json_quote_str (ctx_quote, recipients_string));
    printf (", %s: %s",
	    json_quote_str (ctx_quote, "Subject"),
	    json_quote_str (ctx_quote, g_mime_message_get_subject (message)));
    printf (", %s: %s",
	    json_quote_str (ctx_quote, "Date"),
	    json_quote_str (ctx_quote, g_mime_message_get_date_as_string (message)));

    talloc_free (ctx_quote);
    talloc_free (ctx);
}

/* Write a MIME text part out to the given stream.
 *
 * Both line-ending conversion (CRLF->LF) and charset conversion ( ->
 * UTF-8) will be performed, so it is inappropriate to call this
 * function with a non-text part. Doing so will trigger an internal
 * error.
 */
static void
show_text_part_content (GMimeObject *part, GMimeStream *stream_out)
{
    GMimeContentType *content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));
    GMimeStream *stream_filter = NULL;
    GMimeDataWrapper *wrapper;
    const char *charset;

    if (! g_mime_content_type_is_type (content_type, "text", "*"))
	INTERNAL_ERROR ("Illegal request to format non-text part (%s) as text.",
			g_mime_content_type_to_string (content_type));

    if (stream_out == NULL)
	return;

    stream_filter = g_mime_stream_filter_new (stream_out);
    g_mime_stream_filter_add(GMIME_STREAM_FILTER (stream_filter),
			     g_mime_filter_crlf_new (FALSE, FALSE));

    charset = g_mime_object_get_content_type_parameter (part, "charset");
    if (charset) {
	GMimeFilter *charset_filter;
	charset_filter = g_mime_filter_charset_new (charset, "UTF-8");
	/* This result can be NULL for things like "unknown-8bit".
	 * Don't set a NULL filter as that makes GMime print
	 * annoying assertion-failure messages on stderr. */
	if (charset_filter) {
	    g_mime_stream_filter_add (GMIME_STREAM_FILTER (stream_filter),
				      charset_filter);
	    g_object_unref (charset_filter);
	}

    }

    wrapper = g_mime_part_get_content_object (GMIME_PART (part));
    if (wrapper && stream_filter)
	g_mime_data_wrapper_write_to_stream (wrapper, stream_filter);
    if (stream_filter)
	g_object_unref(stream_filter);
}

static const char*
signer_status_to_string (GMimeSignerStatus x)
{
    switch (x) {
    case GMIME_SIGNER_STATUS_NONE:
	return "none";
    case GMIME_SIGNER_STATUS_GOOD:
	return "good";
    case GMIME_SIGNER_STATUS_BAD:
	return "bad";
    case GMIME_SIGNER_STATUS_ERROR:
	return "error";
    }
    return "unknown";
}

static void
format_part_start_text (GMimeObject *part, int *part_count)
{
    GMimeContentDisposition *disposition = g_mime_object_get_content_disposition (part);

    if (disposition &&
	strcmp (disposition->disposition, GMIME_DISPOSITION_ATTACHMENT) == 0)
    {
	printf ("\fattachment{ ID: %d", *part_count);

    } else {

	printf ("\fpart{ ID: %d", *part_count);
    }
}

static void
format_part_content_text (GMimeObject *part)
{
    const char *cid = g_mime_object_get_content_id (part);
    GMimeContentType *content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));

    if (GMIME_IS_PART (part))
    {
	const char *filename = g_mime_part_get_filename (GMIME_PART (part));
	if (filename)
	    printf (", Filename: %s", filename);
    }

    if (cid)
	printf (", Content-id: %s", cid);

    printf (", Content-type: %s\n", g_mime_content_type_to_string (content_type));

    if (g_mime_content_type_is_type (content_type, "text", "*") &&
	!g_mime_content_type_is_type (content_type, "text", "html"))
    {
	GMimeStream *stream_stdout = g_mime_stream_file_new (stdout);
	g_mime_stream_file_set_owner (GMIME_STREAM_FILE (stream_stdout), FALSE);
	show_text_part_content (part, stream_stdout);
	g_object_unref(stream_stdout);
    }
    else if (g_mime_content_type_is_type (content_type, "multipart", "*") ||
	     g_mime_content_type_is_type (content_type, "message", "rfc822"))
    {
	/* Do nothing for multipart since its content will be printed
	 * when recursing. */
    }
    else
    {
	printf ("Non-text part: %s\n",
		g_mime_content_type_to_string (content_type));
    }
}

static void
format_part_end_text (GMimeObject *part)
{
    GMimeContentDisposition *disposition;

    disposition = g_mime_object_get_content_disposition (part);
    if (disposition &&
	strcmp (disposition->disposition, GMIME_DISPOSITION_ATTACHMENT) == 0)
    {
	printf ("\fattachment}\n");
    }
    else
    {
	printf ("\fpart}\n");
    }
}

static void
format_part_start_json (unused (GMimeObject *part), int *part_count)
{
    printf ("{\"id\": %d", *part_count);
}

static void
format_part_encstatus_json (int status)
{
    printf (", \"encstatus\": [{\"status\": ");
    if (status) {
	printf ("\"good\"");
    } else {
	printf ("\"bad\"");
    }
    printf ("}]");
}

static void
format_part_sigstatus_json (const GMimeSignatureValidity* validity)
{
    printf (", \"sigstatus\": [");

    if (!validity) {
	printf ("]");
	return;
    }

    const GMimeSigner *signer = g_mime_signature_validity_get_signers (validity);
    int first = 1;
    void *ctx_quote = talloc_new (NULL);

    while (signer) {
	if (first)
	    first = 0;
	else
	    printf (", ");

	printf ("{");

	/* status */
	printf ("\"status\": %s",
		json_quote_str (ctx_quote,
				signer_status_to_string (signer->status)));

	if (signer->status == GMIME_SIGNER_STATUS_GOOD)
	{
	    if (signer->fingerprint)
		printf (", \"fingerprint\": %s", json_quote_str (ctx_quote, signer->fingerprint));
	    /* these dates are seconds since the epoch; should we
	     * provide a more human-readable format string? */
	    if (signer->created)
		printf (", \"created\": %d", (int) signer->created);
	    if (signer->expires)
		printf (", \"expires\": %d", (int) signer->expires);
	    /* output user id only if validity is FULL or ULTIMATE. */
	    /* note that gmime is using the term "trust" here, which
	     * is WRONG.  It's actually user id "validity". */
	    if ((signer->name) && (signer->trust)) {
		if ((signer->trust == GMIME_SIGNER_TRUST_FULLY) || (signer->trust == GMIME_SIGNER_TRUST_ULTIMATE))
		    printf (", \"userid\": %s", json_quote_str (ctx_quote, signer->name));
           }
       } else {
           if (signer->keyid)
               printf (", \"keyid\": %s", json_quote_str (ctx_quote, signer->keyid));
       }
       if (signer->errors != GMIME_SIGNER_ERROR_NONE) {
           printf (", \"errors\": %x", signer->errors);
       }

       printf ("}");
       signer = signer->next;
    }

    printf ("]");

    talloc_free (ctx_quote);
}

static void
format_part_content_json (GMimeObject *part)
{
    GMimeContentType *content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));
    GMimeStream *stream_memory = g_mime_stream_mem_new ();
    const char *cid = g_mime_object_get_content_id (part);
    void *ctx = talloc_new (NULL);
    GByteArray *part_content;

    printf (", \"content-type\": %s",
	    json_quote_str (ctx, g_mime_content_type_to_string (content_type)));

    if (cid != NULL)
	    printf(", \"content-id\": %s", json_quote_str (ctx, cid));

    if (GMIME_IS_PART (part))
    {
	const char *filename = g_mime_part_get_filename (GMIME_PART (part));
	if (filename)
	    printf (", \"filename\": %s", json_quote_str (ctx, filename));
    }

    if (g_mime_content_type_is_type (content_type, "text", "*") &&
	!g_mime_content_type_is_type (content_type, "text", "html"))
    {
	show_text_part_content (part, stream_memory);
	part_content = g_mime_stream_mem_get_byte_array (GMIME_STREAM_MEM (stream_memory));

	printf (", \"content\": %s", json_quote_chararray (ctx, (char *) part_content->data, part_content->len));
    }
    else if (g_mime_content_type_is_type (content_type, "multipart", "*"))
    {
	printf (", \"content\": [");
    }
    else if (g_mime_content_type_is_type (content_type, "message", "rfc822"))
    {
	printf (", \"content\": [{");
    }

    talloc_free (ctx);
    if (stream_memory)
	g_object_unref (stream_memory);
}

static void
format_part_end_json (GMimeObject *part)
{
    GMimeContentType *content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));

    if (g_mime_content_type_is_type (content_type, "multipart", "*"))
	printf ("]");
    else if (g_mime_content_type_is_type (content_type, "message", "rfc822"))
	printf ("}]");

    printf ("}");
}

static void
format_part_content_raw (GMimeObject *part)
{
    if (! GMIME_IS_PART (part))
	return;

    GMimeStream *stream_stdout;
    GMimeStream *stream_filter = NULL;
    GMimeDataWrapper *wrapper;

    stream_stdout = g_mime_stream_file_new (stdout);
    g_mime_stream_file_set_owner (GMIME_STREAM_FILE (stream_stdout), FALSE);

    stream_filter = g_mime_stream_filter_new (stream_stdout);

    wrapper = g_mime_part_get_content_object (GMIME_PART (part));

    if (wrapper && stream_filter)
	g_mime_data_wrapper_write_to_stream (wrapper, stream_filter);

    if (stream_filter)
	g_object_unref (stream_filter);

    if (stream_stdout)
	g_object_unref(stream_stdout);
}

static void
show_message (void *ctx,
	      const notmuch_show_format_t *format,
	      notmuch_message_t *message,
	      int indent,
	      notmuch_show_params_t *params)
{
    if (params->part <= 0) {
	fputs (format->message_start, stdout);
	if (format->message)
	    format->message(ctx, message, indent);

	fputs (format->header_start, stdout);
	if (format->header)
	    format->header(ctx, message);
	fputs (format->header_end, stdout);

	fputs (format->body_start, stdout);
    }

    if (format->part_content)
	show_message_body (message, format, params);

    if (params->part <= 0) {
	fputs (format->body_end, stdout);

	fputs (format->message_end, stdout);
    }
}

static void
show_messages (void *ctx,
	       const notmuch_show_format_t *format,
	       notmuch_messages_t *messages,
	       int indent,
	       notmuch_show_params_t *params)
{
    notmuch_message_t *message;
    notmuch_bool_t match;
    int first_set = 1;
    int next_indent;

    fputs (format->message_set_start, stdout);

    for (;
	 notmuch_messages_valid (messages);
	 notmuch_messages_move_to_next (messages))
    {
	if (!first_set)
	    fputs (format->message_set_sep, stdout);
	first_set = 0;

	fputs (format->message_set_start, stdout);

	message = notmuch_messages_get (messages);

	match = notmuch_message_get_flag (message, NOTMUCH_MESSAGE_FLAG_MATCH);

	next_indent = indent;

	if (match || params->entire_thread) {
	    show_message (ctx, format, message, indent, params);
	    next_indent = indent + 1;

	    fputs (format->message_set_sep, stdout);
	}

	show_messages (ctx,
		       format,
		       notmuch_message_get_replies (message),
		       next_indent,
		       params);

	notmuch_message_destroy (message);

	fputs (format->message_set_end, stdout);
    }

    fputs (format->message_set_end, stdout);
}

/* Formatted output of single message */
static int
do_show_single (void *ctx,
		notmuch_query_t *query,
		const notmuch_show_format_t *format,
		notmuch_show_params_t *params)
{
    notmuch_messages_t *messages;
    notmuch_message_t *message;

    if (notmuch_query_count_messages (query) != 1) {
	fprintf (stderr, "Error: search term did not match precisely one message.\n");
	return 1;
    }

    messages = notmuch_query_search_messages (query);
    message = notmuch_messages_get (messages);

    if (message == NULL) {
	fprintf (stderr, "Error: Cannot find matching message.\n");
	return 1;
    }

    notmuch_message_set_flag (message, NOTMUCH_MESSAGE_FLAG_MATCH, 1);

    /* Special case for --format=raw of full single message, just cat out file */
    if (params->raw && 0 == params->part) {

	const char *filename;
	FILE *file;
	size_t size;
	char buf[4096];

	filename = notmuch_message_get_filename (message);
	if (filename == NULL) {
	    fprintf (stderr, "Error: Cannot message filename.\n");
	    return 1;
	}

	file = fopen (filename, "r");
	if (file == NULL) {
	    fprintf (stderr, "Error: Cannot open file %s: %s\n", filename, strerror (errno));
	    return 1;
	}

	while (!feof (file)) {
	    size = fread (buf, 1, sizeof (buf), file);
	    ignore_result(fwrite (buf, size, 1, stdout));
	}

	fclose (file);

    } else {

	show_message (ctx, format, message, 0, params);

    }

    return 0;
}

/* Formatted output of threads */
static int
do_show (void *ctx,
	 notmuch_query_t *query,
	 const notmuch_show_format_t *format,
	 notmuch_show_params_t *params)
{
    notmuch_threads_t *threads;
    notmuch_thread_t *thread;
    notmuch_messages_t *messages;
    int first_toplevel = 1;

    fputs (format->message_set_start, stdout);

    for (threads = notmuch_query_search_threads (query);
	 notmuch_threads_valid (threads);
	 notmuch_threads_move_to_next (threads))
    {
	thread = notmuch_threads_get (threads);

	messages = notmuch_thread_get_toplevel_messages (thread);

	if (messages == NULL)
	    INTERNAL_ERROR ("Thread %s has no toplevel messages.\n",
			    notmuch_thread_get_thread_id (thread));

	if (!first_toplevel)
	    fputs (format->message_set_sep, stdout);
	first_toplevel = 0;

	show_messages (ctx, format, messages, 0, params);

	notmuch_thread_destroy (thread);

    }

    fputs (format->message_set_end, stdout);

    return 0;
}

int
notmuch_show_command (void *ctx, unused (int argc), unused (char *argv[]))
{
    notmuch_config_t *config;
    notmuch_database_t *notmuch;
    notmuch_query_t *query;
    char *query_string;
    char *opt;
    const notmuch_show_format_t *format = &format_text;
    notmuch_show_params_t params;
    int mbox = 0;
    int format_specified = 0;
    int i;

    params.entire_thread = 0;
    params.raw = 0;
    params.part = -1;
    params.cryptoctx = NULL;
    params.decrypt = 0;

    argc--; argv++; /* skip subcommand argument */

    for (i = 0; i < argc && argv[i][0] == '-'; i++) {
	if (strcmp (argv[i], "--") == 0) {
	    i++;
	    break;
	}
	if (STRNCMP_LITERAL (argv[i], "--format=") == 0) {
	    opt = argv[i] + sizeof ("--format=") - 1;
	    if (strcmp (opt, "text") == 0) {
		format = &format_text;
	    } else if (strcmp (opt, "json") == 0) {
		format = &format_json;
		params.entire_thread = 1;
	    } else if (strcmp (opt, "mbox") == 0) {
		format = &format_mbox;
		mbox = 1;
	    } else if (strcmp (opt, "raw") == 0) {
		format = &format_raw;
		params.raw = 1;
	    } else {
		fprintf (stderr, "Invalid value for --format: %s\n", opt);
		return 1;
	    }
	    format_specified = 1;
	} else if (STRNCMP_LITERAL (argv[i], "--part=") == 0) {
	    params.part = atoi(argv[i] + sizeof ("--part=") - 1);
	} else if (STRNCMP_LITERAL (argv[i], "--entire-thread") == 0) {
	    params.entire_thread = 1;
	} else if ((STRNCMP_LITERAL (argv[i], "--verify") == 0) ||
		   (STRNCMP_LITERAL (argv[i], "--decrypt") == 0)) {
	    if (params.cryptoctx == NULL) {
		GMimeSession* session = g_object_new(g_mime_session_get_type(), NULL);
		if (NULL == (params.cryptoctx = g_mime_gpg_context_new(session, "gpg")))
		    fprintf (stderr, "Failed to construct gpg context.\n");
		else
		    g_mime_gpg_context_set_always_trust((GMimeGpgContext*)params.cryptoctx, FALSE);
		g_object_unref (session);
		session = NULL;
	    }
	    if (STRNCMP_LITERAL (argv[i], "--decrypt") == 0)
		params.decrypt = 1;
	} else {
	    fprintf (stderr, "Unrecognized option: %s\n", argv[i]);
	    return 1;
	}
    }

    argc -= i;
    argv += i;

    config = notmuch_config_open (ctx, NULL, NULL);
    if (config == NULL)
	return 1;

    query_string = query_string_from_args (ctx, argc, argv);
    if (query_string == NULL) {
	fprintf (stderr, "Out of memory\n");
	return 1;
    }

    if (mbox && params.part > 0) {
	fprintf (stderr, "Error: specifying parts is incompatible with mbox output format.\n");
	return 1;
    }

    if (*query_string == '\0') {
	fprintf (stderr, "Error: notmuch show requires at least one search term.\n");
	return 1;
    }

    notmuch = notmuch_database_open (notmuch_config_get_database_path (config),
				     NOTMUCH_DATABASE_MODE_READ_ONLY);
    if (notmuch == NULL)
	return 1;

    query = notmuch_query_create (notmuch, query_string);
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	return 1;
    }

    /* if part was requested and format was not specified, use format=raw */
    if (params.part >= 0 && !format_specified)
	format = &format_raw;

    /* If --format=raw specified without specifying part, we can only
     * output single message, so set part=0 */
    if (params.raw && params.part < 0)
	params.part = 0;

    if (params.part >= 0)
	return do_show_single (ctx, query, format, &params);
    else
	return do_show (ctx, query, format, &params);

    notmuch_query_destroy (query);
    notmuch_database_close (notmuch);

    if (params.cryptoctx)
	g_object_unref(params.cryptoctx);

    return 0;
}
