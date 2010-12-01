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

typedef struct show_format {
    const char *message_set_start;
    const char *message_start;
    void (*message) (const void *ctx,
		     notmuch_message_t *message,
		     int indent);
    const char *header_start;
    void (*header) (const void *ctx,
		    notmuch_message_t *message);
    const char *header_end;
    const char *body_start;
    void (*part) (GMimeObject *part,
		  int *part_count,
		  gboolean first,
		  notmuch_show_params_t *params);
    const char *body_end;
    const char *message_end;
    const char *message_set_sep;
    const char *message_set_end;
} show_format_t;

static void
format_message_text (unused (const void *ctx),
		     notmuch_message_t *message,
		     int indent);
static void
format_headers_text (const void *ctx,
		     notmuch_message_t *message);
static void
format_part_text (GMimeObject *part,
		  int *part_count,
		  gboolean first,
		  notmuch_show_params_t *params);
static const show_format_t format_text = {
    "",
	"\fmessage{ ", format_message_text,
	    "\fheader{\n", format_headers_text, "\fheader}\n",
	    "\fbody{\n", format_part_text, "\fbody}\n",
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
format_part_json (GMimeObject *part,
		  int *part_count,
		  gboolean first,
		  notmuch_show_params_t *params);
static const show_format_t format_json = {
    "[",
	"{", format_message_json,
	    ", \"headers\": {", format_headers_json, "}\n",
	    ", \"body\": [", format_part_json, "]\n",
	"}\n", ", ",
    "]\n"
};

static void
format_message_mbox (const void *ctx,
		     notmuch_message_t *message,
		     unused (int indent));

static const show_format_t format_mbox = {
    "",
        "", format_message_mbox,
            "", NULL, "",
            "", NULL, "",
        "", "",
    ""
};

static void
format_message_part (const void *ctx,
		     notmuch_message_t *message,
		     unused (int indent));
static void
format_headers_part (const void *ctx,
		     notmuch_message_t *message);
static void
format_part_part (GMimeObject *part,
		  int *part_count,
		  gboolean first,
		  notmuch_show_params_t *params);
static const show_format_t format_part = {
    "",
	"", format_message_part,
	    "", format_headers_part, "",
	    "", format_part_part, "",
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
    printf("]");
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
    /* XXX: How to free addresses here? */
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
format_message_part (unused (const void *ctx),
		     unused (notmuch_message_t *message),
		     unused (int indent))
{
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
format_headers_part (unused (const void *ctx),
		     unused (notmuch_message_t *message))
{
}

static void
show_part_content (GMimeObject *part, GMimeStream *stream_out)
{
    GMimeStream *stream_filter = NULL;
    GMimeDataWrapper *wrapper;
    const char *charset;

    charset = g_mime_object_get_content_type_parameter (part, "charset");

    if (stream_out) {
	stream_filter = g_mime_stream_filter_new (stream_out);
	g_mime_stream_filter_add(GMIME_STREAM_FILTER (stream_filter),
				 g_mime_filter_crlf_new (FALSE, FALSE));
        if (charset) {
	    GMimeFilter *charset_filter;
	    charset_filter = g_mime_filter_charset_new (charset, "UTF-8");
	    /* This result can be NULL for things like "unknown-8bit".
	     * Don't set a NULL filter as that makes GMime print
	     * annoying assertion-failure messages on stderr. */
	    if (charset_filter)
		g_mime_stream_filter_add (GMIME_STREAM_FILTER (stream_filter),
					  charset_filter);
	}
    }

    wrapper = g_mime_part_get_content_object (GMIME_PART (part));
    if (wrapper && stream_filter)
	g_mime_data_wrapper_write_to_stream (wrapper, stream_filter);
    if (stream_filter)
	g_object_unref(stream_filter);
}

static void
format_part_text (GMimeObject *part,
		  int *part_count,
		  gboolean first,
		  unused (notmuch_show_params_t *params))
{
    GMimeContentDisposition *disposition;
    GMimeContentType *content_type;

    /* Avoid compiler complaints about unused arguments. */
    (void) first;

    if (GMIME_IS_MULTIPART (part)) {
	GMimeMultipart *multipart = GMIME_MULTIPART (part);
	int i;

	for (i = 0; i < g_mime_multipart_get_count (multipart); i++) {
		*part_count += 1;

		format_part_text (g_mime_multipart_get_part (multipart, i),
				  part_count, i == 0, params);
	}

	return;
    }

    if (GMIME_IS_MESSAGE_PART (part)) {
	GMimeMessage *mime_message;
	const char *value;
	InternetAddressList *addresses;

	mime_message = g_mime_message_part_get_message (GMIME_MESSAGE_PART (part));

	/* Insert the headers of the enclosed message. */
	fputs (format_text.header_start, stdout);

	value = g_mime_message_get_sender(mime_message);
	printf ("From: %s\n", value);
	value = g_mime_message_get_subject(mime_message);
	printf ("Subject: %s\n", value);
	addresses = g_mime_message_get_recipients(mime_message, GMIME_RECIPIENT_TYPE_TO);
	printf ("To: %s\n", internet_address_list_to_string (addresses, FALSE));
	addresses = g_mime_message_get_recipients(mime_message, GMIME_RECIPIENT_TYPE_CC);
	printf ("Cc: %s\n", internet_address_list_to_string (addresses, FALSE));
	value = g_mime_message_get_date_as_string(mime_message);
	printf ("Date: %s\n", value);

	fputs (format_text.header_end, stdout);

	format_part_text (g_mime_message_get_mime_part (mime_message),
			  part_count, TRUE, params);

	return;
    }

    disposition = g_mime_object_get_content_disposition (part);
    if (disposition &&
	strcmp (disposition->disposition, GMIME_DISPOSITION_ATTACHMENT) == 0)
    {
	const char *filename = g_mime_part_get_filename (GMIME_PART (part));
	content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));

	printf ("\fattachment{ ID: %d, Content-type: %s\n",
		*part_count,
		g_mime_content_type_to_string (content_type));
	printf ("Attachment: %s (%s)\n", filename,
		g_mime_content_type_to_string (content_type));

	if (g_mime_content_type_is_type (content_type, "text", "*") &&
	    !g_mime_content_type_is_type (content_type, "text", "html"))
	{
	    GMimeStream *stream_stdout = g_mime_stream_file_new (stdout);
	    g_mime_stream_file_set_owner (GMIME_STREAM_FILE (stream_stdout), FALSE);
	    show_part_content (part, stream_stdout);
	    g_object_unref(stream_stdout);
	}

	printf ("\fattachment}\n");

	return;
    }

    content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));

    printf ("\fpart{ ID: %d, Content-type: %s\n",
	    *part_count,
	    g_mime_content_type_to_string (content_type));

    if (g_mime_content_type_is_type (content_type, "text", "*") &&
	!g_mime_content_type_is_type (content_type, "text", "html"))
    {
	GMimeStream *stream_stdout = g_mime_stream_file_new (stdout);
	g_mime_stream_file_set_owner (GMIME_STREAM_FILE (stream_stdout), FALSE);
	show_part_content (part, stream_stdout);
	g_object_unref(stream_stdout);
    }
    else
    {
	printf ("Non-text part: %s\n",
		g_mime_content_type_to_string (content_type));
    }

    printf ("\fpart}\n");
}

static void
format_part_json (GMimeObject *part,
		  int *part_count,
		  gboolean first,
		  unused (notmuch_show_params_t *params))

{
    GMimeContentType *content_type;
    GMimeContentDisposition *disposition;
    void *ctx = talloc_new (NULL);
    GMimeStream *stream_memory = g_mime_stream_mem_new ();
    GByteArray *part_content;
    const char *cid;

    *part_count += 1;

    content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));

    if (!first)
	fputs (",\n", stdout);

    printf ("{\"id\": %d, \"content-type\": %s",
	    *part_count,
	    json_quote_str (ctx, g_mime_content_type_to_string (content_type)));

    cid = g_mime_object_get_content_id (part);
    if (cid != NULL)
	    printf(", \"content-id\": %s",
		   json_quote_str (ctx, cid));

    if (GMIME_IS_MULTIPART (part)) {
	GMimeMultipart *multipart = GMIME_MULTIPART (part);
	int i;

	printf (", \"content\": [\n");

	for (i = 0; i < g_mime_multipart_get_count (multipart); i++) {
		*part_count += 1;

		format_part_json (g_mime_multipart_get_part (multipart, i),
				  part_count, i == 0, params);
	}

	printf ("]}\n");

	return;
    }

    if (GMIME_IS_MESSAGE_PART (part)) {
	GMimeMessage *mime_message;
	void *ctx_quote = talloc_new (ctx);
	const char *value;
	InternetAddressList *addresses;

	*part_count += 1;
	mime_message = g_mime_message_part_get_message (GMIME_MESSAGE_PART (part));

	/* Insert the headers of the enclosed message. */
	printf (", \"headers\": {");

	value = g_mime_message_get_sender(mime_message);
	printf ("\"From\": %s,", json_quote_str (ctx_quote, value));
	value = g_mime_message_get_subject(mime_message);
	printf ("\"Subject\": %s,", json_quote_str (ctx_quote, value));
	addresses = g_mime_message_get_recipients(mime_message, GMIME_RECIPIENT_TYPE_TO);
	printf ("\"To\": %s,", json_quote_str (ctx_quote, internet_address_list_to_string (addresses, FALSE)));
	addresses = g_mime_message_get_recipients(mime_message, GMIME_RECIPIENT_TYPE_CC);
	printf ("\"Cc\": %s,", json_quote_str (ctx_quote, internet_address_list_to_string (addresses, FALSE)));
	value = g_mime_message_get_date_as_string(mime_message);
	printf ("\"Date\": %s", json_quote_str (ctx_quote, value));

	talloc_free (ctx_quote);

	printf ("}\n");

	printf (", \"content\": \n");

	format_part_json (g_mime_message_get_mime_part (mime_message),
			  part_count, TRUE, params);

	printf ("}\n");

	return;
    }

    disposition = g_mime_object_get_content_disposition (part);
    if (disposition &&
	strcmp (disposition->disposition, GMIME_DISPOSITION_ATTACHMENT) == 0)
    {
	const char *filename = g_mime_part_get_filename (GMIME_PART (part));

	printf (", \"filename\": %s", json_quote_str (ctx, filename));
    }

    if (g_mime_content_type_is_type (content_type, "text", "*") &&
	!g_mime_content_type_is_type (content_type, "text", "html"))
    {
	show_part_content (part, stream_memory);
	part_content = g_mime_stream_mem_get_byte_array (GMIME_STREAM_MEM (stream_memory));

	printf (", \"content\": %s", json_quote_chararray (ctx, (char *) part_content->data, part_content->len));
    }

    fputs ("}", stdout);

    talloc_free (ctx);
    if (stream_memory)
	g_object_unref (stream_memory);
}

static void
format_part_part (GMimeObject *part,
		  int *part_count,
		  unused (gboolean first),
		  notmuch_show_params_t *params)
{
    GMimeStream *stream_filter = NULL;
    GMimeDataWrapper *wrapper;
    GMimeStream *stream_stdout = g_mime_stream_file_new (stdout);

    *part_count += 1;

    if (GMIME_IS_MULTIPART (part)) {
	GMimeMultipart *multipart = GMIME_MULTIPART (part);
	int i;

	for (i = 0; i < g_mime_multipart_get_count (multipart); i++) {
		*part_count += 1;

		format_part_part (g_mime_multipart_get_part (multipart, i),
				  part_count, i == 0, params);
	}

	return;
    }

    if (GMIME_IS_MESSAGE_PART (part)) {
	GMimeMessage *mime_message;

	mime_message = g_mime_message_part_get_message (GMIME_MESSAGE_PART (part));

	format_part_part (g_mime_message_get_mime_part (mime_message),
			  part_count, TRUE, params);
	return;
    }

    if (*part_count == params->part) {
	    stream_filter = g_mime_stream_filter_new(stream_stdout);
	    wrapper = g_mime_part_get_content_object (GMIME_PART (part));
	    if (wrapper && stream_filter)
		    g_mime_data_wrapper_write_to_stream (wrapper, stream_filter);
	    if (stream_filter)
		    g_object_unref(stream_filter);
    }

}

static void
show_message (void *ctx,
	      const show_format_t *format,
	      notmuch_message_t *message,
	      int indent,
	      notmuch_show_params_t *params)
{
    fputs (format->message_start, stdout);
    if (format->message)
	format->message(ctx, message, indent);

    fputs (format->header_start, stdout);
    if (format->header)
	format->header(ctx, message);
    fputs (format->header_end, stdout);

    fputs (format->body_start, stdout);
    if (format->part)
	show_message_body (notmuch_message_get_filename (message), format->part,
			   params);
    fputs (format->body_end, stdout);

    fputs (format->message_end, stdout);
}


static void
show_messages (void *ctx, const show_format_t *format, notmuch_messages_t *messages, int indent,
	       notmuch_bool_t entire_thread, notmuch_show_params_t *params)
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

	if (match || entire_thread) {
	    show_message (ctx, format, message, indent, params);
	    next_indent = indent + 1;

	    fputs (format->message_set_sep, stdout);
	}

	show_messages (ctx, format, notmuch_message_get_replies (message),
		       next_indent, entire_thread, params);

	notmuch_message_destroy (message);

	fputs (format->message_set_end, stdout);
    }

    fputs (format->message_set_end, stdout);
}

/* Support for --format=raw */
static int
do_show_raw (unused(void *ctx), notmuch_query_t *query)
{
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    const char *filename;
    FILE *file;
    size_t size;
    char buf[4096];

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
	fwrite (buf, size, 1, stdout);
    }

    fclose (file);

    return 0;
}

/* Support for --format=text|json|mbox */
static int
do_show (void *ctx,
	 notmuch_query_t *query,
	 const show_format_t *format,
	 int entire_thread,
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

	show_messages (ctx, format, messages, 0, entire_thread, params);

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
    const show_format_t *format = &format_text;
    int entire_thread = 0;
    int one_message = 0;
    notmuch_show_params_t params;
    int i;
    int raw = 0;

    params.part = 0;

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
		entire_thread = 1;
	    } else if (strcmp (opt, "mbox") == 0) {
		format = &format_mbox;
	    } else if (strcmp (opt, "raw") == 0) {
		raw = 1;
	    } else if (strcmp (opt, "part") == 0) {
		format = &format_part;
		one_message = 1;
	    } else {
		fprintf (stderr, "Invalid value for --format: %s\n", opt);
		return 1;
	    }
	} else if (STRNCMP_LITERAL (argv[i], "--entire-thread") == 0) {
	    entire_thread = 1;
	} else if (STRNCMP_LITERAL (argv[i], "--part=") == 0) {
	    params.part = atoi(argv[i] + sizeof ("--part=") - 1);
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

    if (raw)
	return do_show_raw (ctx, query);
    else
	return do_show (ctx, query, format, entire_thread, &params);

    notmuch_query_destroy (query);
    notmuch_database_close (notmuch);

    return 0;
}
