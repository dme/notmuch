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

static volatile sig_atomic_t interrupted;

static void
handle_sigint (unused (int sig))
{
    static char msg[] = "Stopping...         \n";
    ignore_result(write(STDERR_FILENO, msg, sizeof(msg)-1));
    interrupted = 1;
}

static char *
_escape_tag (char *buf, const char *tag)
{
    const char *in = tag;
    char *out = buf;
    /* Boolean terms surrounded by double quotes can contain any
     * character.  Double quotes are quoted by doubling them. */
    *out++ = '"';
    while (*in) {
	if (*in == '"')
	    *out++ = '"';
	*out++ = *in++;
    }
    *out++ = '"';
    *out = 0;
    return buf;
}

static char *
_optimize_tag_query (void *ctx, const char *orig_query_string, char *argv[],
		     int *add_tags, int add_tags_count,
		     int *remove_tags, int remove_tags_count)
{
    /* This is subtler than it looks.  Xapian ignores the '-' operator
     * at the beginning both queries and parenthesized groups and,
     * furthermore, the presence of a '-' operator at the beginning of
     * a group can inhibit parsing of the previous operator.  Hence,
     * the user-provided query MUST appear first, but it is safe to
     * parenthesize and the exclusion part of the query must not use
     * the '-' operator (though the NOT operator is fine). */

    char *escaped, *query_string;
    const char *join = "";
    int i;
    unsigned int max_tag_len = 0;

    /* Allocate a buffer for escaping tags.  This is large enough to
     * hold a fully escaped tag with every character doubled plus
     * enclosing quotes and a NUL. */
    for (i = 0; i < add_tags_count; i++)
	if (strlen (argv[add_tags[i]] + 1) > max_tag_len)
	    max_tag_len = strlen (argv[add_tags[i]] + 1);
    for (i = 0; i < remove_tags_count; i++)
	if (strlen (argv[remove_tags[i]] + 1) > max_tag_len)
	    max_tag_len = strlen (argv[remove_tags[i]] + 1);
    escaped = talloc_array(ctx, char, max_tag_len * 2 + 3);
    if (!escaped)
	return NULL;

    /* Build the new query string */
    if (strcmp (orig_query_string, "*") == 0)
	query_string = talloc_strdup (ctx, "(");
    else
	query_string = talloc_asprintf (ctx, "( %s ) and (", orig_query_string);

    for (i = 0; i < add_tags_count && query_string; i++) {
	query_string = talloc_asprintf_append_buffer (
	    query_string, "%snot tag:%s", join,
	    _escape_tag (escaped, argv[add_tags[i]] + 1));
	join = " or ";
    }
    for (i = 0; i < remove_tags_count && query_string; i++) {
	query_string = talloc_asprintf_append_buffer (
	    query_string, "%stag:%s", join,
	    _escape_tag (escaped, argv[remove_tags[i]] + 1));
	join = " or ";
    }

    if (query_string)
	query_string = talloc_strdup_append_buffer (query_string, ")");

    talloc_free (escaped);
    return query_string;
}

int
notmuch_tag_command (void *ctx, unused (int argc), unused (char *argv[]))
{
    int *add_tags, *remove_tags;
    int add_tags_count = 0;
    int remove_tags_count = 0;
    char *query_string;
    notmuch_config_t *config;
    notmuch_database_t *notmuch;
    notmuch_query_t *query;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    struct sigaction action;
    notmuch_bool_t synchronize_flags;
    int i;

    /* Setup our handler for SIGINT */
    memset (&action, 0, sizeof (struct sigaction));
    action.sa_handler = handle_sigint;
    sigemptyset (&action.sa_mask);
    action.sa_flags = SA_RESTART;
    sigaction (SIGINT, &action, NULL);

    add_tags = talloc_size (ctx, argc * sizeof (int));
    if (add_tags == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }

    remove_tags = talloc_size (ctx, argc * sizeof (int));
    if (remove_tags == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }

    argc--; argv++; /* skip subcommand argument */

    for (i = 0; i < argc; i++) {
	if (strcmp (argv[i], "--") == 0) {
	    i++;
	    break;
	}
	if (argv[i][0] == '+') {
	    add_tags[add_tags_count++] = i;
	} else if (argv[i][0] == '-') {
	    remove_tags[remove_tags_count++] = i;
	} else {
	    break;
	}
    }

    if (add_tags_count == 0 && remove_tags_count == 0) {
	fprintf (stderr, "Error: 'notmuch tag' requires at least one tag to add or remove.\n");
	return 1;
    }

    query_string = query_string_from_args (ctx, argc - i, &argv[i]);

    if (*query_string == '\0') {
	fprintf (stderr, "Error: notmuch tag requires at least one search term.\n");
	return 1;
    }

    /* Optimize the query so it excludes messages that already have
     * the specified set of tags. */
    query_string = _optimize_tag_query (ctx, query_string, argv,
					add_tags, add_tags_count,
					remove_tags, remove_tags_count);
    if (query_string == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }

    config = notmuch_config_open (ctx, NULL, NULL);
    if (config == NULL)
	return 1;

    notmuch = notmuch_database_open (notmuch_config_get_database_path (config),
				     NOTMUCH_DATABASE_MODE_READ_WRITE);
    if (notmuch == NULL)
	return 1;

    synchronize_flags = notmuch_config_get_maildir_synchronize_flags (config);

    query = notmuch_query_create (notmuch, query_string);
    if (query == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }

    /* tagging is not interested in any special sort order */
    notmuch_query_set_sort (query, NOTMUCH_SORT_UNSORTED);

    for (messages = notmuch_query_search_messages (query);
	 notmuch_messages_valid (messages) && !interrupted;
	 notmuch_messages_move_to_next (messages))
    {
	message = notmuch_messages_get (messages);

	notmuch_message_freeze (message);

	for (i = 0; i < remove_tags_count; i++)
	    notmuch_message_remove_tag (message,
					argv[remove_tags[i]] + 1);

	for (i = 0; i < add_tags_count; i++)
	    notmuch_message_add_tag (message, argv[add_tags[i]] + 1);

	notmuch_message_thaw (message);

	if (synchronize_flags)
	    notmuch_message_tags_to_maildir_flags (message);

	notmuch_message_destroy (message);
    }

    notmuch_query_destroy (query);
    notmuch_database_close (notmuch);

    return interrupted;
}
