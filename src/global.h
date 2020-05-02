// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2020 Marco Baye
// Have a look at "acme.c" for further info
//
// Global stuff - things that are needed by several modules
// 19 Nov 2014	Merged Johann Klasek's report listing generator patch
// 23 Nov 2014	Merged Martin Piper's "--msvc" error output patch
#ifndef global_H
#define global_H


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "config.h"

#define LOCAL_PREFIX		'.'	// FIXME - this is not yet used consistently!
#define CHEAP_PREFIX		'@'	// prefix character for cheap locals

// Constants

#define SF_FOUND_BLANK		(1u << 0)	// statement had space or tab
#define SF_IMPLIED_LABEL	(1u << 1)	// statement had implied label def
extern const char	s_and[];
extern const char	s_asl[];
extern const char	s_asr[];
extern const char	s_bra[];
extern const char	s_brl[];
extern const char	s_cbm[];
extern const char	s_eor[];
extern const char	s_error[];
extern const char	s_lsr[];
extern const char	s_scrxor[];
extern char		s_untitled[];
extern const char	s_Zone[];
#define s_zone	(s_subzone + 3)	// Yes, I know I'm sick
extern const char	s_subzone[];
extern const char	s_pet[];
extern const char	s_raw[];
extern const char	s_scr[];
// error messages during assembly
extern const char	exception_cannot_open_input_file[];
extern const char	exception_missing_string[];
extern const char	exception_negative_size[];
extern const char	exception_no_left_brace[];
extern const char	exception_no_memory_left[];
extern const char	exception_no_right_brace[];
//extern const char	exception_not_yet[];
extern const char	exception_number_out_of_range[];
extern const char	exception_pc_undefined[];
extern const char	exception_syntax[];
// byte flags table
extern const char	global_byte_flags[];
#define BYTE_STARTS_KEYWORD(b)		(global_byte_flags[(unsigned char) b] & (1u << 7))	// byte is allowed at start of keyword (a-z, A-Z, _, everything>127)
#define BYTE_CONTINUES_KEYWORD(b)	(global_byte_flags[(unsigned char) b] & (1u << 6))	// byte is allowed in a keyword (as above, plus digits)
//#define BYTE_TO_LOWER_CASE(b)	bit 5 means: "byte is upper case, and can be converted to lower case by ORing this bit" - but this is not used at the moment!
#define BYTE_IS_SYNTAX_CHAR(b)		(global_byte_flags[(unsigned char) b] & (1u << 4))	// special character for input syntax
#define BYTE_FOLLOWS_ANON(b)		(global_byte_flags[(unsigned char) b] & (1u << 3))	// preceding '-' are backward label
// bits 2, 1 and 0 are currently unused

// TODO - put in runtime struct:
extern char	GotByte;	// Last byte read (processed)
// configuration
struct config {
	char		pseudoop_prefix;	// '!' or '.'
	int		process_verbosity;	// level of additional output
	boolean		warn_on_indented_labels;	// warn if indented label is encountered
	boolean		warn_on_old_for;	// warn if "!for" with old syntax is found
	boolean		warn_on_type_mismatch;	// use type-checking system
	signed long	max_errors;	// errors before giving up
	boolean		format_msvc;		// enabled by --msvc
	boolean		format_color;		// enabled by --color
	FILE		*msg_stream;		// defaults to stderr, changed to stdout by --use-stdout
	boolean		honor_leading_zeroes;	// TRUE, disabled by --ignore-zeroes
	boolean		segment_warning_is_error;	// FALSE, enabled by --strict-segments
	boolean		test_new_features;	// FALSE, enabled by --test
};
extern struct config	config;

struct pass {
	int	number;	// counts up from zero
	int	undefined_count;	// counts undefined expression results (if this stops decreasing, next pass must list them as errors)
	//int	needvalue_count;	// counts undefined expression results actually needed for output (when this hits zero, we're done)	FIXME - use
	int	error_count;
};
extern struct pass	pass;
#define FIRST_PASS	(pass.number == 0)

// report stuff
#define REPORT_ASCBUFSIZE	1024
#define REPORT_BINBUFSIZE	9	// eight are shown, then "..."
struct report {
	FILE		*fd;		// report file descriptor (NULL => no report)
	struct input	*last_input;
	size_t		asc_used;
	size_t		bin_used;
	int		bin_address;	// address at start of bin_buf[]
	char		asc_buf[REPORT_ASCBUFSIZE];	// source bytes
	char		bin_buf[REPORT_BINBUFSIZE];	// output bytes
};
extern struct report	*report;	// TODO - put in "part" struct

// Macros for skipping a single space character
#define SKIPSPACE()		\
do {				\
	if (GotByte == ' ')	\
		GetByte();	\
} while (0)
#define NEXTANDSKIPSPACE()	\
do {				\
	if (GetByte() == ' ')	\
		GetByte();	\
} while (0)


// Prototypes

// set configuration to default values
extern void config_default(struct config *conf);
// allocate memory and die if not available
extern void *safe_malloc(size_t);
// Parse block, beginning with next byte.
// End reason (either CHAR_EOB or CHAR_EOF) can be found in GotByte afterwards
// Has to be re-entrant.
extern void Parse_until_eob_or_eof(void);
// Skip space. If GotByte is CHAR_SOB ('{'), parse block and return TRUE.
// Otherwise (if there is no block), return FALSE.
// Don't forget to call EnsureEOL() afterwards.
extern int Parse_optional_block(void);
// error/warning counter so macro calls can find out whether to show a call stack
extern int Throw_get_counter(void);
// Output a warning.
// This means the produced code looks as expected. But there has been a
// situation that should be reported to the user, for example ACME may have
// assembled a 16-bit parameter with an 8-bit value.
extern void Throw_warning(const char *);
// Output a warning if in first pass. See above.
extern void Throw_first_pass_warning(const char *);
// Output an error.
// This means something went wrong in a way that implies that the output
// almost for sure won't look like expected, for example when there was a
// syntax error. The assembler will try to go on with the assembly though, so
// the user gets to know about more than one of his typos at a time.
extern void Throw_error(const char *);
// Output a serious error, stopping assembly.
// Serious errors are those that make it impossible to go on with the
// assembly. Example: "!fill" without a parameter - the program counter cannot
// be set correctly in this case, so proceeding would be of no use at all.
extern void Throw_serious_error(const char *);
// handle bugs
extern void Bug_found(const char *, int);


#endif
