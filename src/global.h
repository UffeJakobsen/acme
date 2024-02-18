// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2024 Marco Baye
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

#define LOCAL_PREFIX	'.'	// FIXME - this is not yet used consistently!
#define CHEAP_PREFIX	'@'	// prefix character for cheap locals

// Constants

extern char		s_untitled[];
// error messages during assembly
extern const char	exception_missing_string[];
extern const char	exception_negative_size[];
extern const char	exception_no_left_brace[];
extern const char	exception_no_memory_left[];
extern const char	exception_no_right_brace[];
//extern const char	exception_not_yet[];
extern const char	exception_number_out_of_range[];
extern const char	exception_number_out_of_8b_range[];
extern const char	exception_pc_undefined[];
extern const char	exception_symbol_defined[];
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

enum version {
	VER_OLDEST_SUPPORTED,		// v0.85 looks like the oldest version it makes sense to actually support
	VER_DEPRECATE_REALPC,		// v0.86 made !pseudopc/!realpc give a warning to use !pseudopc{} instead, and !to wants a file format
	VER_SHORTER_SETPC_WARNING,	// v0.93 claimed to allow *= inside !pseudopc blocks, but didn't. It shortened the warning, but '}' or !realpc clobbered PC
	VER_RIGHTASSOCIATIVEPOWEROF,	// v0.94.6 made "power of" operator right-associative
					// v0.94.7 fixed a bug: empty code segments no longer included in output file
	VER_DISABLED_OBSOLETE_STUFF,	// v0.94.8 made *= work inside !pseudopc, disabled !cbm/!realpc/!subzone
	VER_NEWFORSYNTAX,		// v0.94.12 introduced the new "!for" syntax
					// v0.95.2 changed ANC#8 from 0x2b to 0x0b
	VER_BACKSLASHESCAPING,		// v0.97 introduced backslash escaping (and therefore strings)
	VER_CURRENT,			// "RELEASE"
					// possible changes in future versions:
					//	paths should be relative to file, not start dir
					//	ignore leading zeroes?
	VER_FUTURE			// far future
};
enum debuglevel {
	DEBUGLEVEL_SERIOUS	= -3,	// ACME stops right away
	DEBUGLEVEL_ERROR	= -2,	// something is wrong
	DEBUGLEVEL_WARNING	= -1,	// something looks wrong
	DEBUGLEVEL_INFO		= 0,	// info msg ("173 bytes left in code area!")
	DEBUGLEVEL_DEBUG	= 1	// debug msg
	// debug messages with higher levels are suppressed,
	// can be changed using "--debuglevel" cli switch.
};
enum outfile_format {
	OUTFILE_FORMAT_UNSPECIFIED,	// default (uses "plain" actually)
	OUTFILE_FORMAT_PLAIN,		// no header, just code
	OUTFILE_FORMAT_CBM,		// 16-bit load address, code (default for "!to" pseudo opcode)
	OUTFILE_FORMAT_APPLE		// 16-bit load address, 16-bit length, code
};
// configuration
struct config {
	char		pseudoop_prefix;	// '!' or '.'
	int		process_verbosity;	// level of additional output
	boolean		warn_on_indented_labels;	// warn if indented label is encountered
	boolean		warn_on_type_mismatch;	// use type-checking system
	int		warn_bin_mask;	// bitmask for digit counter of binary literals
	signed long	max_errors;	// errors before giving up
	boolean		format_msvc;		// enabled by --msvc
	boolean		format_color;		// enabled by --color
	FILE		*msg_stream;		// defaults to stderr, changed to stdout by --use-stdout
	boolean		honor_leading_zeroes;	// TRUE, disabled by --ignore-zeroes
	enum debuglevel	debuglevel_segmentprobs;	// WARNING, changed to ERROR by --strict-segments
	boolean		all_warnings_are_errors;	// FALSE, enabled by --strict
	boolean		test_new_features;	// FALSE, enabled by --test
	enum version	wanted_version;	// set by --dialect (and --test --test)
	signed long	debuglevel;	// set by --debuglevel, used by "!debug"
	signed long	outbuf_size;	// 64K, "--test" changes to 16M
	const struct cpu_type	*initial_cpu_type;
	const char	*symbollist_filename;
	const char	*vicelabels_filename;
	const char	*output_filename;	// TODO - put in "part" struct
	enum outfile_format	outfile_format;
	const char	*report_filename;	// TODO - put in "part" struct
#define MEMINIT_USE_DEFAULT	256	// default value for next field if cli switch not used:
	signed long	mem_init_value;	// set by --initmem
#define NO_VALUE_GIVEN	(-1)	// default value for these fields if cli switch not used:
	signed long	initial_pc;	// set by --setpc
	signed long	outfile_start;	// set by --from-to
	signed long	outfile_limit;	// end+1, set by --from-to
};
extern struct config	config;

struct pass {
	int	number;	// counts up from zero
	int	undefined_count;	// counts undefined expression results (if this stops decreasing, next pass must list them as errors)
	//int	needvalue_count;	// counts undefined expression results actually needed for output (when this hits zero, we're done)	FIXME - use
	int	error_count;
	int	warning_count;
	boolean	complain_about_undefined;	// will be FALSE until error pass is needed
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


// prototypes

// set configuration to default values
extern void config_default(struct config *conf);

// allocate memory and die if not available
extern void *safe_malloc(size_t amount);

// set new value for "we are in !addr block" flag,
// return old value
// (called by "!addr { }")
extern boolean parser_change_addr_block_flag(boolean new_value);

// set new value for "we are in !nowarn block" flag,
// return old value
// (called by "!nowarn { }")
extern boolean parser_change_nowarn_block_flag(boolean new_value);

// called by "!addr" pseudo op if used without block
extern void parser_set_addr_prefix(void);

// called by "!nowarn" pseudo op if used without block
extern void parser_set_nowarn_prefix(void);

// call with symbol name in GlobalDynaBuf and GotByte == '='
// "powers" is for "!set" pseudo opcode so changes are allowed (see symbol.h for powers)
extern void parse_assignment(scope_t scope, bits force_bit, bits powers);

// Parse block, beginning with next byte.
// End reason (either CHAR_EOB or CHAR_EOF) can be found in GotByte afterwards
// Has to be re-entrant.
extern void parse_until_eob_or_eof(void);

// Skip space. If GotByte is CHAR_SOB ('{'), parse block and return TRUE.
// Otherwise (if there is no block), return FALSE.
// Don't forget to call EnsureEOL() afterwards.
extern int parse_optional_block(void);

// generate a debug/info/warning/error message
void throw_message(enum debuglevel level, const char msg[]);

// output a warning (something looks wrong, like "label name starts with shift-space character")
#define Throw_warning(msg)	throw_message(DEBUGLEVEL_WARNING, msg)

// output a warning if in first pass. See above.
extern void Throw_first_pass_warning(const char *msg);

// output an error (something is wrong, no output file will be generated).
// the assembler will try to go on with the assembly, so the user gets to know
// about more than one of his typos at a time.
#define Throw_error(msg)	throw_message(DEBUGLEVEL_ERROR, msg)

// output a serious error (assembly stops, for example if outbuffer overruns).
#define Throw_serious_error(msg)	throw_message(DEBUGLEVEL_SERIOUS, msg)

// handle bugs
extern void BUG(const char *msg, int code);

// insert object (in case of list, will iterate/recurse until done)
struct iter_context {
	void		(*fn)(intval_t);	// output function
	boolean		accept_long_strings;	// if FALSE, only 1-char-strings work
	unsigned char	stringxor;		// for !scrxor, 0 otherwise
};
extern void output_object(struct object *object, struct iter_context *iter);

// output 8-bit value with range check
extern void output_8(intval_t value);

// output 16-bit value with range check big-endian
extern void output_be16(intval_t value);

// output 16-bit value with range check little-endian
extern void output_le16(intval_t value);

// output 24-bit value with range check big-endian
extern void output_be24(intval_t value);

// output 24-bit value with range check little-endian
extern void output_le24(intval_t value);

// output 32-bit value (without range check) big-endian
extern void output_be32(intval_t value);

// output 32-bit value (without range check) little-endian
extern void output_le32(intval_t value);


// string to show if outputformat_set() returns nonzero
extern const char	outputformat_names[];

// convert output format name held in DynaBuf to enum.
// returns OUTFILE_FORMAT_UNSPECIFIED on error.
extern enum outfile_format outputformat_find(void);


#endif
