// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2024 Marco Baye
// Have a look at "acme.c" for further info
//
// Global stuff - things that are needed by several modules
//  4 Oct 2006	Fixed a typo in a comment
// 22 Nov 2007	Added warn_on_indented_labels
//  2 Jun 2014	Added warn_on_old_for and warn_on_type_mismatch
// 19 Nov 2014	Merged Johann Klasek's report listing generator patch
// 23 Nov 2014	Merged Martin Piper's "--msvc" error output patch
//  9 Jan 2018	Made '/' a syntax char to allow for "//" comments
// 14 Apr 2020	Added config vars for "ignore zeroes" and "segment warnings to errors"
#include "global.h"
#include <string.h>	// for strcmp()
#include "platform.h"
#include "acme.h"
#include "alu.h"
#include "cpu.h"
#include "dynabuf.h"
#include "encoding.h"
#include "input.h"
#include "macro.h"
#include "output.h"
#include "pseudoopcodes.h"
#include "section.h"
#include "symbol.h"
#include "tree.h"
#include "typesystem.h"


// constants
char		s_untitled[]	= "<untitled>";	// FIXME - this is actually const


// Exception messages during assembly
const char	exception_missing_string[]	= "No string given.";
const char	exception_negative_size[]	= "Negative size argument.";
const char	exception_no_left_brace []	= "Expected '{' character.";
const char	exception_no_memory_left[]	= "Out of memory.";
const char	exception_no_right_brace []	= "Expected '}', found EOF instead.";
//const char	exception_not_yet[]	= "Sorry, feature not yet implemented.";
// TODO - show actual value in error message
const char	exception_number_out_of_range[]	= "Number out of range.";
const char	exception_number_out_of_8b_range[]	= "Number does not fit in 8 bits.";
static const char	exception_number_out_of_16b_range[]	= "Number does not fit in 16 bits.";
static const char	exception_number_out_of_24b_range[]	= "Number does not fit in 24 bits.";
const char	exception_pc_undefined[]	= "Program counter undefined.";
const char	exception_symbol_defined[]	= "Symbol already defined.";
const char	exception_syntax[]		= "Syntax error.";
// default value for number of errors before exiting
#define MAXERRORS	10

// Flag table:
// This table contains flags for all the 256 possible byte values. The
// assembler reads the table whenever it needs to know whether a byte is
// allowed to be in a label name, for example.
//   Bits	Meaning when set
// 7.......	Byte allowed to start keyword
// .6......	Byte allowed in keyword
// ..5.....	Byte is upper case, can be lowercased by OR-ing this bit(!)
// ...4....	special character for input syntax: 0x00 TAB LF CR SPC / : ; }
// ....3...	preceding sequence of '-' characters is anonymous backward
//		label. Currently only set for ')', ',' and CHAR_EOS.
// .....210	currently unused
const char	global_byte_flags[256]	= {
/*$00*/	0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,// control characters
	0x00, 0x10, 0x10, 0x00, 0x00, 0x10, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
/*$20*/	0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,// " !"#$%&'"
	0x00, 0x08, 0x00, 0x00, 0x08, 0x00, 0x00, 0x10,// "()*+,-./"
	0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40,// "01234567"
	0x40, 0x40, 0x10, 0x10, 0x00, 0x00, 0x00, 0x00,// "89:;<=>?"
/*$40*/	0x00, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0,// "@ABCDEFG"
	0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0,// "HIJKLMNO"
	0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0,// "PQRSTUVW"
	0xe0, 0xe0, 0xe0, 0x00, 0x00, 0x00, 0x00, 0xc0,// "XYZ[\]^_"
/*$60*/	0x00, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0,// "`abcdefg"
	0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0,// "hijklmno"
	0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0,// "pqrstuvw"
	0xc0, 0xc0, 0xc0, 0x00, 0x00, 0x10, 0x00, 0x00,// "xyz{|}~" BACKSPACE
/*$80*/	0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0,// umlauts etc. ...
	0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0,
	0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0,
	0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0,
/*$a0*/	0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0,
	0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0,
	0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0,
	0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0,
/*$c0*/	0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0,
	0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0,
	0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0,
	0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0,
/*$e0*/	0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0,
	0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0,
	0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0,
	0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0,
};


// variables
char		GotByte;			// Last byte read (processed)
struct report 	*report			= NULL;
struct config	config;
struct pass	pass;
struct sanity	sanity;

// set configuration to default values
void config_default(struct config *conf)
{
	conf->pseudoop_prefix		= '!';	// can be changed to '.' by CLI switch
	conf->process_verbosity		= 0;	// level of additional output
	conf->warn_on_indented_labels	= TRUE;	// warn if indented label is encountered
	conf->warn_on_type_mismatch	= FALSE;	// use type-checking system
	conf->warn_bin_mask		= 3;  // %11 -> warn if not divisible by four
	conf->max_errors		= MAXERRORS;	// errors before giving up
	conf->sanity_limit		= SANITY_LIMIT;	// changed by --maxdepth
	conf->format_msvc		= FALSE;	// enabled by --msvc
	conf->format_color		= FALSE;	// enabled by --color
	conf->msg_stream		= stderr;	// set to stdout by --use-stdout
	conf->honor_leading_zeroes	= TRUE;		// disabled by --ignore-zeroes
	conf->debuglevel_segmentprobs	= DEBUGLEVEL_WARNING;	// changed to ERROR by --strict-segments		TODO - toggle default?
	conf->all_warnings_are_errors	= FALSE;	// enabled by --strict
	conf->test_new_features		= FALSE;	// enabled by --test
	conf->dialect			= V__CURRENT_VERSION;	// changed by --dialect
	conf->debuglevel		= DEBUGLEVEL_DEBUG;	// changed by --debuglevel, used by "!debug"
	conf->outbuf_size		= 0x10000;	// 64K, "--test" changes to 16M
	conf->initial_cpu_type		= NULL;
	conf->symbollist_filename	= NULL;
	conf->vicelabels_filename	= NULL;
	conf->output_filename		= NULL;
	conf->outfile_format		= OUTFILE_FORMAT_UNSPECIFIED;
	conf->report_filename		= NULL;
	conf->mem_init_value		= NO_VALUE_GIVEN;	// set by --initmem
	conf->initial_pc		= NO_VALUE_GIVEN;	// set by --setpc
	conf->outfile_start		= NO_VALUE_GIVEN;	// set by --from-to
	conf->outfile_limit		= NO_VALUE_GIVEN;	// end+1, set by --from-to
}

// memory allocation stuff

// allocate memory and die if not available
void *safe_malloc(size_t size)
{
	void	*block;

	if ((block = malloc(size)) == NULL)
		Throw_serious_error(exception_no_memory_left);
	return block;
}


// Parser stuff


static boolean	in_addr_block	= FALSE;
static boolean	in_nowarn_block	= FALSE;

// set new value for "we are in !addr block" flag,
// return old value
// (called by "!addr { }")
boolean parser_change_addr_block_flag(boolean new_value)
{
	boolean	old_value	= in_addr_block;

	in_addr_block = new_value;
	return old_value;
}
// set new value for "we are in !nowarn block" flag,
// return old value
// (called by "!nowarn { }")
boolean parser_change_nowarn_block_flag(boolean new_value)
{
	boolean	old_value	= in_nowarn_block;

	in_nowarn_block = new_value;
	return old_value;
}

#define SF_FOUND_BLANK		(1u << 0)	// statement started with space or tab
#define SF_FOUND_SYMBOL		(1u << 1)	// statement had label or symbol definition
#define SF_ADDR_PREFIX		(1u << 2)	// explicit symbol definition is an address
#define SF_NOWARN_PREFIX	(1u << 3)	// suppress warnings for this statement
static bits	statement_flags;

// called by "!addr" pseudo op if used without block
extern void parser_set_addr_prefix(void)
{
	statement_flags |= SF_ADDR_PREFIX;
}
// called by "!nowarn" pseudo op if used without block
extern void parser_set_nowarn_prefix(void)
{
	statement_flags |= SF_NOWARN_PREFIX;
}

// Check and return whether first symbol of statement. Complain if not.
static int first_symbol_of_statement(void)
{
	if (statement_flags & SF_FOUND_SYMBOL) {
		Throw_error("Unknown mnemonic");
		input_skip_remainder();
		return FALSE;
	}
	statement_flags |= SF_FOUND_SYMBOL;	// now there has been one
	return TRUE;
}


// parse label definition (can be either global or local).
// name must be held in GlobalDynaBuf.
// called by parse_symbol_definition, parse_backward_anon_def, parse_forward_anon_def
// "powers" is used by backward anons to allow changes
static void set_label(scope_t scope, bits force_bit, bits powers)
{
	struct symbol	*symbol;
	struct object	result;

	if ((statement_flags & SF_FOUND_BLANK) && config.warn_on_indented_labels) {
		if (pass.number == 1)
			Throw_warning("Label name not in leftmost column.");
	}
	symbol = symbol_find(scope);
	result.type = &type_number;
	vcpu_read_pc(&result.u.number);	// FIXME - if undefined, check pass.flags.complain_about_undefined and maybe throw "value not defined"!
	symbol_set_object(symbol, &result, powers);
	if (force_bit)
		symbol_set_force_bit(symbol, force_bit);
	// global labels must open new scope for cheap locals
	if (scope == SCOPE_GLOBAL)
		section_new_cheap_scope(section_now);
}


// call with symbol name in GlobalDynaBuf and '=' already eaten.
// fn is exported so "!set" pseudo opcode can call it.
// "powers" is for "!set" pseudo opcode so changes are allowed (see symbol.h for powers)
void parse_assignment(scope_t scope, bits force_bit, bits powers)
{
	struct symbol	*symbol;
	struct object	result;

	symbol = symbol_find(scope);
	ALU_any_result(&result);
	// if wanted, mark as address reference
	if (in_addr_block || (statement_flags & SF_ADDR_PREFIX)) {
		// FIXME - checking types explicitly is ugly...
		if (result.type == &type_number)
			result.u.number.addr_refs = 1;
	}
	symbol_set_object(symbol, &result, powers);
	if (force_bit)
		symbol_set_force_bit(symbol, force_bit);
}


// parse symbol definition (can be either global or local, may turn out to be a label).
// name must be held in GlobalDynaBuf.
static void parse_symbol_definition(scope_t scope)
{
	bits	force_bit;

	if (GotByte == '?')
		symbol_fix_dynamic_name();

	force_bit = input_get_force_bit();	// skips spaces after	(yes, force bit is allowed for label definitions)
	if (GotByte == '=') {
		// explicit symbol definition (symbol = <something>)
		GetByte();	// eat '='
		parse_assignment(scope, force_bit, POWER_NONE);
		input_ensure_EOS();
	} else {
		// implicit symbol definition (label)
		set_label(scope, force_bit, POWER_NONE);
	}
}


// Parse global symbol definition or assembler mnemonic
static void parse_mnemo_or_global_symbol_def(void)
{
	// read keyword and ask current cpu type if it's a mnemonic
	if (cpu_current_type->keyword_is_mnemonic(input_read_keyword()))
		return;	// statement has been handled

	// if we're here, it wasn't a mnemonic, so it can only be a symbol name
	if (!first_symbol_of_statement())
		return;	// more than one symbol, error has been reported

	// Now GotByte = illegal char
	// 04 Jun 2005: this fix should help to explain "strange" error messages.
	// 17 May 2014: now it works for UTF-8 as well.
	if ((*GLOBALDYNABUF_CURRENT == (char) 0xa0)
	|| ((GlobalDynaBuf->size >= 2) && (GLOBALDYNABUF_CURRENT[0] == (char) 0xc2) && (GLOBALDYNABUF_CURRENT[1] == (char) 0xa0))) {
		if (pass.number == 1)
			Throw_warning("Symbol name starts with a shift-space character.");
	}
	parse_symbol_definition(SCOPE_GLOBAL);
}


// parse (cheap) local symbol definition
static void parse_local_symbol_def(void)
{
	scope_t	scope;

	if (!first_symbol_of_statement())
		return;

	if (input_read_scope_and_symbol_name(&scope) == 0)
		parse_symbol_definition(scope);
}


// parse anonymous backward label definition. Called with GotByte == '-'
static void parse_backward_anon_def(void)
{
	if (!first_symbol_of_statement())
		return;

	dynabuf_clear(GlobalDynaBuf);
	do {
		DYNABUF_APPEND(GlobalDynaBuf, '-');
	} while (GetByte() == '-');
	dynabuf_append(GlobalDynaBuf, '\0');
	// backward anons change their value!
	set_label(section_now->local_scope, NO_FORCE_BIT, POWER_CHANGE_VALUE);
}


// parse anonymous forward label definition. called with GotByte == ?
static void parse_forward_anon_def(void)
{
	if (!first_symbol_of_statement())
		return;

	dynabuf_clear(GlobalDynaBuf);
	dynabuf_append(GlobalDynaBuf, '+');
	while (GotByte == '+') {
		DYNABUF_APPEND(GlobalDynaBuf, '+');
		GetByte();
	}
	symbol_fix_forward_anon_name(TRUE);	// TRUE: increment counter
	dynabuf_append(GlobalDynaBuf, '\0');
	//printf("[%d, %s]\n", section_now->local_scope, GlobalDynaBuf->buffer);
	set_label(section_now->local_scope, NO_FORCE_BIT, POWER_NONE);
}


// Parse block, beginning with next byte.
// End reason (either CHAR_EOB or CHAR_EOF) can be found in GotByte afterwards
// Has to be re-entrant.
void parse_until_eob_or_eof(void)
{
	// start with next byte
	// (don't SKIPSPACE() here, we want to warn about "label not in leftmost column"!)
	GetByte();
	// loop until end of block or end of file
	while ((GotByte != CHAR_EOB) && (GotByte != CHAR_EOF)) {
		// process one statement
		statement_flags = 0;	// no spaces, no labels, no !addr, no !nowarn
		// Parse until end of statement. Only loops in these cases:
		// - statement contains label and something else
		// - "!ifdef"/"!ifndef" is used without block
		// - "!addr"/"!nowarn" is used without block
		do {
			// check for pseudo opcodes was moved out of switch,
			// because prefix character is now configurable.
			if (GotByte == config.pseudoop_prefix) {
				pseudoopcode_parse();
			} else {
				switch (GotByte) {
				case CHAR_EOS:	// end of statement
					// Ignore now, act later
					// (stops from being "default")
					break;
				case ' ':	// space
					statement_flags |= SF_FOUND_BLANK;
					/*FALLTHROUGH*/
				case CHAR_SOL:	// start of line
					GetByte();	// skip
					break;
				case '-':
					parse_backward_anon_def();
					break;
				case '+':
					GetByte();
					if ((GotByte == LOCAL_PREFIX)
					|| (GotByte == CHEAP_PREFIX)
					|| (BYTE_CONTINUES_KEYWORD(GotByte)))
						macro_parse_call();
					else
						parse_forward_anon_def();
					break;
				case '*':
					notreallypo_setpc();	// define program counter (fn is in pseudoopcodes.c)
					break;
				case LOCAL_PREFIX:
				case CHEAP_PREFIX:
					parse_local_symbol_def();
					break;
				default:
					if (BYTE_STARTS_KEYWORD(GotByte)) {
						parse_mnemo_or_global_symbol_def();
					} else {
						Throw_error(exception_syntax);	// FIXME - include char in error message!
						input_skip_remainder();
					}
				}
			}
		} while (GotByte != CHAR_EOS);	// until end-of-statement
		output_end_statement();	// adjust program counter
		// go on with next byte
		GetByte();	//NEXTANDSKIPSPACE();
	}
}


// Skip space. If GotByte is CHAR_SOB ('{'), parse block and return TRUE.
// Otherwise (if there is no block), return FALSE.
// Don't forget to call EnsureEOL() afterwards.
int parse_optional_block(void)
{
	SKIPSPACE();
	if (GotByte != CHAR_SOB)
		return FALSE;
	parse_until_eob_or_eof();
	if (GotByte != CHAR_EOB)
		Throw_serious_error(exception_no_right_brace);
	GetByte();
	return TRUE;
}


// Error handling

// This function will do the actual output for warnings, errors and serious
// errors. It shows the given message string, as well as the current
// context: file name, line number, source type and source title.
static void throw_msg(const char *message, const char *ansicolor, const char *type)
{
	const char	*resetcolor	= "\033[0m";

	if (!config.format_color) {
		ansicolor = "";
		resetcolor = "";
	}

	if (config.format_msvc) {
		fprintf(config.msg_stream, "%s(%d) : %s%s%s (%s %s): %s\n",
			input_now->location.plat_filename, input_now->location.line_number,
			ansicolor, type, resetcolor,
			section_now->type, section_now->title, message);
	} else {
		fprintf(config.msg_stream, "%s%s%s - File %s, line %d (%s %s): %s\n",
			ansicolor, type, resetcolor,
			input_now->location.plat_filename, input_now->location.line_number,
			section_now->type, section_now->title, message);
	}
}

// generate debug/info/warning/error message
void throw_message(enum debuglevel level, const char msg[])
{
	// if level is taken from source, ensure valid value:
	if (level < DEBUGLEVEL_SERIOUS)
		level = DEBUGLEVEL_SERIOUS;

	switch (level) {
	case DEBUGLEVEL_SERIOUS:
		// output a serious error
		// (assembly stops, for example if outbuffer overruns).
		PLATFORM_SERIOUS(msg);
		throw_msg(msg, "\033[1m\033[31m", "Serious error");	// bold + red
		//++pass.error_count;	// FIXME - needed when problem below is solved
		exit(ACME_finalize(EXIT_FAILURE)); // FIXME - this inhibits output of macro call stack
	case DEBUGLEVEL_ERROR:
		// output an error
		// (something is wrong, no output file will be generated).
		PLATFORM_ERROR(msg);
		throw_msg(msg, "\033[31m", "Error");	// red
		++pass.error_count;
		if (pass.error_count >= config.max_errors)
			exit(ACME_finalize(EXIT_FAILURE));
		break;
	case DEBUGLEVEL_WARNING:
		// output a warning
		// (something looks wrong, like "label name starts with shift-space character")
		// first check if warnings should be suppressed right now:
		if (in_nowarn_block || (statement_flags & SF_NOWARN_PREFIX))
			break;
		PLATFORM_WARNING(msg);
		throw_msg(msg, "\033[33m", "Warning");	// yellow
		++pass.warning_count;
		// then check if warnings should be handled like errors:
		if (config.all_warnings_are_errors) {
			++pass.error_count;
			if (pass.error_count >= config.max_errors)
				exit(ACME_finalize(EXIT_FAILURE));
		}
		break;
	case DEBUGLEVEL_INFO:
		throw_msg(msg, "\033[32m", "Info");	// green
		break;
	default:
		// debug
		throw_msg(msg, "\033[36m", "Debug");	// cyan
		break;
	}
}


// throw "macro twice" error (FIXME - also use for "symbol twice"!)
// first output a warning, then an error, this guarantees that ACME does not
// reach the maximum error limit inbetween.
void throw_redef_error(struct location *old_def, const char msg[])
{
	struct location	buffered_location;
	const char	*buffered_section_type;
	char		*buffered_section_title;

	// CAUTION, ugly kluge: fiddle with input_now and section_now
	// data so error message is actually helpful
	// buffer old data
	buffered_location = input_now->location;
	buffered_section_type = section_now->type;
	buffered_section_title = section_now->title;
	// set new (fake) data
	input_now->location = *old_def;
	section_now->type = "earlier";
	section_now->title = "definition";
	// show warning with location of earlier definition
	Throw_warning(msg);	// FIXME - throw as info?
	// restore old data
	input_now->location = buffered_location;
	section_now->type = buffered_section_type;
	section_now->title = buffered_section_title;
	// show error with location of current definition
	Throw_error(msg);
}


// process error that might vanish if symbols change:
// if current pass is an "error output" pass, actually throw error.
// otherwise just increment counter to let mainloop know this pass wasn't successful.
void throw_symbol_error(const char *msg)
{
	// atm we just mimic the old behaviour. in future, do something like this:
	//if (pass.is_error_pass)
		Throw_error(msg);
	//else
		//++pass.symbol_errors;
}


// handle bugs
// FIXME - use a local buffer and sprintf/snprintf to put error code into message!
void BUG(const char *message, int code)
{
	Throw_warning("Bug in ACME, code follows");
	fprintf(stderr, "(0x%x:)", code);
	Throw_serious_error(message);
}


// insert object (in case of list, will iterate/recurse until done)
void output_object(struct object *object, struct iter_context *iter)
{
	struct listitem	*item;
	int		length;
	char		*read;

	if (object->type == &type_number) {
		if (object->u.number.ntype == NUMTYPE_UNDEFINED)
			iter->fn(0);
		else if (object->u.number.ntype == NUMTYPE_INT)
			iter->fn(object->u.number.val.intval);
		else if (object->u.number.ntype == NUMTYPE_FLOAT)
			iter->fn(object->u.number.val.fpval);
		else
			BUG("IllegalNumberType0", object->u.number.ntype);
	} else if (object->type == &type_list) {
		// iterate over list
		item = object->u.listhead->next;
		while (item != object->u.listhead) {
			output_object(&item->u.payload, iter);
			item = item->next;
		}
	} else if (object->type == &type_string) {
		// iterate over string
		read = object->u.string->payload;
		length = object->u.string->length;
		// single-char strings are accepted, to be more compatible with
		// versions before 0.97 (and empty strings are not really a problem...)
		if (iter->accept_long_strings || (length < 2)) {
			while (length--)
				iter->fn(iter->stringxor ^ encoding_encode_char(*(read++)));
		} else {
			Throw_error("There's more than one character.");	// see alu.c for the original of this error
		}
	} else {
		BUG("IllegalObjectType", 0);
	}
}


// output 8-bit value with range check
void output_8(intval_t value)
{
	if ((value < -0x80) || (value > 0xff))
		throw_symbol_error(exception_number_out_of_8b_range);
	output_byte(value);
}


// output 16-bit value with range check big-endian
void output_be16(intval_t value)
{
	if ((value < -0x8000) || (value > 0xffff))
		throw_symbol_error(exception_number_out_of_16b_range);
	output_byte(value >> 8);
	output_byte(value);
}


// output 16-bit value with range check little-endian
void output_le16(intval_t value)
{
	if ((value < -0x8000) || (value > 0xffff))
		throw_symbol_error(exception_number_out_of_16b_range);
	output_byte(value);
	output_byte(value >> 8);
}


// output 24-bit value with range check big-endian
void output_be24(intval_t value)
{
	if ((value < -0x800000) || (value > 0xffffff))
		throw_symbol_error(exception_number_out_of_24b_range);
	output_byte(value >> 16);
	output_byte(value >> 8);
	output_byte(value);
}


// output 24-bit value with range check little-endian
void output_le24(intval_t value)
{
	if ((value < -0x800000) || (value > 0xffffff))
		throw_symbol_error(exception_number_out_of_24b_range);
	output_byte(value);
	output_byte(value >> 8);
	output_byte(value >> 16);
}


// FIXME - the range checks below are commented out because 32-bit
// signed integers cannot exceed the range of 32-bit signed integers.
// But now that 64-bit machines are the norm, "intval_t" might be a
// 64-bit int. I need to address this problem one way or another.


// output 32-bit value (without range check) big-endian
void output_be32(intval_t value)
{
//	if ((value < -0x80000000) || (value > 0xffffffff))
//		throw_symbol_error(exception_number_out_of_32b_range);
	output_byte(value >> 24);
	output_byte(value >> 16);
	output_byte(value >> 8);
	output_byte(value);
}


// output 32-bit value (without range check) little-endian
void output_le32(intval_t value)
{
//	if ((value < -0x80000000) || (value > 0xffffffff))
//		throw_symbol_error(exception_number_out_of_32b_range);
	output_byte(value);
	output_byte(value >> 8);
	output_byte(value >> 16);
	output_byte(value >> 24);
}


// string shown in CLI error message if outputformat_set() returns nonzero:
const char	outputformat_names[]	= "'plain', 'cbm', 'apple'";

// convert output format name held in DynaBuf to enum.
// returns OUTFILE_FORMAT_UNSPECIFIED on error.
enum outfile_format outputformat_find(void)
{
	if (strcmp(GlobalDynaBuf->buffer, "plain") == 0)
		return OUTFILE_FORMAT_PLAIN;
	else if (strcmp(GlobalDynaBuf->buffer, "cbm") == 0)
		return OUTFILE_FORMAT_CBM;
	else if (strcmp(GlobalDynaBuf->buffer, "apple") == 0)
		return OUTFILE_FORMAT_APPLE;
//	else if (strcmp(GlobalDynaBuf->buffer, "o65") == 0)
//		return OUTFILE_FORMAT_O65;

	return OUTFILE_FORMAT_UNSPECIFIED;
}
