// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2024 Marco Baye
// Have a look at "acme.c" for further info
//
// Input stuff
// 19 Nov 2014	Merged Johann Klasek's report listing generator patch
//  9 Jan 2018	Allowed "//" comments
#include "input.h"
#include "config.h"
#include "alu.h"
#include "dynabuf.h"
#include "global.h"
#include "platform.h"
#include "section.h"
#include "symbol.h"
#include "tree.h"


// Constants
const char	FILE_READBINARY[]	= "rb";
#define CHAR_LF		(10)	// line feed		(in file)
		//	(10)	// start of line	(in high-level format)
#define CHAR_CR		(13)	// carriage return	(in file)
		//	(13)	// end of file		(in high-level format)
// if the characters above are changed, don't forget to adjust byte_flags[]!

// fake input structure (for error msgs before any real input is established)
static struct input	outermost	= {
	"<none>",	// file name for resolving paths
	{
		"<none>",	// file name where code initially came from (differs during macro execution)
		0,		// line number
	},
	INPUTSRC_FILE,	// fake file access, so no RAM read
	INPUTSTATE_EOF,	// state of input
	{
		NULL	// RAM read pointer or file handle
	}
};


// variables
struct input	*input_now	= &outermost;	// current input structure


// functions

// parse a whole source code file
// file name must be given in platform style, i.e.
// "directory/basename.extension" on linux,
// "directory.basename/extension" on RISC OS, etc.
// and the pointer must remain valid forever!
void input_parse_and_close_platform_file(const char *eternal_plat_filename, FILE *fd)
{
	struct input	new_input,
			*outer_input;

	// be verbose
	if (config.process_verbosity >= 3)
		printf("Parsing source file \"%s\".\n", eternal_plat_filename);
	// set up new input
	new_input.plat_pathref_filename		= eternal_plat_filename;
	new_input.location.plat_filename	= eternal_plat_filename;
	new_input.location.line_number		= 1;
	new_input.source		= INPUTSRC_FILE;
	new_input.state			= INPUTSTATE_SOF;
	new_input.src.fd		= fd;
	// remember where outer input struct is
	outer_input = input_now;
	// activate new input struct
	input_now = &new_input;
	// parse block and check end reason
	parse_until_eob_or_eof();
	if (GotByte != CHAR_EOF)
		Throw_error("Expected EOF, found '}' instead." );
	// close sublevel src
	// (this looks like we could just use "fd" as arg, but maybe the file
	// has been replaced with a different one in the meantime...)
	fclose(input_now->src.fd);
	// restore outer input struct
	input_now = outer_input;
}


// remember source code character for report generator
#define HEXBUFSIZE	9	// actually, 4+1 is enough, but for systems without snprintf(), let's be extra-safe.
#define IF_WANTED_REPORT_SRCCHAR(c)	do { if (report->fd) report_srcchar(c); } while(0)
static void report_srcchar(char new_char)
{
	static char	prev_char	= '\0';
	int		ii;
	char		hex_address[HEXBUFSIZE];
	char		hexdump[2 * REPORT_BINBUFSIZE + 2];	// +2 for '.' and terminator

	// if input has changed, insert explanation
	if (input_now != report->last_input) {
		fprintf(report->fd, "\n; ******** Source: %s\n", input_now->location.plat_filename);	// actually, UNIX-style might be better than platform-style here...
		report->last_input = input_now;
		report->asc_used = 0;	// clear buffer
		prev_char = '\0';
	}
	if (prev_char == '\n') {
		// line start after line break detected and EOS processed,
		// build report line:
		// show line number...
		fprintf(report->fd, "%6d  ", input_now->location.line_number - 1);
		// prepare outbytes' start address
		if (report->bin_used) {
#if _BSD_SOURCE || _XOPEN_SOURCE >= 500 || _ISOC99_SOURCE || _POSIX_C_SOURCE >= 200112L
			snprintf(hex_address, HEXBUFSIZE, "%04x", report->bin_address);
#else
			sprintf(hex_address, "%04x", report->bin_address);
#endif
		} else {
			hex_address[0] = '\0';
		}
		// prepare outbytes
		hexdump[0] = '\0';
		for (ii = 0; ii < report->bin_used; ++ii)
			sprintf(hexdump + 2 * ii, "%02x", (unsigned int) (unsigned char) (report->bin_buf[ii]));
		// if binary buffer is full, overwrite last byte with "..."
		if (report->bin_used == REPORT_BINBUFSIZE)
			sprintf(hexdump + 2 * (REPORT_BINBUFSIZE - 1), "...");
		// show address and bytes
		fprintf(report->fd, "%-4s %-19s", hex_address, hexdump);
		// at this point the output should be a multiple of 8 characters
		// so far to preserve tabs of the source...
		if (report->asc_used == REPORT_ASCBUFSIZE)
			--report->asc_used;
		report->asc_buf[report->asc_used] = '\0';
		fprintf(report->fd, "%s\n", report->asc_buf);	// show source line
		report->asc_used = 0;	// reset buffers
		report->bin_used = 0;
	}
	if (new_char != '\n' && new_char != '\r') {	// detect line break
		if (report->asc_used < REPORT_ASCBUFSIZE)
			report->asc_buf[report->asc_used++] = new_char;
	}
	prev_char = new_char;
}


// Deliver source code from current file (!) in shortened high-level format
static char get_processed_from_file(void)
{
	static int	from_file	= 0;

	for (;;) {
		switch (input_now->state) {
		case INPUTSTATE_SOF:
			// fetch first byte from the current source file
			from_file = getc(input_now->src.fd);
			IF_WANTED_REPORT_SRCCHAR(from_file);
			//TODO - check for bogus/malformed BOM (0xef 0xbb 0xbf as UTF-8-encoded 0xfeff) and ignore?
			// check for hashbang line and ignore
			if (from_file == '#') {
				// remember to skip remainder of line
				input_now->state = INPUTSTATE_COMMENT;
				return CHAR_EOS;	// end of statement
			}
			input_now->state = INPUTSTATE_AGAIN;
			break;
		case INPUTSTATE_NORMAL:
			// fetch a fresh byte from the current source file
			from_file = getc(input_now->src.fd);
			IF_WANTED_REPORT_SRCCHAR(from_file);
			// now process it
			/*FALLTHROUGH*/
		case INPUTSTATE_AGAIN:
			// Process the latest byte again. Of course, this only
			// makes sense if the loop has executed at least once,
			// otherwise the contents of from_file are undefined.
			// If the source is changed so there is a possibility
			// to enter INPUTSTATE_AGAIN mode without first having
			// defined "from_file", trouble may arise...
			input_now->state = INPUTSTATE_NORMAL;
			// EOF must be checked first because it cannot be used
			// as an index into global_byte_flags[]
			if (from_file == EOF) {
				// remember to send an end-of-file
				input_now->state = INPUTSTATE_EOF;
				return CHAR_EOS;	// end of statement
			}

			// check whether character is special one
			// if not, everything's cool and froody, so return it
			if (BYTE_IS_SYNTAX_CHAR(from_file) == 0)
				return (char) from_file;

			// check special characters ("0x00 TAB LF CR SPC / : ; }")
			switch (from_file) {
			case '\t':
			case ' ':
				// remember to skip all following blanks
				input_now->state = INPUTSTATE_SKIPBLANKS;
				return ' ';

			case CHAR_LF:	// LF character
				// remember to send a start-of-line
				input_now->state = INPUTSTATE_LF;
				return CHAR_EOS;	// end of statement

			case CHAR_CR:	// CR character
				// remember to check CRLF + send start-of-line
				input_now->state = INPUTSTATE_CR;
				return CHAR_EOS;	// end of statement

			case CHAR_EOB:
				// remember to send an end-of-block
				input_now->state = INPUTSTATE_EOB;
				return CHAR_EOS;	// end of statement

			case '/':
				// to check for "//", get another byte:
				from_file = getc(input_now->src.fd);
				IF_WANTED_REPORT_SRCCHAR(from_file);
				if (from_file != '/') {
					// not "//", so:
					input_now->state = INPUTSTATE_AGAIN;	// second byte must be parsed normally later on
					return '/';	// first byte is returned normally right now
				}
				// it's really "//", so act as if ';'
				/*FALLTHROUGH*/
			case ';':
				// remember to skip remainder of line
				input_now->state = INPUTSTATE_COMMENT;
				return CHAR_EOS;	// end of statement

			case ':':	// statement delimiter
				// just deliver an EOS instead
				return CHAR_EOS;	// end of statement

			default:
				// complain if byte is 0
				Throw_error("Source file contains illegal character.");	// FIXME - throw some dynamic "did not expect XYZ character" error instead!
				return (char) from_file;
			}
		case INPUTSTATE_SKIPBLANKS:
			// read until non-blank, then deliver that
			do {
				from_file = getc(input_now->src.fd);
				IF_WANTED_REPORT_SRCCHAR(from_file);
			} while ((from_file == '\t') || (from_file == ' '));
			// re-process last byte
			input_now->state = INPUTSTATE_AGAIN;
			break;
		case INPUTSTATE_LF:
			// return start-of-line, then continue in normal mode
			input_now->state = INPUTSTATE_NORMAL;
			return CHAR_SOL;	// new line

		case INPUTSTATE_CR:
			// return start-of-line, remember to check for LF
			input_now->state = INPUTSTATE_SKIPLF;
			return CHAR_SOL;	// new line

		case INPUTSTATE_SKIPLF:
			from_file = getc(input_now->src.fd);
			IF_WANTED_REPORT_SRCCHAR(from_file);
			// if LF, ignore it and fetch another byte
			// otherwise, process current byte
			if (from_file == CHAR_LF)
				input_now->state = INPUTSTATE_NORMAL;
			else
				input_now->state = INPUTSTATE_AGAIN;
			break;
		case INPUTSTATE_COMMENT:
			// read until end-of-line or end-of-file
			do {
				from_file = getc(input_now->src.fd);
				IF_WANTED_REPORT_SRCCHAR(from_file);
			} while ((from_file != EOF) && (from_file != CHAR_CR) && (from_file != CHAR_LF));
			// re-process last byte
			input_now->state = INPUTSTATE_AGAIN;
			break;
		case INPUTSTATE_EOB:
			// deliver EOB
			input_now->state = INPUTSTATE_NORMAL;
			return CHAR_EOB;	// end of block

		case INPUTSTATE_EOF:
			// deliver EOF
			input_now->state = INPUTSTATE_NORMAL;
			return CHAR_EOF;	// end of file

		default:
			BUG("StrangeInputMode", input_now->state);
		}
	}
}

// This function delivers the next byte from the currently active byte source
// in shortened high-level format. FIXME - use fn ptr?
// When inside quotes, use input_quoted_to_dynabuf() instead!
char GetByte(void)
{
//	for (;;) {
		// If byte source is RAM, then no conversions are
		// necessary, because in RAM the source already has
		// high-level format
		// Otherwise, the source is a file. This means we will call
		// get_processed_from_file() which will do a shit load of conversions.
		switch (input_now->source) {
		case INPUTSRC_RAM:
			GotByte = *(input_now->src.ram_ptr++);
			break;
		case INPUTSRC_FILE:
			GotByte = get_processed_from_file();
			break;
		default:
			BUG("IllegalInputSrc", input_now->source);
		}
//		// if start-of-line was read, increment line counter and repeat
//		if (GotByte != CHAR_SOL)
//			return GotByte;
//		input_now->location.line_number++;
//	}
		if (GotByte == CHAR_SOL)
			input_now->location.line_number++;
		return GotByte;
}

// This function delivers the next byte from the currently active byte source
// in un-shortened high-level format.
// This function complains if CHAR_EOS (end of statement) is read.
// TODO - check if return value is actually used
static char GetQuotedByte(void)
{
	int	from_file;	// must be an int to catch EOF

	switch (input_now->source) {
	case INPUTSRC_RAM:
		// if byte source is RAM, then no conversion is necessary,
		// because in RAM the source already has high-level format
		GotByte = *(input_now->src.ram_ptr++);
		break;
	case INPUTSRC_FILE:
		// fetch a fresh byte from the current source file
		from_file = getc(input_now->src.fd);
		IF_WANTED_REPORT_SRCCHAR(from_file);
		switch (from_file) {
		case EOF:
			// remember to send an end-of-file
			input_now->state = INPUTSTATE_EOF;
			GotByte = CHAR_EOS;	// end of statement
			break;
		case CHAR_LF:	// LF character
			// remember to send a start-of-line
			input_now->state = INPUTSTATE_LF;
			GotByte = CHAR_EOS;	// end of statement
			break;
		case CHAR_CR:	// CR character
			// remember to check for CRLF + send a start-of-line
			input_now->state = INPUTSTATE_CR;
			GotByte = CHAR_EOS;	// end of statement
			break;
		default:
			GotByte = from_file;
		}
		break;
	default:
		BUG("IllegalInputSrc", input_now->source);
	}
	// now check for end of statement
	if (GotByte == CHAR_EOS)
		Throw_error("Quotes still open at end of line.");
	return GotByte;
}

// Skip remainder of statement, for example on error
void input_skip_remainder(void)
{
	// read characters until end-of-statement, but check for quotes,
	// otherwise this might treat a quoted colon like EOS!
	dynabuf_clear(GlobalDynaBuf);
	while (GotByte != CHAR_EOS) {
		// check for quotes
		if ((GotByte == '"') || (GotByte == '\'')) {
			if (input_quoted_to_dynabuf(GotByte))
				break;	// error (CHAR_EOS before closing quote)
		}
		GetByte();
	}
	dynabuf_clear(GlobalDynaBuf);
}

// Ensure that the remainder of the current statement is empty, for example
// after mnemonics using implied addressing.
void input_ensure_EOS(void)	// Now GotByte = first char to test
{
	SKIPSPACE();
	if (GotByte) {
		// FIXME - move this to its own function!
		char	buf[80];	// actually needed are 51
		char	quote;		// character before and after

		// FIXME - change quoting: do not assume char is printable!
		quote = (GotByte == '\'') ? '"' : '\'';	// use single quotes, unless byte is a single quote (then use double quotes)
		sprintf(buf, "Expected end-of-statement, found %c%c%c instead.", quote, GotByte, quote);
		Throw_error(buf);
		input_skip_remainder();
	}
}

// read string to dynabuf until closing quote is found
// returns 1 on errors (unterminated, escaping error)
int input_quoted_to_dynabuf(char closing_quote)
{
	boolean	escaped	= FALSE;

	//dynabuf_clear(GlobalDynaBuf);	// do not clear, caller might want to append to existing contents (TODO - check!)
	for (;;) {
		GetQuotedByte();
		if (GotByte == CHAR_EOS)
			return 1;	// unterminated string constant; GetQuotedByte will have complained already

		if (escaped) {
			// previous byte was backslash, so do not check for terminator nor backslash
			escaped = FALSE;
			// do not actually _convert_ escape sequences to their target byte, that is done by input_unescape_dynabuf() below!
			// TODO - but maybe check for illegal escape sequences?
			// at the moment checking is only done when the string
			// gets used for something...
		} else {
			// non-escaped: only terminator and backslash are of interest
			if (GotByte == closing_quote)
				return 0;	// ok

			if ((GotByte == '\\') && (config.dialect >= V0_97__BACKSLASH_ESCAPING))
				escaped = TRUE;
		}
		DYNABUF_APPEND(GlobalDynaBuf, GotByte);
	}
}

// process backslash escapes in GlobalDynaBuf (so size might shrink)
// returns 1 on errors (escaping errors)
// TODO - check: if this is only ever called directly after input_quoted_to_dynabuf, integrate that call here?
int input_unescape_dynabuf(void)
{
	int	read_index	= 0,
		write_index	= 0;
	char	byte;
	boolean	escaped;

	if (config.dialect < V0_97__BACKSLASH_ESCAPING)
		return 0;	// ok

	escaped = FALSE;
	// CAUTION - contents of dynabuf are not terminated:
	while (read_index < GlobalDynaBuf->size) {
		byte = GLOBALDYNABUF_CURRENT[read_index++];
		if (escaped) {
			switch (byte) {
			case '\\':
			case '\'':
			case '"':
				break;
			case '0':	// NUL
				byte = 0;
				break;
			case 't':	// TAB
				byte = 9;
				break;
			case 'n':	// LF
				byte = 10;
				break;
			case 'r':	// CR
				byte = 13;
				break;
			// TODO - 'a' to BEL? others?
			default:
				Throw_error("Unsupported backslash sequence.");	// TODO - add unexpected character to error message?
			}
			GLOBALDYNABUF_CURRENT[write_index++] = byte;
			escaped = FALSE;
		} else {
			if (byte == '\\') {
				escaped = TRUE;
			} else {
				GLOBALDYNABUF_CURRENT[write_index++] = byte;
			}
		}
	}
	if (escaped)
		BUG("PartialEscapeSequence", 0);
	GlobalDynaBuf->size = write_index;
	return 0;	// ok
}

// Read block into GlobalDynabuf
// (reading starts with next byte, so call directly after reading opening brace).
// After calling this function, GotByte holds '}'. Unless EOF was found first,
// but then a serious error would have been thrown.
static void block_to_dynabuf(void)
{
	char	byte;
	int	depth	= 1;	// to find matching block end

	// prepare global dynamic buffer
	dynabuf_clear(GlobalDynaBuf);
	do {
		byte = GetByte();
		// store
		DYNABUF_APPEND(GlobalDynaBuf, byte);
		// now check for some special characters
		switch (byte) {
		case CHAR_EOF:	// End-of-file in block? Sorry, no way.
			Throw_serious_error(exception_no_right_brace);

		case '"':	// Quotes? Okay, read quoted stuff.
		case '\'':
			input_quoted_to_dynabuf(byte);
			DYNABUF_APPEND(GlobalDynaBuf, GotByte);	// add closing quote
			break;
		case CHAR_SOB:
			++depth;
			break;
		case CHAR_EOB:
			--depth;
			break;
		}
	} while (depth);
}
// Skip block (starting with next byte, so call directly after reading opening brace).
// After calling this function, GotByte holds '}'. Unless EOF was found first,
// but then a serious error would have been thrown.
void input_block_skip(void)
{
	block_to_dynabuf();
}
// Read block into GlobalDynabuf, make a copy and return a pointer to that
// (reading starts with next byte, so call directly after reading opening brace).
// After calling this function, GotByte holds '}'. Unless EOF was found first,
// but then a serious error would have been thrown.
char *input_block_getcopy(void)
{
	block_to_dynabuf();
	// prepare to return copy of block
	// add EOF, just to make sure block is never read too far
	dynabuf_append(GlobalDynaBuf, CHAR_EOS);
	dynabuf_append(GlobalDynaBuf, CHAR_EOF);
	// return pointer to copy
	return dynabuf_get_copy(GlobalDynaBuf);
}

// Append to GlobalDynaBuf while characters are legal for keywords.
// Throws "missing string" error if none.
// Returns number of characters added.
static int append_keyword_to_global_dynabuf(void)
{
	int	length	= 0;

	// add characters to buffer until an illegal one comes along
	while (BYTE_CONTINUES_KEYWORD(GotByte)) {
		DYNABUF_APPEND(GlobalDynaBuf, GotByte);
		++length;
		GetByte();
	}
	if (length == 0)
		Throw_error(exception_missing_string);
	return length;
}

// append optional '.'/'@' prefix to GlobalDynaBuf, then keep
// appending while characters are legal for keywords.
// throw "missing string" error if none.
// return whether there was an error.
int input_append_symbol_name_to_global_dynabuf(void)
{
	if ((GotByte == LOCAL_PREFIX)
	|| (GotByte == CHEAP_PREFIX)) {
		dynabuf_append(GlobalDynaBuf, GotByte);
		GetByte();
	} else if (!BYTE_STARTS_KEYWORD(GotByte)) {
		// FIXME - show invalid char in error message!
		Throw_error(exception_missing_string);
		return 1;	// error
	}
	return append_keyword_to_global_dynabuf() == 0;	// zero length -> error!
}

// read symbol name into GlobalDynaBuf, set scope,
// return whether there was an error (namely, "no string given").
int input_readscopeandsymbolname(scope_t *scope, boolean dotkluge)
{
	int	err;

	SKIPSPACE();
	dynabuf_clear(GlobalDynaBuf);

	if (dotkluge) {
		// this happens after the expression parser has eaten the '.'
		// and did not find a decimal digit. -> not a float value ->
		// must be a local symbol -> we must restore the '.' in front!
		dynabuf_append(GlobalDynaBuf, '.');
		err = append_keyword_to_global_dynabuf() == 0;	// zero length -> error!
	} else {
		err = input_append_symbol_name_to_global_dynabuf();
	}
	// add terminator to buffer (increments buffer's length counter)
	dynabuf_append(GlobalDynaBuf, '\0');
	if (err) {
		*scope = SCOPE_GLOBAL;	// bogus, but at least not un-initialized
		return 1;	// error
	}
	if (GLOBALDYNABUF_CURRENT[0] == LOCAL_PREFIX) {
		*scope = section_now->local_scope;
	} else if (GLOBALDYNABUF_CURRENT[0] == CHEAP_PREFIX) {
		*scope = section_now->cheap_scope;
	} else {
		*scope = SCOPE_GLOBAL;
	}
	return 0;	// no error
}

// Clear dynamic buffer, then append to it until an illegal (for a keyword)
// character is read. Zero-terminate the string. Return its length (without
// terminator).
// Zero lengths will produce a "missing string" error.
int input_read_keyword(void)
{
	int	length;

	dynabuf_clear(GlobalDynaBuf);
	length = append_keyword_to_global_dynabuf();
	// add terminator to buffer (increments buffer's length counter)
	dynabuf_append(GlobalDynaBuf, '\0');
	return length;
}

// Clear dynamic buffer, then append to it until an illegal (for a keyword)
// character is read. Zero-terminate the string, then convert to lower case.
// Return its length (without terminator).
// Zero lengths will produce a "missing string" error.
int input_read_and_lower_keyword(void)
{
	int	length;

	dynabuf_clear(GlobalDynaBuf);
	length = append_keyword_to_global_dynabuf();
	// add terminator to buffer (increments buffer's length counter)
	dynabuf_append(GlobalDynaBuf, '\0');
	dynabuf_to_lower(GlobalDynaBuf, GlobalDynaBuf);	// convert to lower case
	return length;
}

// shared ending when trying to read a file name.
// The file name given in the assembler source code is converted from
// UNIX style to platform style.
// Returns nonzero on error. Filename in GlobalDynaBuf, including terminator.
// Errors are handled and reported, but caller should call
// input_skip_remainder() then.
static int read_filename_shared_end(boolean *absolute)
{
	// check length
	if (GlobalDynaBuf->size == 0) {
		Throw_error("No file name given.");
		return 1;	// error
	}

	// resolve backslash escapes
	if (input_unescape_dynabuf())
		return 1;	// escaping error

	// terminate string
	dynabuf_append(GlobalDynaBuf, '\0');
	// add another zero byte to make sure the buffer is large enough so the
	// string could grow another byte:
	dynabuf_append(GlobalDynaBuf, '\0');
	// (this is an extremely ugly kluge for an extremely unlikely situation,
	// but still less ugly than all other workarounds I could think of. see
	// _riscos.c for the extremely unlikely situation where this is needed)

	// platform-specific path name conversion
	// (and tell absolute/relative paths apart)
	platform_convert_path(absolute, GLOBALDYNABUF_CURRENT);

	return 0;	// ok
}

// try to read a file name for an input file.
// library access by using <...> quoting is allowed.
// flags for "library access" and "absolute path" will be set accordingly.
// The file name given in the assembler source code is converted from
// UNIX style to platform style.
// Returns nonzero on error. Filename in GlobalDynaBuf.
// Errors are handled and reported, but caller should call
// input_skip_remainder() then.
int input_read_input_filename(struct filespecflags *flags)
{
	dynabuf_clear(GlobalDynaBuf);
	SKIPSPACE();
	if (GotByte == '<') {
		// library access:
		flags->uses_lib = TRUE;
		// read file name string (must be a single string <literal>)
		if (input_quoted_to_dynabuf('>'))
			return 1;	// unterminated or escaping error

		GetByte();	// eat '>' terminator
	} else {
		// "normal", non-library access:
		flags->uses_lib = FALSE;
// old algo (do not merge with similar parts from "if" block!):
		if (GotByte != '"') {
			Throw_error("File name quotes not found (\"\" or <>).");
			return 1;	// error
		}
		// read file name string
		if (input_quoted_to_dynabuf('"'))
			return 1;	// unterminated or escaping error

		GetByte();	// eat terminator
// new algo:
// it should be possible to construct the name of input file from symbols, so
// build environments can define a name at one place and use it at another.
// FIXME - use expression parser to read filename string!
// see lines 416 and 1317 in pseudoopcodes.c for two more possible callers!
	}

	// check length, remember abs/rel, unescape, terminate, do platform conversion
	return read_filename_shared_end(&flags->absolute);
}

// Try to read a comma, skipping spaces before and after. Return TRUE if comma
// found, otherwise FALSE.
int input_accept_comma(void)
{
	SKIPSPACE();
	if (GotByte != ',')
		return FALSE;

	NEXTANDSKIPSPACE();
	return TRUE;
}

// Try to read given character.
// If found, eat character and return TRUE.
// If not found, throw syntax error and return FALSE.
int input_expect(int chr)
{
	// one caller uses this to read the '=' part of "!=", so
	// do not call SKIPSPACE() here!
	if (GotByte == chr) {
		GetByte();	// eat expected char
		return TRUE;
	}
	Throw_error(exception_syntax);	// FIXME - build "expected X, found Y" error msg!
	return FALSE;
}

// read optional info about parameter length
// FIXME - move to different file!
bits input_get_force_bit(void)
{
	char	byte;
	bits	force_bit	= 0;

	if (GotByte == '+') {
		byte = GetByte();
		if (byte == '1')
			force_bit = NUMBER_FORCES_8;
		else if (byte == '2')
			force_bit = NUMBER_FORCES_16;
		else if (byte == '3')
			force_bit = NUMBER_FORCES_24;
		if (force_bit)
			GetByte();
		else
			Throw_error("Illegal postfix.");
	}
	SKIPSPACE();
	return force_bit;
}


static	STRUCT_DYNABUF_REF(pathbuf, 256);	// to combine search path and file spec

// copy platform-specific library search path into pathbuf:
static void library_path_to_pathbuf(void)
{
	char	*lib_prefix;	// depends on platform

	dynabuf_clear(pathbuf);
	lib_prefix = PLATFORM_LIBPREFIX;
	if ((PLATFORM_NEEDS_ENV_VAR) && (lib_prefix == NULL)) {
		Throw_error("\"ACME\" environment variable not found.");
	} else {
		dynabuf_add_string(pathbuf, lib_prefix);
	}
}

// copy "default search path" from current file's file name into pathbuf:
static void default_path_to_pathbuf(void)
{
	const char	*start	= input_now->plat_pathref_filename,
			*readptr,
			*found;

	dynabuf_clear(pathbuf);
	if (config.dialect >= V0_98__PATHS_AND_SYMBOLCHANGE) {
		// scan filename for last directory separator
		readptr = start;
		found = NULL;
		while (*readptr) {
			if ((*readptr == DIRECTORY_SEPARATOR)
			|| (*readptr == ALTERNATIVE_DIR_SEP)) {
				found = readptr;
			}
			++readptr;
		}
		if (found) {
			// +1 because we want the separator as well:
			dynabuf_add_bytes(pathbuf, start, found - start + 1);
		}
	} else {
		// do nothing -
		// pathbuf is empty, which means "default search path" is "",
		// which is exactly like it was in older versions.
	}
}

// try to read a file name for an output file.
// library access by using <...> quoting is forbidden.
// The file name given in the assembler source code is converted from
// UNIX style to platform style.
// Returns nonzero on error. Filename in GlobalDynaBuf.
// Errors are handled and reported, but caller should call
// input_skip_remainder() then.
//
// this is only used for "!to" and "!sl", i.e. output file names. these
// must be given as a literal string, and it should be kept this way.
int input_read_output_filename(void)
{
	boolean	absolute;

	SKIPSPACE();
	if (GotByte == '<') {
		Throw_error("Writing to library not supported.");
		return 1;	// error
	}
	if (GotByte != '"') {
		Throw_error("File name quotes not found (\"\").");
		return 1;	// error
	}
	dynabuf_clear(GlobalDynaBuf);
	// read file name string (must be a single string literal! do not change this!)
	if (input_quoted_to_dynabuf('"'))
		return 1;	// unterminated or escaping error

	GetByte();	// eat terminator
	// check length, remember abs/rel, unescape, terminate, do platform conversion:
	if (read_filename_shared_end(&absolute))
		return 1;	// empty string or escaping error

	if (absolute) {
		// keep file name as it is
	} else {
		// get current file's path
		default_path_to_pathbuf();
		// add output file name
		dynabuf_add_string(pathbuf, GLOBALDYNABUF_CURRENT);
		// terminate
		dynabuf_append(pathbuf, '\0');
		// copy full file name back to GlobalDynaBuf
		dynabuf_clear(GlobalDynaBuf);
		dynabuf_add_string(GlobalDynaBuf, pathbuf->buffer);
		dynabuf_append(GlobalDynaBuf, '\0');
	}

	return 0;	// ok
}


// "include path" stuff:

// ring list struct for "include path items"
struct ipi {
	struct ipi	*next,
			*prev;
	const char	*path;
};

// head element
static struct ipi	ipi_head	= {&ipi_head, &ipi_head, NULL};

// add entry
void includepaths_add(const char *path)
{
	struct ipi	*ipi;

	ipi = safe_malloc(sizeof(*ipi));
	ipi->path = path;
	ipi->next = &ipi_head;
	ipi->prev = ipi_head.prev;
	ipi->next->prev = ipi;
	ipi->prev->next = ipi;
}

// add filename (from GlobalDynaBuf) to pathbuf and try to open file:
static FILE *combine_and_open_ro(void)
{
	FILE	*stream;

	// if path does not end with directory separator, add one:
	if (pathbuf->size
	&& (pathbuf->buffer[pathbuf->size - 1] != DIRECTORY_SEPARATOR)
	&& (pathbuf->buffer[pathbuf->size - 1] != ALTERNATIVE_DIR_SEP)) {
		dynabuf_append(pathbuf, DIRECTORY_SEPARATOR);
	}
	// add file name
	dynabuf_add_string(pathbuf, GLOBALDYNABUF_CURRENT);
	// terminate
	dynabuf_append(pathbuf, '\0');
	// try to open for reading
	stream = fopen(pathbuf->buffer, FILE_READBINARY);
	if (config.process_verbosity >= 9)
		printf("Trying \"%s\"...%s\n", pathbuf->buffer, stream ? " OK!" : "");
	return stream;
}

// open file for reading
// "flags" decide whether library access, search paths or absolute path is wanted.
// file name is expected in GlobalDynaBuf, in platform style and terminated.
// returns NULL or open stream
// on success, GlobalDynaBuf contains full file name in platform style
FILE *includepaths_open_ro(struct filespecflags *flags)
{
	FILE		*stream;
	struct ipi	*ipi;

	if (flags->uses_lib) {
		// use library prefix
		library_path_to_pathbuf();
		stream = combine_and_open_ro();
	} else {
		// first try current default prefix
		if (flags->absolute)
			dynabuf_clear(pathbuf);	// which is "" if absolute
		else
			default_path_to_pathbuf();	// or current path if relative
		stream = combine_and_open_ro();
		if (stream == NULL) {
			// default prefix failed, so try list entries:
			// (does not seem to make much sense for absolute paths,
			// but maybe some windows user used search paths like
			// "D:" or "E:" - oh well...)
			for (ipi = ipi_head.next; ipi != &ipi_head; ipi = ipi->next) {
				dynabuf_clear(pathbuf);
				dynabuf_add_string(pathbuf, ipi->path);
				stream = combine_and_open_ro();
				if (stream) {
					break;
				}
			}
		}
	}
	if (stream) {
		// copy successful file name back to GlobalDynaBuf
		dynabuf_clear(GlobalDynaBuf);
		dynabuf_add_string(GlobalDynaBuf, pathbuf->buffer);
		dynabuf_append(GlobalDynaBuf, '\0');
	} else {
		// CAUTION, I'm re-using the path dynabuf to assemble the error message:
		dynabuf_clear(pathbuf);
		dynabuf_add_string(pathbuf, "Cannot open input file ");
		dynabuf_append(pathbuf, flags->uses_lib ? '<' : '\"');
		dynabuf_add_string(pathbuf, GLOBALDYNABUF_CURRENT);
		dynabuf_append(pathbuf, flags->uses_lib ? '>' : '\"');
		dynabuf_append(pathbuf, '.');
		dynabuf_append(pathbuf, '\0');
		Throw_error(pathbuf->buffer);
	}
	//fprintf(stderr, "File is [%s]\n", GLOBALDYNABUF_CURRENT);
	return stream;
}
