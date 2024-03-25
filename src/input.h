// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2024 Marco Baye
// Have a look at "acme.c" for further info
//
// Input stuff
#ifndef input_H
#define input_H


#include <stdio.h>	// for FILE
#include "config.h"	// for bits and scope_t


// type definitions

// values for input component "src.state"
enum inputstate {
	INPUTSTATE_SOF,		// start of file (check for hashbang)
	INPUTSTATE_NORMAL,	// everything's fine
	INPUTSTATE_AGAIN,	// re-process last byte
	INPUTSTATE_SKIPBLANKS,	// shrink multiple spaces
	INPUTSTATE_LF,		// send start-of-line after end-of-statement
	INPUTSTATE_CR,		// same, but also remember to skip LF
	INPUTSTATE_SKIPLF,	// skip LF if that's next
	INPUTSTATE_COMMENT,	// skip characters until newline or EOF
	INPUTSTATE_EOB,		// send end-of-block after end-of-statement
	INPUTSTATE_EOF,		// send end-of-file after end-of-statement
};
enum inputsrc {
	INPUTSRC_FILE,
	INPUTSRC_RAM
};
struct input {
	const char	*plat_pathref_filename;	// file
	// the filename *above* refers to the source file currently being parsed, which
	// is needed as a reference for relative paths.
	// the filename *below* (in "location") refers to the source file where
	// the current code initially came from, i.e. it may change during macro execution.
	struct location	location;	// file + line (during RAM reads as well)
	enum inputsrc	source;
	enum inputstate	state;	// state of input
	union {
		FILE	*fd;		// file descriptor
		char	*ram_ptr;	// RAM read ptr (loop or macro block)
	} src;
};
struct filespecflags {
	boolean	uses_lib;	// file name was given in <...> instead of "..."
	boolean	absolute;	// file name started with '/'
};


// Constants
extern const char	FILE_READBINARY[];
// Special characters
// The program *heavily* relies on CHAR_EOS (end of statement) being 0x00!
#define CHAR_EOS	(0)	// end of statement	(in high-level format)
#define CHAR_SOB	'{'	// start of block
#define CHAR_EOB	'}'	// end of block
#define CHAR_SOL	(10)	// start of line	(in high-level format)
#define CHAR_EOF	(13)	// end of file		(in high-level format)
// if the characters above are changed, don't forget to adjust global_byte_flags[]!


// Variables
extern struct input	*input_now;	// current input structure


// Prototypes

// parse a whole source code file
// file name must be given in platform style, i.e.
// "directory/basename.extension" on linux,
// "directory.basename/extension" on RISC OS, etc.
// and the pointer must remain valid forever!
extern void input_parse_and_close_platform_file(const char *eternal_plat_filename, FILE *fd);

// get next byte from currently active byte source in shortened high-level
// format. When inside quotes, use input_quoted_to_dynabuf() instead!
extern char GetByte(void);

// Skip remainder of statement, for example on error
extern void input_skip_remainder(void);

// Ensure that the remainder of the current statement is empty, for example
// after mnemonics using implied addressing.
extern void input_ensure_EOS(void);

// read string to dynabuf until closing quote is found
// returns 1 on errors (unterminated, escaping error)
extern int input_quoted_to_dynabuf(char closing_quote);

// process backslash escapes in GlobalDynaBuf (so size might shrink)
// returns 1 on errors (escaping errors)
extern int input_unescape_dynabuf(void);

// Skip block (starting with next byte, so call directly after reading opening brace).
// After calling this function, GotByte holds '}'. Unless EOF was found first,
// but then a serious error would have been thrown.
extern void input_block_skip(void);
// Read block into GlobalDynabuf, make a copy and return a pointer to that
// (reading starts with next byte, so call directly after reading opening brace).
// After calling this function, GotByte holds '}'. Unless EOF was found first,
// but then a serious error would have been thrown.
extern char *input_block_getcopy(void);

// append optional '.'/'@' prefix to GlobalDynaBuf, then keep
// appending while characters are legal for keywords.
// throw "missing string" error if none.
// return whether there was an error.
extern int input_append_symbol_name_to_global_dynabuf(void);

// FIXME - move these to "symbol.h" and remove dependency on "scope":
// read symbol name into GlobalDynaBuf, set scope,
// return whether there was an error (namely, "no string given").
extern int input_readscopeandsymbolname(scope_t *scope, boolean dotkluge);
#define input_read_scope_and_symbol_name(scope)	input_readscopeandsymbolname(scope, FALSE)
#define input_read_scope_and_symbol_name_KLUGED(scope)	input_readscopeandsymbolname(scope, TRUE)

// Clear dynamic buffer, then append to it until an illegal (for a keyword)
// character is read. Zero-terminate the string. Return its length (without
// terminator).
// Zero lengths will produce a "missing string" error.
extern int input_read_keyword(void);

// Clear dynamic buffer, then append to it until an illegal (for a keyword)
// character is read. Zero-terminate the string, then convert to lower case.
// Return its length (without terminator).
// Zero lengths will produce a "missing string" error.
extern int input_read_and_lower_keyword(void);

// try to read a file name for an input file.
// library access by using <...> quoting is allowed.
// flags for "library access" and "absolute path" will be set accordingly.
// The file name given in the assembler source code is converted from
// UNIX style to platform style.
// Returns nonzero on error. Filename in GlobalDynaBuf.
// Errors are handled and reported, but caller should call
// input_skip_remainder() then.
extern int input_read_input_filename(struct filespecflags *flags);

// try to read a file name for an output file ("!to" and "!sl" only).
// library access by using <...> quoting is forbidden.
// The file name given in the assembler source code is converted from
// UNIX style to platform style.
// Returns nonzero on error. Filename in GlobalDynaBuf.
// Errors are handled and reported, but caller should call
// input_skip_remainder() then.
// FIXME - the name suggests this fn reads "the" output filename, but it only
// reads "an" output filename: either symbollist or the real output file.
extern int input_read_output_filename(void);

// Try to read a comma, skipping spaces before and after. Return TRUE if comma
// found, otherwise FALSE.
extern int input_accept_comma(void);

// Try to read given character.
// If found, eat character and return TRUE.
// If not found, throw syntax error and return FALSE.
extern int input_expect(int chr);

// read optional info about parameter length
// FIXME - move to different file!
extern bits input_get_force_bit(void);


// "include path" stuff:

// add entry
extern void includepaths_add(const char *path);

// open file for reading
// "flags" decide whether library access, search paths or absolute path is wanted.
// file name is expected in GlobalDynaBuf, in platform style and terminated.
// returns NULL or open stream
// on success, GlobalDynaBuf contains full file name in platform style
extern FILE *includepaths_open_ro(struct filespecflags *flags);


#endif
