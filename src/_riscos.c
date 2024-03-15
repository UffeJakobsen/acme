// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2024 Marco Baye
// Have a look at "acme.c" for further info
//
// Platform specific stuff (in this case, for RISC OS)
#ifndef platform_C
#define platform_C


#include <stdlib.h>
#include <kernel.h>
#include "input.h"	// for input_now->location.plat_filename


// constants

// SWIs
#define  OS_FILE			0x00008
#define XDDEUTILS_THROWBACKSTART	0x62587
#define XDDEUTILS_THROWBACKSEND		0x62588
#define XDDEUTILS_THROWBACKEND		0x62589


// variables
bits	RISCOS_flags	= 0;	// used to store platform-specific flags


// exit handler: if throwback was used, de-register now
void RISCOS_exit(void)
{
	_kernel_swi_regs	regs;

	if (RISCOS_flags & RISCOSFLAG_THROWN) {
		_kernel_swi(XDDEUTILS_THROWBACKEND, &regs, &regs);
		RISCOS_flags &= ~RISCOSFLAG_THROWN;
	}
}


// used as PLATFORM_INIT: registers exit handler
void RISCOS_entry(void)
{
	atexit(RISCOS_exit);
}


// convert UNIX-style path name to RISC OS-style path name:
// "path/to/file.ext" -> "path.to.file/ext"
// "../../twolevelsup" -> "^.^.twolevelsup"
void platform_convert_path(boolean *is_absolute, char *readptr)
{
	char	*writeptr,
		previous;

	// init
	*is_absolute = FALSE;	// there are two ways for this to become true
	writeptr = readptr;	// good thing the string can only shrink, but not grow
	previous = '/';	// make sure ".." substitution also works for the first component
	// check for leading '/'
/* FIXME - this cannot work because "$." is longer than "/"
(there is a bogus workaround further down)
	if (*readptr == '/') {
		*is_absolute = TRUE;
		++readptr;
		*(writeptr++) = '$';
		*(writeptr++) = '.';
	}
*/
	// now scan remaining characters and convert all "../" components to "^."
	while (*readptr) {
		if ((previous == '/')
		&& (*readptr == '.')
		&& (readptr[1] == '.')
		&& (readptr[2] == '/')) {
			readptr += 2;
			*(writeptr++) = '^';
		}
		previous = *readptr;	// remember for next ".." check
		if (*readptr == ':') {
			// prefixes like "myproject:" mean the path is absolute
			*is_absolute = TRUE;
		} else if (*readptr == '$') {
			// bogus workaround: user needs to use "$/whatever" for root dir
			*is_absolute = TRUE;
		}
		// convert characters
		if (*readptr == '.') {
			*writeptr = '/';
		} else if (*readptr == '/') {
			*writeptr = '.';
		} else if (*readptr == '?') {
			*writeptr = '#';
		} else if (*readptr == '#') {
			*writeptr = '?';
		} else {
			*writeptr = *readptr;	// copy character
		}
		++readptr;
		++writeptr;
	}
	*writeptr = *readptr;	// copy terminator
}


// setting the created files' types
void RISCOS_set_filetype(const char *filename, int file_type)
{
	_kernel_swi_regs	regs;

	regs.r[0] = 18;	// reason code (set file type)
	regs.r[1] = (int) filename;
	regs.r[2] = file_type;
	_kernel_swi(OS_FILE, &regs, &regs);
}


// throwback protocol: "type" can be 0, 1 or 2 (DDEUtils message types)
void RISCOS_throwback(const char *message, int type)
{
	_kernel_swi_regs	regs;

	// only use throwback protocol if wanted
	if ((RISCOS_flags & RISCOSFLAG_THROWBACK) == 0)
		return;

	// if this is the first throwback, set it up and send info
	if ((RISCOS_flags & RISCOSFLAG_THROWN) == 0) {
		RISCOS_flags |= RISCOSFLAG_THROWN;
		_kernel_swi(XDDEUTILS_THROWBACKSTART, &regs, &regs);
		regs.r[0] = 0;
		regs.r[1] = 0;
	//	regs.r[2] = (int) toplevel_source;
		regs.r[2] = (int) input_now->location.plat_filename;
		_kernel_swi(XDDEUTILS_THROWBACKSEND, &regs, &regs);
	}
	// send throwback message
	regs.r[0] = 1;
	regs.r[1] = 0;
	regs.r[2] = (int) input_now->location.plat_filename;
	regs.r[3] = input_now->location.line_number;
	regs.r[4] = type;
	regs.r[5] = (int) message;
	_kernel_swi(XDDEUTILS_THROWBACKSEND, &regs, &regs);
}


#endif
