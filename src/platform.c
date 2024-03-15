// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2024 Marco Baye
// Have a look at "acme.c" for further info
//
// Platform specific stuff
#include "platform.h"


// Amiga
#ifdef _AMIGA
#include "_amiga.c"
#endif

// DOS, OS/2 and Windows
#if defined(__DJGPP__) || defined(__OS2__) || defined(__Windows__) || defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(_WIN64) || defined(__MINGW32__) || defined(__MINGW64__)
#include "_dos.c"
#endif

// RISC OS
#ifdef __riscos__
#include "_riscos.c"
#endif

// add further platform files here

// Unix/Linux/others (also works on newer versions of Windows)
#ifndef platform_C
#include "_std.c"
#endif


// stuff shared by some, but not all platforms:
#if PLATFORM_NEEDS_ENV_VAR

#include <stdlib.h>	// for getenv()
#include "dynabuf.h"

// path of library tree, taken from env var
char	*platform_lib_prefix	= NULL;

// function to setup pointer above
void platform_read_env_var(void)
{
	char	*env_var;

	// Find out the path of ACME's library
	env_var = getenv("ACME");
	// if environment variable was found, make a copy
	if (env_var) {
		dynabuf_clear(GlobalDynaBuf);
		// copy environment variable to global dynamic buffer
		dynabuf_add_string(GlobalDynaBuf, env_var);
		dynabuf_append(GlobalDynaBuf, '\0');	// add terminator
		platform_lib_prefix = dynabuf_get_copy(GlobalDynaBuf);
	}
}

#endif
