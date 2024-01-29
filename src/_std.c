// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2024 Marco Baye
// Have a look at "acme.c" for further info
//
// Platform specific stuff (in this case, for unknown OSes)
#ifndef platform_C
#define platform_C


#include <stdlib.h>
#include "dynabuf.h"


// variables
char	*AnyOS_lib_prefix	= NULL;	// header string of library tree


// used as PLATFORM_INIT: reads "ACME" environment variable
void AnyOS_entry(void)
{
	char	*env_var;

	// Find out the path of ACME's library
	env_var = getenv("ACME");
	// if environment variable was found, make a copy
	if (env_var) {
		dynabuf_clear(GlobalDynaBuf);
		// copy environment variable to global dynamic buffer
		dynabuf_add_string(GlobalDynaBuf, env_var);
		dynabuf_append(GlobalDynaBuf, '/');	// add dir separator
		dynabuf_append(GlobalDynaBuf, '\0');	// add terminator
		AnyOS_lib_prefix = dynabuf_get_copy(GlobalDynaBuf);
	}
}


#endif
