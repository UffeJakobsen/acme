// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2024 Marco Baye
// Have a look at "acme.c" for further info
//
// Platform specific stuff (in this case, for unknown OSes)
#ifndef platform_C
#define platform_C


// convert UNIX-style path name to local platform-style path name:
void platform_convert_path(boolean *is_absolute, char *p)
{
	*is_absolute = (*p == '/');
	// ...no need to really "convert" anything...
}


#endif
