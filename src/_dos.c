// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2024 Marco Baye
// Have a look at "acme.c" for further info
//
// Platform specific stuff (in this case, for DOS, OS/2 and Windows)
#ifndef platform_C
#define platform_C


// convert UNIX-style pathname to DOS-style pathname
void DOS_convert_path(char *p)
{
	while (*p) {
		if (*p == '/') {
			*p = '\\';
		} else if (*p == '\\') {
			*p = '/';
		}
		++p;
	}
}


#endif
