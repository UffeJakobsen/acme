// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2024 Marco Baye
// Have a look at "acme.c" for further info
//
// Platform specific stuff (in this case, for DOS, OS/2 and Windows)
#ifndef platform_C
#define platform_C


// convert UNIX-style path name to DOS-style path name:
// "path/file" -> "path\file"
void platform_convert_path(boolean *is_absolute, char *p)
{
	// leading '/' means absolute path
	*is_absolute = (*p == '/');
	// stuff like "C:" also means absolute
	if ((*p) && (p[1] == ':'))
		*is_absolute = TRUE;
	// exchange forward and backward slashes
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
