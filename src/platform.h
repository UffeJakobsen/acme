// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2024 Marco Baye
// Have a look at "acme.c" for further info
//
// Platform specific stuff


// Amiga
#ifdef _AMIGA
#define PLATFORM_VERSION	"Ported to AmigaOS by Christoph Mammitzsch."
#include "_amiga.h"
#endif

// DOS, OS/2 and Windows
#if defined(__DJGPP__) || defined(__OS2__) || defined(__Windows__) || defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(_WIN64) || defined(__MINGW32__) || defined(__MINGW64__)
#define PLATFORM_VERSION	"DOS/OS2/Win32/Win64 version. Compiled by Dirk Hoepfner"
#include "_dos.h"
#endif

// RISC OS
#ifdef __riscos__
#define PLATFORM_VERSION	"RISC OS version."
#include "_riscos.h"
#endif

// add further platform files here

// Unix/Linux/others (also works on newer versions of Windows)
#ifndef PLATFORM_VERSION
#define PLATFORM_VERSION	"Platform independent version."
#include "_std.h"
#endif


// stuff shared by some, but not all platforms:
#if PLATFORM_NEEDS_ENV_VAR

// path to library tree, taken from env var:
extern char	*platform_lib_prefix;

// function to setup pointer above
extern void platform_read_env_var(void);

#endif
