// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2024 Marco Baye
// Have a look at "acme.c" for further info
//
// CPU type stuff
#ifndef cpu_type_H
#define cpu_type_H


#include "config.h"


// types

struct cpu_type {
	// This function is not allowed to change GlobalDynaBuf
	// because that's where the mnemonic is stored!
	boolean		(*keyword_is_mnemonic)(int);
	bits		flags;	// see below for bit meanings
//	int		pc_mask;	// last value before program counter wraps to zero
	int		dummy_pc;	// value to use for pc after complaining to user they did not set it
	unsigned char	default_align_value;
	//int		reserved_keywords_maxlen;	// TODO - add
	//int		(*reserved_keyword_check)(void);	// TODO - add
};
#define	CPUFLAG_INDIRECTJMPBUGGY	(1u << 0)	// warn if "jmp ($xxff)" is assembled
#define CPUFLAG_SUPPORTSLONGREGS	(1u << 1)	// allow "!al" and "!rl" pseudo opcodes
#define CPUFLAG_8B_AND_AB_NEED_0_ARG	(1u << 2)	// warn if "ane/lxa #$xx" uses non-zero arg
#define CPUFLAG_ISBIGENDIAN		(1u << 3)	// for 16/24/32-bit values, output msb first
#define CPUFLAG_DECIMALSUBTRACTBUGGY	(1u << 4)	// warn if "sed" is assembled
#define CPUFLAG_WARN_ABOUT_FF_PTR	(1u << 5)	// warn if MNEMO($ff) is assembled


// constants

extern const char	cputype_names[];	// string to show if cputype_find() returns NULL


// variables

extern const struct cpu_type	*cpu_current_type;
extern boolean			cpu_a_is_long;
extern boolean			cpu_xy_are_long;


// prototypes

// if cpu type and value match, set register length variable to value.
// if cpu type and value don't match, complain instead.
extern void vcpu_check_and_set_reg_length(boolean *var, boolean make_long);

// set default value for pass
extern void cputype_passinit(void);

// lookup cpu type held in DynaBuf and return its struct pointer (or NULL on failure)
extern const struct cpu_type *cputype_find(void);


#endif
