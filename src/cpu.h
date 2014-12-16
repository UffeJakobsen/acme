// ACME - a crossassembler for producing 6502/65c02/65816 code.
// Copyright (C) 1998-2014 Marco Baye
// Have a look at "acme.c" for further info
//
// CPU type stuff
#ifndef cpu_type_H
#define cpu_type_H


#include "config.h"


// CPU type structure definition
struct cpu_type {
	// This function is not allowed to change GlobalDynaBuf
	// because that's where the mnemonic is stored!
	int	(*keyword_is_mnemonic)(int);
	int	flags;	// see below for bit meanings
	char	default_align_value;
};
#define	CPUFLAG_INDIRECTJMPBUGGY	(1u << 0)	// warn if "jmp ($xxff)" is assembled
#define CPUFLAG_SUPPORTSLONGREGS	(1u << 1)	// allow "!al" and "!rl" pseudo opcodes
#define CPUFLAG_8B_AND_AB_NEED_0_ARG	(1u << 2)	// warn if "ane/lxa #$xx" uses non-zero arg


// if cpu type and value match, set register length variable to value.
// if cpu type and value don't match, complain instead.
extern void vcpu_check_and_set_reg_length(int *var, int make_long);
// set default value for pass
extern void cputype_passinit(const struct cpu_type *cpu_type);
// lookup cpu type held in DynaBuf and return its struct pointer (or NULL on failure)
extern const struct cpu_type *cputype_find(void);


#endif
