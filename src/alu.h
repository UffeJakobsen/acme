// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2020 Marco Baye
// Have a look at "acme.c" for further info
//
// ALU stuff (the expression parser)
#ifndef alu_H
#define alu_H


#include "config.h"


// types
/*
enum expression_type {
	EXTY_EMPTY,	// next char after space was comma or end-of-statement
	EXTY_NUMBER,	// int or float (what is returned by the current functions)
	EXTY_STRING,	// TODO
	EXTY_REGISTER,	// reserved cpu constant (register names), TODO
	EXTY_LIST	// TODO
};
*/
struct expression {
	//enum expression_type	type;
	struct number		number;
	boolean			is_empty;		// nothing parsed (first character was a delimiter)	FIXME - make into its own result type!
	int			open_parentheses;	// number of parentheses still open
	boolean			is_parenthesized;	// whole expression was in parentheses (indicating indirect addressing)
};


// constants

// flag bits in number struct:
#define NUMBER_IS_FLOAT		(1u << 6)	// floating point value
#define NUMBER_EVER_UNDEFINED	(1u << 5)	// value once was related to undefined
// expression. Needed for producing the same addresses in all passes; because in
// the first pass there will almost for sure be labels that are undefined, you
// can't simply get the addressing mode from looking at the parameter's value.
#define NUMBER_IS_DEFINED	(1u << 4)	// 0: undefined expression (value will be zero). 1: known result
#define NUMBER_FITS_BYTE	(1u << 3)	// value is guaranteed to fit in one byte
#define NUMBER_FORCES_24	(1u << 2)	// value usage forces 24-bit usage
#define NUMBER_FORCES_16	(1u << 1)	// value usage forces 16-bit usage
#define NUMBER_FORCES_8		(1u << 0)	// value usage forces 8-bit usage
#define NUMBER_FORCEBITS	(NUMBER_FORCES_8 | NUMBER_FORCES_16 | NUMBER_FORCES_24)


// create dynamic buffer, operator/function trees and operator/operand stacks
extern void ALU_init(void);


// FIXME - replace all the functions below with a single one using a "flags" arg!
/* its return value would then be "error"/"ok".
// input flags:
#define ACCEPT_EMPTY		(1u << 0)	// if not given, throws error
#define ACCEPT_UNDEFINED	(1u << 1)	// if not given, undefined throws serious error
//#define ACCEPT_INT		(1u << )	needed when strings come along!
#define ACCEPT_FLOAT		(1u << 2)	// if not given, floats are converted to integer
#define ACCEPT_OPENPARENTHESIS	(1u << 3)	// if not given, throws syntax error
//#define ACCEPT_STRING
// do I need ACCEPT_INT and/or ACCEPT_ADDRESS?
*/

// stores int value if given. Returns whether stored. Throws error if undefined.
extern boolean ALU_optional_defined_int(intval_t *target);
// returns int value (0 if result was undefined)
extern intval_t ALU_any_int(void);
// stores int value and flags (floats are transformed to int)
extern void ALU_int_result(struct number *intresult);
// stores int value and flags (floats are transformed to int)
// if result was undefined, serious error is thrown
extern void ALU_defined_int(struct number *intresult);
// stores int value and flags, allowing for one '(' too many (x-indirect addr).
extern void ALU_liberal_int(struct expression *expression);
// stores value and flags (result may be either int or float)
extern void ALU_any_result(struct number *result);


#endif
