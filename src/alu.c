// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2020 Marco Baye
// Have a look at "acme.c" for further info
//
// Arithmetic/logic unit
// 11 Oct 2006	Improved float reading in parse_decimal_value()
// 24 Nov 2007	Now accepts floats starting with decimal point
// 31 Jul 2009	Changed ASR again, just to be on the safe side.
// 14 Jan 2014	Changed associativity of "power-of" operator,
//		so a^b^c now means a^(b^c).
//  7 May 2014	C-style "==" operators are now recognized (but
//		give a warning).
// 31 May 2014	Added "0b" binary number prefix as alternative to "%".
// 28 Apr 2015	Added symbol name output to "value not defined" error.
//  1 Feb 2019	Prepared to make "honor leading zeroes" optionally (now done)
// FIXME - the words "operand"/"operator"/"operation" are too similar, so:
//	use "argument" instead of "operand"
//	use <TODO> instead of operator/operation
#include "alu.h"
#include <stdlib.h>
#include <math.h>	// only for fp support
#include "platform.h"
#include "dynabuf.h"
#include "encoding.h"
#include "global.h"
#include "input.h"
#include "output.h"
#include "section.h"
#include "symbol.h"
#include "tree.h"


// constants

#define ERRORMSG_DYNABUF_INITIALSIZE	256	// ad hoc
#define FUNCTION_DYNABUF_INITIALSIZE	8	// enough for "arctan"
#define HALF_INITIAL_STACK_SIZE	8
static const char	exception_div_by_zero[]	= "Division by zero.";
static const char	exception_no_value[]	= "No value given.";
static const char	exception_paren_open[]	= "Too many '('.";
#define s_or	(s_eor + 1)	// Yes, I know I'm sick
#define s_xor	(s_scrxor + 3)	// Yes, I know I'm sick
static const char	s_arcsin[]	= "arcsin";
#define s_sin	(s_arcsin + 3)	// Yes, I know I'm sick
static const char	s_arccos[]	= "arccos";
#define s_cos	(s_arccos + 3)	// Yes, I know I'm sick
static const char	s_arctan[]	= "arctan";
#define s_tan	(s_arctan + 3)	// Yes, I know I'm sick

enum op_group {
	OPGROUP_SPECIAL,	// start/end of expression, and parentheses
	OPGROUP_MONADIC,	// {result} = {op} {arg}
	OPGROUP_DYADIC		// {result} = {arg1} {op} {arg2}
};
enum op_handle {
	// special (pseudo) operators:
	OPHANDLE_EXPRSTART,	//		"start of expression"
	OPHANDLE_EXPREND,	//		"end of expression"
	OPHANDLE_OPENING,	//	(v	'(', starts subexpression (handled like monadic)
	OPHANDLE_CLOSING,	//	v)	')', ends subexpression (handled like dyadic)
	// monadic operators (including functions):
	OPHANDLE_NOT,		//	!v	NOT v	bit-wise NOT
	OPHANDLE_NEGATE,	//	-v		Negate
	OPHANDLE_LOWBYTEOF,	//	<v		Lowbyte of
	OPHANDLE_HIGHBYTEOF,	//	>v		Highbyte of
	OPHANDLE_BANKBYTEOF,	//	^v		Bankbyte of
	OPHANDLE_ADDR,		//	addr(v)
	OPHANDLE_INT,		//	int(v)
	OPHANDLE_FLOAT,		//	float(v)
	OPHANDLE_SIN,		//	sin(v)
	OPHANDLE_COS,		//	cos(v)
	OPHANDLE_TAN,		//	tan(v)
	OPHANDLE_ARCSIN,	//	arcsin(v)
	OPHANDLE_ARCCOS,	//	arccos(v)
	OPHANDLE_ARCTAN,	//	arctan(v)
	// dyadic operators:
	OPHANDLE_POWEROF,	//	v^w
	OPHANDLE_MULTIPLY,	//	v*w
	OPHANDLE_DIVIDE,	//	v/w		(Integer) Division
	OPHANDLE_INTDIV,	//	v/w	v DIV w	Integer Division
	OPHANDLE_MODULO,	//	v%w	v MOD w	Remainder
	OPHANDLE_SL,		//	v<<w	v ASL w	v LSL w	Shift left
	OPHANDLE_ASR,		//	v>>w	v ASR w	Arithmetic shift right
	OPHANDLE_LSR,		//	v>>>w	v LSR w	Logical shift right
	OPHANDLE_ADD,		//	v+w
	OPHANDLE_SUBTRACT,	//	v-w
	OPHANDLE_EQUALS,	//	v=w
	OPHANDLE_LE,		//	v<=w
	OPHANDLE_LESSTHAN,	//	v< w
	OPHANDLE_GE,		//	v>=w
	OPHANDLE_GREATERTHAN,	//	v> w
	OPHANDLE_NOTEQUAL,	//	v!=w	v<>w	v><w
	OPHANDLE_AND,		//	v&w		v AND w
	OPHANDLE_OR,		//	v|w		v OR w
	OPHANDLE_EOR,		//	v EOR w		v XOR w		(FIXME:remove)
	OPHANDLE_XOR,		//	v XOR w
};
struct operator {
	int		priority;	// lsb holds "is_right_associative" info!
#define IS_RIGHT_ASSOCIATIVE(prio)	((prio) & 1)
	enum op_group	group;
	enum op_handle	handle;
};
static struct operator ops_exprend	= {0, OPGROUP_SPECIAL, OPHANDLE_EXPREND	};
static struct operator ops_exprstart	= {2, OPGROUP_SPECIAL, OPHANDLE_EXPRSTART	};
static struct operator ops_closing	= {4, OPGROUP_SPECIAL, OPHANDLE_CLOSING	};
static struct operator ops_opening	= {6, OPGROUP_SPECIAL, OPHANDLE_OPENING	};
static struct operator ops_or		= {8, OPGROUP_DYADIC, OPHANDLE_OR	};
static struct operator ops_eor		= {10, OPGROUP_DYADIC, OPHANDLE_EOR	};	// FIXME:remove
static struct operator ops_xor		= {10, OPGROUP_DYADIC, OPHANDLE_XOR	};
static struct operator ops_and		= {12, OPGROUP_DYADIC, OPHANDLE_AND	};
static struct operator ops_equals	= {14, OPGROUP_DYADIC, OPHANDLE_EQUALS	};
static struct operator ops_notequal	= {16, OPGROUP_DYADIC, OPHANDLE_NOTEQUAL	};
	// same priority for all comparison operators
static struct operator ops_le		= {18, OPGROUP_DYADIC, OPHANDLE_LE	};
static struct operator ops_lessthan	= {18, OPGROUP_DYADIC, OPHANDLE_LESSTHAN	};
static struct operator ops_ge		= {18, OPGROUP_DYADIC, OPHANDLE_GE	};
static struct operator ops_greaterthan	= {18, OPGROUP_DYADIC, OPHANDLE_GREATERTHAN	};
	// same priority for all byte extraction operators
static struct operator ops_lowbyteof	= {20, OPGROUP_MONADIC, OPHANDLE_LOWBYTEOF	};
static struct operator ops_highbyteof	= {20, OPGROUP_MONADIC, OPHANDLE_HIGHBYTEOF	};
static struct operator ops_bankbyteof	= {20, OPGROUP_MONADIC, OPHANDLE_BANKBYTEOF	};
	// same priority for all shift operators (left-associative, though they could be argued to be made right-associative :))
static struct operator ops_sl		= {22, OPGROUP_DYADIC, OPHANDLE_SL	};
static struct operator ops_asr		= {22, OPGROUP_DYADIC, OPHANDLE_ASR	};
static struct operator ops_lsr		= {22, OPGROUP_DYADIC, OPHANDLE_LSR	};
	// same priority for "+" and "-"
static struct operator ops_add		= {24, OPGROUP_DYADIC, OPHANDLE_ADD	};
static struct operator ops_subtract	= {24, OPGROUP_DYADIC, OPHANDLE_SUBTRACT	};
	// same priority for "*", "/" and "%"
static struct operator ops_multiply	= {26, OPGROUP_DYADIC, OPHANDLE_MULTIPLY	};
static struct operator ops_divide	= {26, OPGROUP_DYADIC, OPHANDLE_DIVIDE	};
static struct operator ops_intdiv	= {26, OPGROUP_DYADIC, OPHANDLE_INTDIV	};
static struct operator ops_modulo	= {26, OPGROUP_DYADIC, OPHANDLE_MODULO	};
	// highest "real" priorities
static struct operator ops_negate	= {28, OPGROUP_MONADIC, OPHANDLE_NEGATE	};
static struct operator ops_powerof	= {29, OPGROUP_DYADIC, OPHANDLE_POWEROF	};	// right-associative!
static struct operator ops_not		= {30, OPGROUP_MONADIC, OPHANDLE_NOT	};
	// function calls act as if they were monadic operators.
	// they need high priorities to make sure they are evaluated once the
	// parentheses' content is known:
	// "sin(3 + 4) DYADIC_OPERATOR 5" becomes "sin 7 DYADIC_OPERATOR 5",
	// so function calls' priority must be higher than all dyadic operators.
static struct operator ops_addr		= {32, OPGROUP_MONADIC, OPHANDLE_ADDR	};
static struct operator ops_int		= {32, OPGROUP_MONADIC, OPHANDLE_INT	};
static struct operator ops_float	= {32, OPGROUP_MONADIC, OPHANDLE_FLOAT	};
static struct operator ops_sin		= {32, OPGROUP_MONADIC, OPHANDLE_SIN	};
static struct operator ops_cos		= {32, OPGROUP_MONADIC, OPHANDLE_COS	};
static struct operator ops_tan		= {32, OPGROUP_MONADIC, OPHANDLE_TAN	};
static struct operator ops_arcsin	= {32, OPGROUP_MONADIC, OPHANDLE_ARCSIN	};
static struct operator ops_arccos	= {32, OPGROUP_MONADIC, OPHANDLE_ARCCOS	};
static struct operator ops_arctan	= {32, OPGROUP_MONADIC, OPHANDLE_ARCTAN	};


// variables
static struct dynabuf	*errormsg_dyna_buf;	// dynamic buffer for "value not defined" error
static struct dynabuf	*function_dyna_buf;	// dynamic buffer for fn names
static struct operator	**operator_stack	= NULL;
static int		operator_stk_size	= HALF_INITIAL_STACK_SIZE;
static int		operator_sp;		// operator stack pointer
static struct number	*operand_stack		= NULL;	// flags and value
static int		operand_stk_size	= HALF_INITIAL_STACK_SIZE;
static int		operand_sp;		// value stack pointer
enum alu_state {
	STATE_EXPECT_OPERAND_OR_MONADIC_OPERATOR,
	STATE_EXPECT_DYADIC_OPERATOR,
	STATE_TRY_TO_REDUCE_STACKS,
	STATE_MAX_GO_ON,	// "border value" to find the stoppers:
	STATE_ERROR,		// error has occured
	STATE_END		// standard end
};
static enum alu_state	alu_state;	// deterministic finite automaton
// predefined stuff
static struct ronode	*operator_tree	= NULL;	// tree to hold operators
static struct ronode	operator_list[]	= {
	PREDEFNODE(s_asr,	&ops_asr),
	PREDEFNODE(s_lsr,	&ops_lsr),
	PREDEFNODE(s_asl,	&ops_sl),
	PREDEFNODE("lsl",	&ops_sl),
	PREDEFNODE("div",	&ops_intdiv),
	PREDEFNODE("mod",	&ops_modulo),
	PREDEFNODE(s_and,	&ops_and),
	PREDEFNODE(s_or,	&ops_or),
	PREDEFNODE(s_eor,	&ops_eor),		// (FIXME:remove)
	PREDEFLAST(s_xor,	&ops_xor),
	//    ^^^^ this marks the last element
};
static struct ronode	*function_tree	= NULL;	// tree to hold functions
static struct ronode	function_list[]	= {
	PREDEFNODE("addr",	&ops_addr),
	PREDEFNODE("address",	&ops_addr),
	PREDEFNODE("int",	&ops_int),
	PREDEFNODE("float",	&ops_float),
	PREDEFNODE(s_arcsin,	&ops_arcsin),
	PREDEFNODE(s_arccos,	&ops_arccos),
	PREDEFNODE(s_arctan,	&ops_arctan),
	PREDEFNODE(s_sin,	&ops_sin),
	PREDEFNODE(s_cos,	&ops_cos),
	PREDEFLAST(s_tan,	&ops_tan),
	//    ^^^^ this marks the last element
};

#define PUSH_OPERATOR(x)	operator_stack[operator_sp++] = (x)

#define PUSH_INTOPERAND(i, f, r)			\
do {							\
	operand_stack[operand_sp].flags = (f);		\
	operand_stack[operand_sp].val.intval = (i);	\
	operand_stack[operand_sp++].addr_refs = (r);	\
} while (0)
#define PUSH_FPOPERAND(fp, f)						\
do {									\
	operand_stack[operand_sp].flags = (f) | NUMBER_IS_FLOAT;	\
	operand_stack[operand_sp].val.fpval = (fp);			\
	operand_stack[operand_sp++].addr_refs = 0;			\
} while (0)


// enlarge operator stack
static void enlarge_operator_stack(void)
{
	operator_stk_size *= 2;
	operator_stack = realloc(operator_stack, operator_stk_size * sizeof(*operator_stack));
	if (operator_stack == NULL)
		Throw_serious_error(exception_no_memory_left);
}


// enlarge operand stack
static void enlarge_operand_stack(void)
{
	operand_stk_size *= 2;
	operand_stack = realloc(operand_stack, operand_stk_size * sizeof(*operand_stack));
	if (operand_stack == NULL)
		Throw_serious_error(exception_no_memory_left);
}


// create dynamic buffer, operator/function trees and operator/operand stacks
void ALU_init(void)
{
	errormsg_dyna_buf = DynaBuf_create(ERRORMSG_DYNABUF_INITIALSIZE);
	function_dyna_buf = DynaBuf_create(FUNCTION_DYNABUF_INITIALSIZE);
	Tree_add_table(&operator_tree, operator_list);
	Tree_add_table(&function_tree, function_list);
	enlarge_operator_stack();
	enlarge_operand_stack();
}


// not-so-braindead algorithm for calculating "to the power of" function for
// integer operands.
// my_pow(whatever, 0) returns 1. my_pow(0, whatever_but_zero) returns 0.
static intval_t my_pow(intval_t mantissa, intval_t exponent)
{
	intval_t	result	= 1;

	while (exponent) {
		// handle exponent's lowmost bit
		if (exponent & 1)
			result *= mantissa;
		// square the mantissa, halve the exponent
		mantissa *= mantissa;
		exponent >>= 1;
	}
	return result;
}


// arithmetic shift right (works even if C compiler does not support it)
static intval_t my_asr(intval_t left, intval_t right)
{
	// if first operand is positive or zero, ASR and LSR are equivalent,
	// so just do it and return the result:
	if (left >= 0)
		return left >> right;

	// However, if the first operand is negative, the result is
	// implementation-defined: While most compilers will do ASR, some others
	// might do LSR instead, and *theoretically*, it is even possible for a
	// compiler to define silly stuff like "shifting a negative value to the
	// right will always return -1".
	// Therefore, in case of a negative operand, we'll use this quick and
	// simple workaround:
	return ~((~left) >> right);
}


// if needed, throw "Value not defined" error
// This function is not allowed to change DynaBuf because the symbol's name
// might be stored there!
static void check_for_def(struct symbol *optional_symbol, int flags, char optional_prefix_char, char *name, size_t length)
{
	if (flags & NUMBER_IS_DEFINED)
		return;

	if (!pass.complain_about_undefined)
		return;

	// only complain once per symbol
	if (optional_symbol) {
		if (optional_symbol->has_been_reported)
			return;

		optional_symbol->has_been_reported = TRUE;
	}

	DYNABUF_CLEAR(errormsg_dyna_buf);
	DynaBuf_add_string(errormsg_dyna_buf, "Value not defined (");
	length += errormsg_dyna_buf->size;

	if (optional_prefix_char) {
		DynaBuf_append(errormsg_dyna_buf, optional_prefix_char);
		++length;
	}
	DynaBuf_add_string(errormsg_dyna_buf, name);
	if (errormsg_dyna_buf->size < length) {
		Bug_found("Illegal symbol name length", errormsg_dyna_buf->size - length);
	} else {
		errormsg_dyna_buf->size = length;
	}
	DynaBuf_add_string(errormsg_dyna_buf, ").");
	DynaBuf_append(errormsg_dyna_buf, '\0');
	Throw_error(errormsg_dyna_buf->buffer);
}


// Lookup (and create, if necessary) symbol tree item and return its value.
// DynaBuf holds the symbol's name and "scope" its scope.
// The name length must be given explicitly because of anonymous forward labels;
// their internal name is different (longer) than their displayed name.
// This function is not allowed to change DynaBuf because that's where the
// symbol name is stored!
static void get_symbol_value(scope_t scope, char optional_prefix_char, size_t name_length)
{
	struct symbol	*symbol;

	// if the symbol gets created now, mark it as unsure
	symbol = symbol_find(scope, NUMBER_EVER_UNDEFINED);
	// if needed, output "value not defined" error
	check_for_def(symbol, symbol->result.flags, optional_prefix_char, GLOBALDYNABUF_CURRENT, name_length);
	// in first pass, count usage
	if (FIRST_PASS)
		symbol->usage++;
	// push operand, regardless of whether int or float
	operand_stack[operand_sp++] = symbol->result;
}


// Parse program counter ('*')
static void parse_program_counter(void)	// Now GotByte = "*"
{
	struct number	pc;

	GetByte();
	vcpu_read_pc(&pc);
	// if needed, output "value not defined" error
	check_for_def(NULL, pc.flags, 0, "*", 1);
	PUSH_INTOPERAND(pc.val.intval, pc.flags, pc.addr_refs);
}


// Parse quoted character.
// The character will be converted using the current encoding.
static void parse_quoted_character(char closing_quote)
{
	intval_t	value;

	// read character to parse - make sure not at end of statement
	if (GetQuotedByte() == CHAR_EOS)
		return;

	// on empty string, complain
	if (GotByte == closing_quote) {
		Throw_error(exception_missing_string);
		alu_state = STATE_ERROR;
		return;
	}

	// parse character
	value = (intval_t) encoding_encode_char(GotByte);
	// Read closing quote (hopefully)
	if (GetQuotedByte() == closing_quote) {
		GetByte();	// if length == 1, proceed with next byte
	} else {
		if (GotByte) {
			// if longer than one character
			Throw_error("There's more than one character.");
			alu_state = STATE_ERROR;
		}
	}
	PUSH_INTOPERAND(value, NUMBER_IS_DEFINED | NUMBER_FITS_BYTE, 0);
	// Now GotByte = char following closing quote (or CHAR_EOS on error)
}


// Parse binary value. Apart from '0' and '1', it also accepts the characters
// '.' and '#', this is much more readable. The current value is stored as soon
// as a character is read that is none of those given above.
static void parse_binary_value(void)	// Now GotByte = "%" or "b"
{
	intval_t	value	= 0;
	int		flags	= NUMBER_IS_DEFINED,
			digits	= -1;	// digit counter

	for (;;) {
		++digits;
		switch (GetByte()) {
		case '0':
		case '.':
			value <<= 1;
			continue;
		case '1':
		case '#':
			value = (value << 1) | 1;
			continue;
		}
		break;	// found illegal character
	}
	// set force bits
	if (config.honor_leading_zeroes) {
		if (digits > 8) {
			if (digits > 16) {
				if (value < 65536)
					flags |= NUMBER_FORCES_24;
			} else {
				if (value < 256)
					flags |= NUMBER_FORCES_16;
			}
		}
	}
	PUSH_INTOPERAND(value, flags, 0);
	// Now GotByte = non-binary char
}


// Parse hexadecimal value. It accepts "0" to "9", "a" to "f" and "A" to "F".
// The current value is stored as soon as a character is read that is none of
// those given above.
static void parse_hexadecimal_value(void)	// Now GotByte = "$" or "x"
{
	char		byte;
	int		digits	= -1,	// digit counter
			flags	= NUMBER_IS_DEFINED;
	intval_t	value	= 0;

	for (;;) {
		++digits;
		byte = GetByte();
		// if digit or legal character, add value
		if ((byte >= '0') && (byte <= '9')) {
			value = (value << 4) + (byte - '0');
			continue;
		}
		if ((byte >= 'a') && (byte <= 'f')) {
			value = (value << 4) + (byte - 'a') + 10;
			continue;
		}
		if ((byte >= 'A') && (byte <= 'F')) {
			value = (value << 4) + (byte - 'A') + 10;
			continue;
		}
		break;	// found illegal character
	}
	// set force bits
	if (config.honor_leading_zeroes) {
		if (digits > 2) {
			if (digits > 4) {
				if (value < 65536)
					flags |= NUMBER_FORCES_24;
			} else {
				if (value < 256)
					flags |= NUMBER_FORCES_16;
			}
		}
	}
	PUSH_INTOPERAND(value, flags, 0);
	// Now GotByte = non-hexadecimal char
}


// parse fractional part of a floating-point value
static void parse_frac_part(int integer_part)	// Now GotByte = first digit after decimal point
{
	double	denominator	= 1,
		fpval		= integer_part;

	// parse digits until no more
	while ((GotByte >= '0') && (GotByte <= '9')) {
		fpval = 10 * fpval + (GotByte & 15);	// this works. it's ASCII.
		denominator *= 10;
		GetByte();
	}
	// FIXME - add possibility to read 'e' and exponent!
	PUSH_FPOPERAND(fpval / denominator, NUMBER_IS_DEFINED);
}


// Parse a decimal value. As decimal values don't use any prefixes, this
// function expects the first digit to be read already.
// If the first two digits are "0x", this function branches to the one for
// parsing hexadecimal values.
// If the first two digits are "0b", this function branches to the one for
// parsing binary values.
// If a decimal point is read, this function branches to the one for parsing
// floating-point values.
// This function accepts '0' through '9' and one dot ('.') as the decimal
// point. The current value is stored as soon as a character is read that is
// none of those given above. Float usage is only activated when a decimal
// point has been found, so don't expect "100000000000000000000" to work.
// CAUTION: "100000000000000000000.0" won't work either, because when the
// decimal point gets parsed, the integer value will have overflown already.
static void parse_decimal_value(void)	// Now GotByte = first digit
{
	intval_t	intval	= (GotByte & 15);	// this works. it's ASCII.

	GetByte();
	// check for "0b" (binary) and "0x" (hexadecimal) prefixes
	if (intval == 0) {
		if (GotByte == 'b') {
			parse_binary_value();
			return;
		}
		if (GotByte == 'x') {
			parse_hexadecimal_value();
			return;
		}
	}
	// parse digits until no more
	while ((GotByte >= '0') && (GotByte <= '9')) {
		intval = 10 * intval + (GotByte & 15);	// ASCII, see above
		GetByte();
	}
	// check whether it's a float
	if (GotByte == '.') {
		// read fractional part
		GetByte();
		parse_frac_part(intval);
	} else {
		PUSH_INTOPERAND(intval, NUMBER_IS_DEFINED, 0);
	}
	// Now GotByte = non-decimal char
}


// Parse octal value. It accepts "0" to "7". The current value is stored as
// soon as a character is read that is none of those given above.
static void parse_octal_value(void)	// Now GotByte = "&"
{
	intval_t	value	= 0;
	int		flags	= NUMBER_IS_DEFINED,
			digits	= 0;	// digit counter

	GetByte();
	while ((GotByte >= '0') && (GotByte <= '7')) {
		value = (value << 3) + (GotByte & 7);	// this works. it's ASCII.
		++digits;
		GetByte();
	}
	// set force bits
	if (config.honor_leading_zeroes) {
		if (digits > 3) {
			if (digits > 6) {
				if (value < 65536)
					flags |= NUMBER_FORCES_24;
			} else {
				if (value < 256)
					flags |= NUMBER_FORCES_16;
			}
		}
	}
	PUSH_INTOPERAND(value, flags, 0);
	// Now GotByte = non-octal char
}


// Parse function call (sin(), cos(), arctan(), ...)
static void parse_function_call(void)
{
	void	*node_body;

	// make lower case version of name in local dynamic buffer
	DynaBuf_to_lower(function_dyna_buf, GlobalDynaBuf);
	// search for tree item
	if (Tree_easy_scan(function_tree, &node_body, function_dyna_buf)) {
		PUSH_OPERATOR((struct operator *) node_body);
	} else {
		Throw_error("Unknown function.");
		alu_state = STATE_ERROR;
	}
}


// Expect operand or monadic operator (hopefully inlined)
// returns TRUE if it ate any non-space (-> so expression isn't empty)
// returns FALSE if first non-space is delimiter (-> end of expression)
static int expect_operand_or_monadic_operator(void)
{
	struct operator	*operator;
	int		ugly_length_kluge;
	boolean		perform_negation;

	SKIPSPACE();
	switch (GotByte) {
	case '+':	// anonymous forward label
		// count plus signs to build name of anonymous label
		DYNABUF_CLEAR(GlobalDynaBuf);
		do
			DYNABUF_APPEND(GlobalDynaBuf, '+');
		while (GetByte() == '+');
		ugly_length_kluge = GlobalDynaBuf->size;	// FIXME - get rid of this!
		symbol_fix_forward_anon_name(FALSE);	// FALSE: do not increment counter
		get_symbol_value(section_now->local_scope, 0, ugly_length_kluge);
		goto now_expect_dyadic;

	case '-':	// NEGATION operator or anonymous backward label
		// count minus signs in case it's an anonymous backward label
		perform_negation = FALSE;
		DYNABUF_CLEAR(GlobalDynaBuf);
		do {
			DYNABUF_APPEND(GlobalDynaBuf, '-');
			perform_negation = !perform_negation;
		} while (GetByte() == '-');
		SKIPSPACE();
		if (BYTE_FOLLOWS_ANON(GotByte)) {
			DynaBuf_append(GlobalDynaBuf, '\0');
			get_symbol_value(section_now->local_scope, 0, GlobalDynaBuf->size - 1);	// -1 to not count terminator
			goto now_expect_dyadic;
		}

		if (perform_negation)
			PUSH_OPERATOR(&ops_negate);
		// State doesn't change
		break;
// Real monadic operators (state doesn't change, still ExpectMonadic)
	case '!':	// NOT operator
		operator = &ops_not;
		goto get_byte_and_push_monadic;

	case '<':	// LOWBYTE operator
		operator = &ops_lowbyteof;
		goto get_byte_and_push_monadic;

	case '>':	// HIGHBYTE operator
		operator = &ops_highbyteof;
		goto get_byte_and_push_monadic;

	case '^':	// BANKBYTE operator
		operator = &ops_bankbyteof;
		goto get_byte_and_push_monadic;

// Faked monadic operators
	case '(':	// left parenthesis
		operator = &ops_opening;
		goto get_byte_and_push_monadic;

	case ')':	// right parenthesis
		// this makes "()" also throw a syntax error
		Throw_error(exception_syntax);
		alu_state = STATE_ERROR;
		break;
// Operands (values, state changes to ExpectDyadic)
	case '"':	// Quoted character
	case '\'':	// Quoted character
		// Character will be converted using current encoding
		parse_quoted_character(GotByte);
		// Now GotByte = char following closing quote
		goto now_expect_dyadic;

	case '%':	// Binary value
		parse_binary_value();	// Now GotByte = non-binary char
		goto now_expect_dyadic;

	case '&':	// Octal value
		parse_octal_value();	// Now GotByte = non-octal char
		goto now_expect_dyadic;

	case '$':	// Hexadecimal value
		parse_hexadecimal_value();
		// Now GotByte = non-hexadecimal char
		goto now_expect_dyadic;

	case '*':	// Program counter
		parse_program_counter();
		// Now GotByte = char after closing quote
		goto now_expect_dyadic;

// FIXME - find a way to tell decimal point and LOCAL_PREFIX apart!
	case '.':	// local symbol or fractional part of float value
		GetByte();	// start after '.'
		// check for fractional part of float value
		if ((GotByte >= '0') && (GotByte <= '9')) {
			parse_frac_part(0);
			// Now GotByte = non-decimal char
			goto now_expect_dyadic;
		}

		if (Input_read_keyword()) {
			// Now GotByte = illegal char
			get_symbol_value(section_now->local_scope, LOCAL_PREFIX, GlobalDynaBuf->size - 1);	// -1 to not count terminator
			goto now_expect_dyadic;
		}

		// if we're here, Input_read_keyword() will have thrown an error (like "no string given"):
		alu_state = STATE_ERROR;
		break;
	case CHEAP_PREFIX:	// cheap local symbol
		//printf("looking in cheap scope %d\n", section_now->cheap_scope);
		GetByte();	// start after '@'
		if (Input_read_keyword()) {
			// Now GotByte = illegal char
			get_symbol_value(section_now->cheap_scope, CHEAP_PREFIX, GlobalDynaBuf->size - 1);	// -1 to not count terminator
			goto now_expect_dyadic;
		}

		// if we're here, Input_read_keyword() will have thrown an error (like "no string given"):
		alu_state = STATE_ERROR;
		break;
	// decimal values and global symbols
	default:	// all other characters
		if ((GotByte >= '0') && (GotByte <= '9')) {
			parse_decimal_value();
			// Now GotByte = non-decimal char
			goto now_expect_dyadic;
		}

		if (BYTE_STARTS_KEYWORD(GotByte)) {
			register int	length;

			// Read global label (or "NOT")
			length = Input_read_keyword();
			// Now GotByte = illegal char
			// Check for NOT. Okay, it's hardcoded,
			// but so what? Sue me...
			if ((length == 3)
			&& ((GlobalDynaBuf->buffer[0] | 32) == 'n')
			&& ((GlobalDynaBuf->buffer[1] | 32) == 'o')
			&& ((GlobalDynaBuf->buffer[2] | 32) == 't')) {
				PUSH_OPERATOR(&ops_not);
				// state doesn't change
			} else {
				if (GotByte == '(') {
					parse_function_call();
// i thought about making the parentheses optional, so you can write "a = sin b"
// just like "a = not b". but then each new function name would have to be made
// a reserved keyword, otherwise stuff like "a = sin * < b" would be ambiguous:
// it could mean either "compare sine of PC to b" or "multiply 'sin' by low byte
// of b".
// however, apart from that check above, function calls have nothing to do with
// parentheses: "sin(x+y)" gets parsed just like "not(x+y)".
				} else {
					get_symbol_value(SCOPE_GLOBAL, 0, GlobalDynaBuf->size - 1);	// -1 to not count terminator
					goto now_expect_dyadic;
				}
			}
		} else {
			// illegal character read - so don't go on
			// we found end-of-expression instead of an operand,
			// that's either an empty expression or an erroneous one!
			PUSH_INTOPERAND(0, 0, 0);	// push dummy operand so stack is ok
			if (operator_stack[operator_sp - 1] == &ops_exprstart) {
				PUSH_OPERATOR(&ops_exprend);
				alu_state = STATE_TRY_TO_REDUCE_STACKS;
			} else {
				Throw_error(exception_syntax);
				alu_state = STATE_ERROR;
			}
			return FALSE;	// found delimiter
		}
		break;

// no other possibilities, so here are the shared endings

get_byte_and_push_monadic:
		GetByte();
		PUSH_OPERATOR(operator);
		// State doesn't change
		break;

now_expect_dyadic:
		// bugfix: if in error state, do not change state back to valid one
		if (alu_state < STATE_MAX_GO_ON)
			alu_state = STATE_EXPECT_DYADIC_OPERATOR;
		break;
	}
	return TRUE;	// parsed something
}


// Expect dyadic operator (hopefully inlined)
static void expect_dyadic_operator(void)
{
	void		*node_body;
	struct operator	*operator;

	SKIPSPACE();
	switch (GotByte) {
// Single-character dyadic operators
	case '^':	// "to the power of"
		operator = &ops_powerof;
		goto get_byte_and_push_dyadic;

	case '+':	// add
		operator = &ops_add;
		goto get_byte_and_push_dyadic;

	case '-':	// subtract
		operator = &ops_subtract;
		goto get_byte_and_push_dyadic;

	case '*':	// multiply
		operator = &ops_multiply;
		goto get_byte_and_push_dyadic;

	case '/':	// divide
		operator = &ops_divide;
		goto get_byte_and_push_dyadic;

	case '%':	// modulo
		operator = &ops_modulo;
		goto get_byte_and_push_dyadic;

	case '&':	// bitwise AND
		operator = &ops_and;
		goto get_byte_and_push_dyadic;

	case '|':	// bitwise OR
		operator = &ops_or;
		goto get_byte_and_push_dyadic;

// This part is commented out because there is no XOR character defined
//	case ???:	// bitwise exclusive OR
//		operator = &ops_xor;
//		goto get_byte_and_push_dyadic;

	case '=':	// is equal
		operator = &ops_equals;
		// if it's "==", accept but warn
		if (GetByte() == '=') {
			Throw_first_pass_warning("C-style \"==\" comparison detected.");
			goto get_byte_and_push_dyadic;
		}
		goto push_dyadic;

	case ')':	// closing parenthesis
		operator = &ops_closing;
		goto get_byte_and_push_dyadic;

// Multi-character dyadic operators
	case '!':	// "!="
		if (GetByte() == '=') {
			operator = &ops_notequal;
			goto get_byte_and_push_dyadic;
		}

		Throw_error(exception_syntax);
		alu_state = STATE_ERROR;
		break;
	case '<':	// "<", "<=", "<<" and "<>"
		switch (GetByte()) {
		case '=':	// "<=", less or equal
			operator = &ops_le;
			goto get_byte_and_push_dyadic;

		case '<':	// "<<", shift left
			operator = &ops_sl;
			goto get_byte_and_push_dyadic;

		case '>':	// "<>", not equal
			operator = &ops_notequal;
			goto get_byte_and_push_dyadic;

		default:	// "<", less than
			operator = &ops_lessthan;
			goto push_dyadic;

		}
		//break; unreachable
	case '>':	// ">", ">=", ">>", ">>>" and "><"
		switch (GetByte()) {
		case '=':	// ">=", greater or equal
			operator = &ops_ge;
			goto get_byte_and_push_dyadic;

		case '<':	// "><", not equal
			operator = &ops_notequal;
			goto get_byte_and_push_dyadic;

		case '>':	// ">>" or ">>>", shift right
			operator = &ops_asr;	// arithmetic shift right
			if (GetByte() != '>')
				goto push_dyadic;

			operator = &ops_lsr;	// logical shift right
			goto get_byte_and_push_dyadic;

		default:	// ">", greater than
			operator = &ops_greaterthan;
			goto push_dyadic;

		}
		//break; unreachable
// end of expression or text version of dyadic operator
	default:
		// check string versions of operators
		if (BYTE_STARTS_KEYWORD(GotByte)) {
			Input_read_and_lower_keyword();
			// Now GotByte = illegal char
			// search for tree item
			if (Tree_easy_scan(operator_tree, &node_body, GlobalDynaBuf)) {
				operator = node_body;
				goto push_dyadic;
			}

			// goto means we don't need an "else {" here
			Throw_error("Unknown operator.");
			alu_state = STATE_ERROR;
		} else {
			// we found end-of-expression when expecting an operator, that's ok.
			operator = &ops_exprend;
			goto push_dyadic;
		}

	}
	return;

// shared endings
get_byte_and_push_dyadic:
	GetByte();
push_dyadic:
	PUSH_OPERATOR(operator);
	alu_state = STATE_TRY_TO_REDUCE_STACKS;
}


// call C's sin/cos/tan function
static void perform_fp(double (*fn)(double), struct number *self)
{
	if (!(self->flags & NUMBER_IS_FLOAT)) {
		self->val.fpval = self->val.intval;
		self->flags |= NUMBER_IS_FLOAT;
	}
	self->val.fpval = fn(self->val.fpval);
	self->addr_refs = 0;	// result now is a non-address
}


// make sure arg is in [-1, 1] range before calling function
static void perform_ranged_fp(double (*fn)(double), struct number *self)
{
	if (!(self->flags & NUMBER_IS_FLOAT)) {
		self->val.fpval = self->val.intval;
		self->flags |= NUMBER_IS_FLOAT;
	}
	if ((self->val.fpval >= -1) && (self->val.fpval <= 1)) {
		self->val.fpval = fn(self->val.fpval);
	} else {
		if (self->flags & NUMBER_IS_DEFINED)
			Throw_error("Argument out of range.");
		self->val.fpval = 0;
	}
	self->addr_refs = 0;	// result now is a non-address
}


// convert value from fp to int
static void fp_to_int(struct number *self)
{
	self->val.intval = self->val.fpval;
	self->flags &= ~NUMBER_IS_FLOAT;
}


// check both left-hand and right-hand values. if float, convert to int.
// in first pass, throw warning
static void both_ensure_int(struct number *self, struct number *other, boolean warn)
{
	if (self->flags & NUMBER_IS_FLOAT) {
		self->val.intval = self->val.fpval;
		self->flags &= ~NUMBER_IS_FLOAT;
	}
	if (other->flags & NUMBER_IS_FLOAT) {
		other->val.intval = other->val.fpval;
		other->flags &= ~NUMBER_IS_FLOAT;
	}
	if (warn) {
		// FIXME - warning is never seen if both operands are undefined in first pass!
		Throw_first_pass_warning("Converted to integer for binary logic operator.");
	}
}


// check both left-hand and right-hand values. if int, convert to float.
static void both_ensure_fp(struct number *self, struct number *other)
{
	if (!(self->flags & NUMBER_IS_FLOAT)) {
		self->val.fpval = self->val.intval;
		self->flags |= NUMBER_IS_FLOAT;
	}
	if (!(other->flags & NUMBER_IS_FLOAT)) {
		other->val.fpval = other->val.intval;
		other->flags |= NUMBER_IS_FLOAT;
	}
}


// make sure both values are float, but mark left one as int (will become one)
static void ensure_int_from_fp(struct number *self, struct number *other)
{
	both_ensure_fp(self, other);
	self->flags &= ~NUMBER_IS_FLOAT;
}


// monadic operators (including functions)
static void number_do_monadic_op(struct number *self, enum op_handle operation)
{
	switch (operation) {
	case OPHANDLE_ADDR:
		self->addr_refs = 1;	// result now is an address
		break;
	case OPHANDLE_INT:
		if (self->flags & NUMBER_IS_FLOAT)
			fp_to_int(self);
		self->addr_refs = 0;	// result now is a non-address
		break;
	case OPHANDLE_FLOAT:
		// convert value from int to fp
		if (!(self->flags & NUMBER_IS_FLOAT)) {
			self->val.fpval = self->val.intval;
			self->flags |= NUMBER_IS_FLOAT;
		}
		self->addr_refs = 0;	// result now is a non-address
		break;
	case OPHANDLE_SIN:
		perform_fp(sin, self);	// also zeroes addr_refs
		break;
	case OPHANDLE_COS:
		perform_fp(cos, self);	// also zeroes addr_refs
		break;
	case OPHANDLE_TAN:
		perform_fp(tan, self);	// also zeroes addr_refs
		break;
	case OPHANDLE_ARCSIN:
		perform_ranged_fp(asin, self);	// also zeroes addr_refs
		break;
	case OPHANDLE_ARCCOS:
		perform_ranged_fp(acos, self);	// also zeroes addr_refs
		break;
	case OPHANDLE_ARCTAN:
		perform_fp(atan, self);	// also zeroes addr_refs
		break;
	case OPHANDLE_NOT:
		// fp becomes int
		if (self->flags & NUMBER_IS_FLOAT)
			fp_to_int(self);
		self->val.intval = ~(self->val.intval);
		self->flags &= ~NUMBER_FITS_BYTE;
		break;
	case OPHANDLE_NEGATE:
		// different operations for fp and int
		if (self->flags & NUMBER_IS_FLOAT)
			self->val.fpval = -(self->val.fpval);
		else
			self->val.intval = -(self->val.intval);
		self->flags &= ~NUMBER_FITS_BYTE;
		self->addr_refs = -(self->addr_refs);	// negate address ref count as well
		break;
	case OPHANDLE_LOWBYTEOF:
		// fp becomes int
		if (self->flags & NUMBER_IS_FLOAT)
			fp_to_int(self);
		self->val.intval = (self->val.intval) & 255;
		self->flags |= NUMBER_FITS_BYTE;
		self->flags &= ~NUMBER_FORCEBITS;
		self->addr_refs = 0;	// result now is a non-address
		break;
	case OPHANDLE_HIGHBYTEOF:
		// fp becomes int
		if (self->flags & NUMBER_IS_FLOAT)
			fp_to_int(self);
		self->val.intval = ((self->val.intval) >> 8) & 255;
		self->flags |= NUMBER_FITS_BYTE;
		self->flags &= ~NUMBER_FORCEBITS;
		self->addr_refs = 0;	// result now is a non-address
		break;
	case OPHANDLE_BANKBYTEOF:
		// fp becomes int
		if (self->flags & NUMBER_IS_FLOAT)
			fp_to_int(self);
		self->val.intval = ((self->val.intval) >> 16) & 255;
		self->flags |= NUMBER_FITS_BYTE;
		self->flags &= ~NUMBER_FORCEBITS;
		self->addr_refs = 0;	// result now is a non-address
		break;
// add new monadic operators here
//	case OPHANDLE_:
//		Throw_error("var type does not support this operation");
//		break;
	default:
		Bug_found("IllegalOperatorHandleNM", operation);
	}
}


// dyadic operators
static void number_do_dyadic_op(struct number *self, enum op_handle operation, struct number *other)
{
	switch (operation) {
	case OPHANDLE_POWEROF:
		if ((self->flags | other->flags) & NUMBER_IS_FLOAT) {
			// at least one float
			both_ensure_fp(self, other);
			self->val.fpval = pow(self->val.fpval, other->val.fpval);
		} else {
			// two ints
			if (other->val.intval >= 0) {
				self->val.intval = my_pow(self->val.intval, other->val.intval);
			} else {
				if (other->flags & NUMBER_IS_DEFINED)
					Throw_error("Exponent is negative.");
				self->val.intval = 0;
			}
		}
		self->addr_refs = 0;	// result now is a non-address
		break;
	case OPHANDLE_MULTIPLY:
		if ((self->flags | other->flags) & NUMBER_IS_FLOAT) {
			// at least one float
			both_ensure_fp(self, other);
			self->val.fpval *= other->val.fpval;
		} else {
			// two ints
			self->val.intval *= other->val.intval;
		}
		self->addr_refs = 0;	// result now is a non-address
		break;
	case OPHANDLE_DIVIDE:
		if ((self->flags | other->flags) & NUMBER_IS_FLOAT) {
			// at least one float
			both_ensure_fp(self, other);
			if (other->val.fpval) {
				self->val.fpval /= other->val.fpval;
			} else {
				if (other->flags & NUMBER_IS_DEFINED)
					Throw_error(exception_div_by_zero);
				self->val.fpval = 0;
			}
		} else {
			// two ints
			if (other->val.intval) {
				self->val.intval /= other->val.intval;
			} else {
				if (other->flags & NUMBER_IS_DEFINED)
					Throw_error(exception_div_by_zero);
				self->val.intval = 0;
			}
		}
		self->addr_refs = 0;	// result now is a non-address
		break;
	case OPHANDLE_INTDIV:
		if ((self->flags | other->flags) & NUMBER_IS_FLOAT) {
			// at least one float
			both_ensure_fp(self, other);
			if (other->val.fpval) {
				self->val.intval = self->val.fpval / other->val.fpval;
			} else {
				if (other->flags & NUMBER_IS_DEFINED)
					Throw_error(exception_div_by_zero);
				self->val.intval = 0;
			}
			self->flags &= ~NUMBER_IS_FLOAT;	// result is int
		} else {
			// two ints
			if (other->val.intval) {
				self->val.intval /= other->val.intval;
			} else {
				if (other->flags & NUMBER_IS_DEFINED)
					Throw_error(exception_div_by_zero);
				self->val.intval = 0;
			}
		}
		self->addr_refs = 0;	// result now is a non-address
		break;
	case OPHANDLE_MODULO:
		if ((self->flags | other->flags) & NUMBER_IS_FLOAT)
			// at least one float
			both_ensure_int(self, other, FALSE);
		if (other->val.intval) {
			self->val.intval %= other->val.intval;
		} else {
			if (other->flags & NUMBER_IS_DEFINED)
				Throw_error(exception_div_by_zero);
			self->val.intval = 0;
		}
		self->addr_refs = 0;	// result now is a non-address
		break;
	case OPHANDLE_ADD:
		if ((self->flags | other->flags) & NUMBER_IS_FLOAT) {
			// at least one float
			both_ensure_fp(self, other);
			self->val.fpval += other->val.fpval;
		} else {
			// two ints
			self->val.intval += other->val.intval;
		}
		self->addr_refs += other->addr_refs;	// add address references
		break;
	case OPHANDLE_SUBTRACT:
		if ((self->flags | other->flags) & NUMBER_IS_FLOAT) {
			// at least one float
			both_ensure_fp(self, other);
			self->val.fpval -= other->val.fpval;
		} else {
			// two ints
			self->val.intval -= other->val.intval;
		}
		self->addr_refs -= other->addr_refs;	// subtract address references
		break;
	case OPHANDLE_SL:
		if (other->flags & NUMBER_IS_FLOAT)
			fp_to_int(other);
		if (self->flags & NUMBER_IS_FLOAT)
			self->val.fpval *= pow(2.0, other->val.intval);
		else
			self->val.intval <<= other->val.intval;
		self->addr_refs = 0;	// result now is a non-address
		break;
	case OPHANDLE_ASR:
		if (other->flags & NUMBER_IS_FLOAT)
			fp_to_int(other);
		if (self->flags & NUMBER_IS_FLOAT)
			self->val.fpval /= (1 << other->val.intval);	// FIXME - why not use pow() as in SL above?
		else
			self->val.intval = my_asr(self->val.intval, other->val.intval);
		self->addr_refs = 0;	// result now is a non-address
		break;
	case OPHANDLE_LSR:
		// fp become int
		if ((self->flags | other->flags) & NUMBER_IS_FLOAT)
			// at least one float
			both_ensure_int(self, other, TRUE);
		self->val.intval = ((uintval_t) (self->val.intval)) >> other->val.intval;
		self->addr_refs = 0;	// result now is a non-address
		break;
	case OPHANDLE_LE:
		if ((self->flags | other->flags) & NUMBER_IS_FLOAT) {
			// at least one float
			ensure_int_from_fp(self, other);
			self->val.intval = (self->val.fpval <= other->val.fpval);
		} else {
			// two ints
			self->val.intval = (self->val.intval <= other->val.intval);
		}
		self->addr_refs = 0;	// result now is a non-address
		break;
	case OPHANDLE_LESSTHAN:
		if ((self->flags | other->flags) & NUMBER_IS_FLOAT) {
			// at least one float
			ensure_int_from_fp(self, other);
			self->val.intval = (self->val.fpval < other->val.fpval);
		} else {
			// two ints
			self->val.intval = (self->val.intval < other->val.intval);
		}
		self->addr_refs = 0;	// result now is a non-address
		break;
	case OPHANDLE_GE:
		if ((self->flags | other->flags) & NUMBER_IS_FLOAT) {
			// at least one float
			ensure_int_from_fp(self, other);
			self->val.intval = (self->val.fpval >= other->val.fpval);
		} else {
			// two ints
			self->val.intval = (self->val.intval >= other->val.intval);
		}
		self->addr_refs = 0;	// result now is a non-address
		break;
	case OPHANDLE_GREATERTHAN:
		if ((self->flags | other->flags) & NUMBER_IS_FLOAT) {
			// at least one float
			ensure_int_from_fp(self, other);
			self->val.intval = (self->val.fpval > other->val.fpval);
		} else {
			// two ints
			self->val.intval = (self->val.intval > other->val.intval);
		}
		self->addr_refs = 0;	// result now is a non-address
		break;
	case OPHANDLE_NOTEQUAL:
		if ((self->flags | other->flags) & NUMBER_IS_FLOAT) {
			// at least one float
			ensure_int_from_fp(self, other);
			self->val.intval = (self->val.fpval != other->val.fpval);
		} else {
			// two ints
			self->val.intval = (self->val.intval != other->val.intval);
		}
		self->addr_refs = 0;	// result now is a non-address
		break;
	case OPHANDLE_EQUALS:
		if ((self->flags | other->flags) & NUMBER_IS_FLOAT) {
			// at least one float
			ensure_int_from_fp(self, other);
			self->val.intval = (self->val.fpval == other->val.fpval);
		} else {
			// two ints
			self->val.intval = (self->val.intval == other->val.intval);
		}
		self->addr_refs = 0;	// result now is a non-address
		break;
	case OPHANDLE_AND:
		// fp become int
		if ((self->flags | other->flags) & NUMBER_IS_FLOAT) {
			// at least one float
			both_ensure_int(self, other, TRUE);
		}
		self->val.intval &= other->val.intval;
		self->addr_refs += other->addr_refs;	// add address references
		break;
	case OPHANDLE_EOR:
		Throw_first_pass_warning("\"EOR\" is deprecated; use \"XOR\" instead.");
		/*FALLTHROUGH*/
	case OPHANDLE_XOR:
		// fp become int
		if ((self->flags | other->flags) & NUMBER_IS_FLOAT) {
			// at least one float
			both_ensure_int(self, other, TRUE);
		}
		self->val.intval ^= other->val.intval;
		self->addr_refs += other->addr_refs;	// add address references
		break;
	case OPHANDLE_OR:
		// fp become int
		if ((self->flags | other->flags) & NUMBER_IS_FLOAT) {
			// at least one float
			both_ensure_int(self, other, TRUE);
		}
		self->val.intval |= other->val.intval;
		self->addr_refs += other->addr_refs;	// add address references
		break;
// add new dyadic operators here
//	case OPHANDLE_:
//		Throw_error("var type does not support this operation");
//		break;
	default:
		Bug_found("IllegalOperatorHandleND", operation);
	}
}


// handler for special operators like parentheses and start/end of expression:
// returns whether caller should just return without cleaning stack
static boolean do_special_op(struct expression *expression, enum op_handle previous, enum op_handle current)
{
	switch (previous) {
	case OPHANDLE_EXPRSTART:
		// the only operator with a lower priority than this
		// "start-of-expression" operator is "end-of-expression",
		// therefore we know we are done.
		// don't touch "is_parenthesized", because start/end are obviously not "real" operators
		--operator_sp;	// decrement operator stack pointer
		alu_state = STATE_END;	// done
		break;
	case OPHANDLE_OPENING:
		expression->is_parenthesized = TRUE;	// found parentheses. if this is not the outermost level, the outermost level will fix this.
		// check current operator
		switch (current) {
		case OPHANDLE_CLOSING:	// matching parentheses
			operator_sp -= 2;	// remove both of them
			alu_state = STATE_EXPECT_DYADIC_OPERATOR;
			break;
		case OPHANDLE_EXPREND:	// unmatched parenthesis
			++(expression->open_parentheses);	// count
			return FALSE;
	
		default:
			Bug_found("StrangeParenthesis", current);
		}
		break;
	case OPHANDLE_CLOSING:
		Throw_error("Too many ')'.");
		alu_state = STATE_ERROR;
		break;
	default:
		Bug_found("IllegalOperatorHandleS", previous);
	}
	return TRUE;
}


// Try to reduce stacks by performing high-priority operations
// (if the previous operator has a higher priority than the current one, do it)
static void try_to_reduce_stacks(struct expression *expression)
{
	struct operator		*previous;
	struct operator		*current;

	if (operator_sp < 2) {
		// we only have one operator, which must be "start of expression"
		alu_state = STATE_EXPECT_OPERAND_OR_MONADIC_OPERATOR;
		return;
	}

	previous = operator_stack[operator_sp - 2];
	current = operator_stack[operator_sp - 1];

	// previous operator has lower piority than current one? then do nothing.
	if (previous->priority < current->priority) {
		alu_state = STATE_EXPECT_OPERAND_OR_MONADIC_OPERATOR;
		return;
	}

	// previous operator has same priority as current one? then check associativity
	if ((previous->priority == current->priority)
	&& IS_RIGHT_ASSOCIATIVE(current->priority)) {
		alu_state = STATE_EXPECT_OPERAND_OR_MONADIC_OPERATOR;
		return;
	}

	// we now know that either
	// - the previous operator has higher priority, or
	// - it has the same priority and is left-associative,
	// so perform that operation!
#define ARG_PREV	(operand_stack[operand_sp - 2])
#define ARG_NOW		(operand_stack[operand_sp - 1])
	// TODO - make handler functions for monadic and dyadic operators type-
	// specific, so strings and lists can get their own handler functions.
	switch (previous->group) {
	case OPGROUP_MONADIC:	// monadic operators
		// TODO - maybe call different functions for ints and floats?
		number_do_monadic_op(&ARG_NOW, previous->handle);
		// operation was something other than parentheses
		expression->is_parenthesized = FALSE;
		break;
	case OPGROUP_DYADIC:	// dyadic operators
		number_do_dyadic_op(&ARG_PREV, previous->handle, &ARG_NOW);
		// Handle flags and decrement value stack pointer
		// "OR" EVER_UNDEFINED and FORCEBIT flags
		ARG_PREV.flags |= ARG_NOW.flags & (NUMBER_EVER_UNDEFINED | NUMBER_FORCEBITS);
		// "AND" DEFINED flag
		ARG_PREV.flags &= (ARG_NOW.flags | ~NUMBER_IS_DEFINED);
		ARG_PREV.flags &= ~NUMBER_FITS_BYTE;	// clear FITS BYTE flag
		--operand_sp;
		// operation was something other than parentheses
		expression->is_parenthesized = FALSE;
		break;
	case OPGROUP_SPECIAL:	// special (pseudo) operators
		if (do_special_op(expression, previous->handle, current->handle))
			return;

		// monadics and dyadics clear is_parenthesized, but here we don't touch it!
		break;
	default:
		Bug_found("IllegalOperatorHandle", previous->group);	// FIXME - change to IllegalOperatorGroup!
	}
// shared endings for "we did the operation indicated by previous operator":
	// remove previous operator and shift down current one
	operator_stack[operator_sp - 2] = operator_stack[operator_sp - 1];
	--operator_sp;	// decrement operator stack pointer
}


// The core of it.
// FIXME - make state machine using function pointers? or too slow?
static void parse_expression(struct expression *expression)
{
	struct number	*result	= &expression->number;

	// init
	expression->is_empty = TRUE;	// becomes FALSE when first valid char gets parsed
	expression->open_parentheses = 0;
	expression->is_parenthesized = FALSE;	// toplevel operator will set this: '(' to TRUE, all others to FALSE
	//expression->number will be overwritten later, so no need to init

	operator_sp = 0;	// operator stack pointer
	operand_sp = 0;	// value stack pointer
	// begin by reading value (or monadic operator)
	alu_state = STATE_EXPECT_OPERAND_OR_MONADIC_OPERATOR;
	PUSH_OPERATOR(&ops_exprstart);
	do {
		// check stack sizes. enlarge if needed
		if (operator_sp >= operator_stk_size)
			enlarge_operator_stack();
		if (operand_sp >= operand_stk_size)
			enlarge_operand_stack();
		switch (alu_state) {
		case STATE_EXPECT_OPERAND_OR_MONADIC_OPERATOR:
			if (expect_operand_or_monadic_operator())
				expression->is_empty = FALSE;
			break;
		case STATE_EXPECT_DYADIC_OPERATOR:
			expect_dyadic_operator();
			break;	// no fallthrough; state might
			// have been changed to END or ERROR
		case STATE_TRY_TO_REDUCE_STACKS:
			try_to_reduce_stacks(expression);
			break;
		case STATE_MAX_GO_ON:	// suppress
		case STATE_ERROR:	// compiler
		case STATE_END:		// warnings
			break;
		}
	} while (alu_state < STATE_MAX_GO_ON);
	// done. check state.
	if (alu_state == STATE_END) {
		// check for bugs
		if (operand_sp != 1)
			Bug_found("OperandStackNotEmpty", operand_sp);
		if (operator_sp != 1)
			Bug_found("OperatorStackNotEmpty", operator_sp);
		// copy result
		*result = operand_stack[0];
		// only allow *one* force bit
		if (result->flags & NUMBER_FORCES_24)
			result->flags &= ~(NUMBER_FORCES_16 | NUMBER_FORCES_8);
		else if (result->flags & NUMBER_FORCES_16)
			result->flags &= ~NUMBER_FORCES_8;
		// if there was nothing to parse, mark as undefined	FIXME - change this! make "nothing" its own result type; only numbers may be undefined
		// (so ALU_defined_int() can react)
		if (expression->is_empty) {
			result->flags &= ~NUMBER_IS_DEFINED;
		} else {
			// not empty. undefined?
			if (!(expression->number.flags & NUMBER_IS_DEFINED)) {
				// then count (in all passes)
				++pass.undefined_count;
			}
		}
		// do some checks depending on int/float
		if (result->flags & NUMBER_IS_FLOAT) {
/*float*/		// if undefined, return zero
			if ((result->flags & NUMBER_IS_DEFINED) == 0)
				result->val.fpval = 0;
			// if value is sure, check to set FITS BYTE
			else if (((result->flags & NUMBER_EVER_UNDEFINED) == 0)
			&& (result->val.fpval <= 255.0)
			&& (result->val.fpval >= -128.0))
				result->flags |= NUMBER_FITS_BYTE;
		} else {
/*int*/			// if undefined, return zero
			if ((result->flags & NUMBER_IS_DEFINED) == 0)
				result->val.intval = 0;
			// if value is sure, check to set FITS BYTE
			else if (((result->flags & NUMBER_EVER_UNDEFINED) == 0)
			&& (result->val.intval <= 255)
			&& (result->val.intval >= -128))
				result->flags |= NUMBER_FITS_BYTE;
		}
	} else {
		// State is STATE_ERROR. Errors have already been reported,
		// but we must make sure not to pass bogus data to caller.
		// FIXME - just use the return value to indicate "there were errors, do not use result!"
		result->flags = 0;	// maybe set DEFINED flag to suppress follow-up errors?
		result->val.intval = 0;
		result->addr_refs = 0;
		// make sure no additional (spurious) errors are reported:
		Input_skip_remainder();
		// FIXME - remove this when new function interface gets used:
		// callers must decide for themselves what to do when expression parser returns error
		// (currently LDA'' results in both "no string given" AND "illegal combination of command and addressing mode"!)
	}
}


// return int value (if undefined, return zero)
// For empty expressions, an error is thrown.
// OPEN_PARENTHESIS: complain
// EMPTY: complain
// UNDEFINED: allow
// FLOAT: convert to int
intval_t ALU_any_int(void)	// ACCEPT_UNDEFINED
{
	struct expression	expression;

	parse_expression(&expression);
	if (expression.open_parentheses)
		Throw_error(exception_paren_open);
	if (expression.is_empty)
		Throw_error(exception_no_value);
	if (expression.number.flags & NUMBER_IS_FLOAT)
		return expression.number.val.fpval;
	else
		return expression.number.val.intval;
}


// stores int value and flags (floats are transformed to int)
// if result is empty or undefined, serious error is thrown
// OPEN_PARENTHESIS: complain
// EMPTY: complain _seriously_
// UNDEFINED: complain _seriously_
// FLOAT: convert to int
void ALU_defined_int(struct number *intresult)	// no ACCEPT constants?
{
	struct expression	expression;
	boolean			buf	= pass.complain_about_undefined;

	pass.complain_about_undefined = TRUE;
	parse_expression(&expression);
	pass.complain_about_undefined = buf;
	*intresult = expression.number;
	if (expression.open_parentheses)
		Throw_error(exception_paren_open);
	if (expression.is_empty)
		Throw_serious_error(exception_no_value);
	if (!(intresult->flags & NUMBER_IS_DEFINED))
		Throw_serious_error(exception_value_not_defined);
	if (intresult->flags & NUMBER_IS_FLOAT) {
		intresult->val.intval = intresult->val.fpval;
		intresult->flags &= ~NUMBER_IS_FLOAT;
	}
}


// Store int value and flags.
// This function allows for "paren" '(' too many. Needed when parsing indirect
// addressing modes where internal indices have to be possible.
// For empty expressions, an error is thrown.
// OPEN_PARENTHESIS: allow
// UNDEFINED: allow
// EMPTY: complain
// FLOAT: convert to int
void ALU_addrmode_int(struct expression *expression, int paren)	// ACCEPT_UNDEFINED | ACCEPT_OPENPARENTHESIS
{
	parse_expression(expression);
	// convert float to int
	if (expression->number.flags & NUMBER_IS_FLOAT) {
		expression->number.val.intval = expression->number.val.fpval;
		expression->number.flags &= ~NUMBER_IS_FLOAT;
	}
	if (expression->open_parentheses > paren) {
		expression->open_parentheses = 0;
		Throw_error(exception_paren_open);
	}
	if (expression->is_empty)
		Throw_error(exception_no_value);
}


// Store value and flags (result may be either int or float)
// For empty expressions, an error is thrown.
// OPEN_PARENTHESIS: complain
// EMPTY: complain
// UNDEFINED: allow
// FLOAT: keep
void ALU_any_result(struct number *result)	// ACCEPT_UNDEFINED | ACCEPT_FLOAT
{
	struct expression	expression;

	parse_expression(&expression);
	*result = expression.number;
	if (expression.open_parentheses)
		Throw_error(exception_paren_open);
	if (expression.is_empty)
		Throw_error(exception_no_value);
}


/* TODO

change parse_expression() to return error/ok.
after that, move
	if (expression.is_empty)
		Throw_error(exception_no_value);
to end of parse_expression()


// stores int value and flags, allowing for "paren" '(' too many (x-indirect addr).
void ALU_addrmode_int(struct expression *expression, int paren)
	mnemo.c
		when parsing addressing modes					needvalue!

// stores value and flags (result may be either int or float)
void ALU_any_result(struct number *result)
	macro.c
		macro call, when parsing call-by-value arg				don't care
	pseudoopcodes.c
		!set									don't care
		when throwing user-specified errors					don't care
	symbol.c
		explicit symbol definition						don't care

// stores int value and flags (floats are transformed to int)
// if result was undefined, serious error is thrown
void ALU_defined_int(struct number *intresult)
	flow.c
		when parsing loop conditions		make bool			serious
	pseudoopcodes.c
		*=					(FIXME, allow undefined)	needvalue!
		!initmem								serious
		!fill (1st arg)				(maybe allow undefined?)	needvalue!
		!skip					(maybe allow undefined?)	needvalue!
		!align (1st + 2nd arg)			(maybe allow undefined?)	needvalue!
		!pseudopc				(FIXME, allow undefined)	needvalue!
		!if					make bool			serious
		twice in !for								serious
		twice for !binary			(maybe allow undefined?)	needvalue!
		//!enum

// returns int value (0 if result was undefined)
intval_t ALU_any_int(void)
	pseudoopcodes.c
		!xor									needvalue!
		iterator for !by, !wo, etc.						needvalue!
		byte values in !raw, !tx, etc.						needvalue!
		!scrxor									needvalue!
		!fill (2nd arg)								needvalue!
		!align (3rd arg)							needvalue!
*/
