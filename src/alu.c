// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2020 Marco Baye
// Have a look at "acme.c" for further info
//
// Arithmetic/logic unit
// 11 Oct 2006	Improved float reading in parse_number_literal()
// 24 Nov 2007	Now accepts floats starting with decimal point
// 31 Jul 2009	Changed ASR again, just to be on the safe side.
// 14 Jan 2014	Changed associativity of "power-of" operator,
//		so a^b^c now means a^(b^c).
//  7 May 2014	C-style "==" operators are now recognized (but
//		give a warning).
// 31 May 2014	Added "0b" binary number prefix as alternative to "%".
// 28 Apr 2015	Added symbol name output to "value not defined" error.
//  1 Feb 2019	Prepared to make "honor leading zeroes" optionally (now done)

// the words "operand"/"operator"/"operation" are too similar, so:
//	"op" means operator/operation
//	"arg" means argument (used instead of "operand")

#include "alu.h"
#include <stdlib.h>
#include <math.h>	// only for fp support
#include <string.h>	// for memcpy()
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
enum op_id {
	// special (pseudo) operators:
	OPID_END_EXPRESSION,	//		end of expression (quasi-dyadic)
	OPID_START_EXPRESSION,	//		start of expression
	OPID_LEFT_PARENTHESIS,	//	(v	'(' starts subexpression (quasi-monadic)
	OPID_START_LIST,	//	[1,2]	'[' starts list literal (quasi-monadic)
	OPID_START_INDEX,	//	v[	'[' starts subexpression (quasi-monadic, also see dyadic OPID_ATINDEX)
	// monadic operators (including functions):
	OPID_NOT,		//	!v	NOT v		bit-wise NOT
	OPID_NEGATE,		//	-v			negation
	OPID_LOWBYTEOF,		//	<v			low byte of
	OPID_HIGHBYTEOF,	//	>v			high byte of
	OPID_BANKBYTEOF,	//	^v			bank byte of
	OPID_ADDRESS,		//	addr(v)				FIXME - add nonaddr()?
	OPID_INT,		//	int(v)
	OPID_FLOAT,		//	float(v)
	OPID_SIN,		//	sin(v)
	OPID_COS,		//	cos(v)
	OPID_TAN,		//	tan(v)
	OPID_ARCSIN,		//	arcsin(v)
	OPID_ARCCOS,		//	arccos(v)
	OPID_ARCTAN,		//	arctan(v)
	OPID_LEN,		//	len(v)
	// dyadic operators:
	OPID_POWEROF,		//	v^w
	OPID_MULTIPLY,		//	v*w
	OPID_DIVIDE,		//	v/w				division
	OPID_INTDIV,		//	v/w	v DIV w			integer division
	OPID_MODULO,		//	v%w	v MOD w			remainder
	OPID_SHIFTLEFT,		//	v<<w	v ASL w	v LSL w		shift left
	OPID_ASR,		//	v>>w	v ASR w			arithmetic shift right
	OPID_LSR,		//	v>>>w	v LSR w			logical shift right
	OPID_ADD,		//	v+w
	OPID_SUBTRACT,		//	v-w
	OPID_EQUALS,		//	v=w
	OPID_LESSOREQUAL,	//	v<=w
	OPID_LESSTHAN,		//	v< w
	OPID_GREATEROREQUAL,	//	v>=w
	OPID_GREATERTHAN,	//	v> w
	OPID_NOTEQUAL,		//	v!=w	v<>w	v><w
	OPID_AND,		//	v&w		v AND w
	OPID_OR,		//	v|w		v OR w
	OPID_EOR,		//	v EOR w		v XOR w		FIXME - remove
	OPID_XOR,		//	v XOR w
	OPID_LIST_APPEND,	//			used internally when building list literal
	OPID_ATINDEX,		//	v[w]
};
struct op {
#define IS_RIGHT_ASSOCIATIVE(prio)	((prio) & 1)
	int		priority;	// lsb holds "is_right_associative" info!
	enum op_group	group;
	enum op_id	id;
	const char	*text_version;
};
static struct op ops_end_expression	= {0, OPGROUP_SPECIAL,	OPID_END_EXPRESSION,	"end of expression"	};
static struct op ops_start_expression	= {2, OPGROUP_SPECIAL,	OPID_START_EXPRESSION,	"start of expression"	};
static struct op ops_left_parenthesis	= {4, OPGROUP_SPECIAL,	OPID_LEFT_PARENTHESIS,	"left parenthesis"	};
static struct op ops_start_list		= {6, OPGROUP_SPECIAL,	OPID_START_LIST,	"start list"	};
static struct op ops_start_index	= {8, OPGROUP_SPECIAL,	OPID_START_INDEX,	"open index"	};
static struct op ops_list_append	= {14, OPGROUP_DYADIC,	OPID_LIST_APPEND,	"append to list"	};
static struct op ops_or			= {16, OPGROUP_DYADIC,	OPID_OR,	"logical or"	};
static struct op ops_eor		= {18, OPGROUP_DYADIC,	OPID_EOR,	"exclusive or"	};	// FIXME - remove
static struct op ops_xor		= {18, OPGROUP_DYADIC,	OPID_XOR,	"exclusive or"	};
static struct op ops_and		= {20, OPGROUP_DYADIC,	OPID_AND,	"logical and"	};
static struct op ops_equals		= {22, OPGROUP_DYADIC,	OPID_EQUALS,		"test for equality"	};
static struct op ops_not_equal		= {24, OPGROUP_DYADIC,	OPID_NOTEQUAL,		"test for inequality"	};
	// same priority for all comparison operators
static struct op ops_less_or_equal	= {26, OPGROUP_DYADIC,	OPID_LESSOREQUAL,	"less than or equal"	};
static struct op ops_less_than		= {26, OPGROUP_DYADIC,	OPID_LESSTHAN,		"less than"	};
static struct op ops_greater_or_equal	= {26, OPGROUP_DYADIC,	OPID_GREATEROREQUAL,	"greater than or equal"	};
static struct op ops_greater_than	= {26, OPGROUP_DYADIC,	OPID_GREATERTHAN,	"greater than"	};
	// same priority for all byte extraction operators
static struct op ops_low_byte_of	= {28, OPGROUP_MONADIC,	OPID_LOWBYTEOF,		"low byte of"	};
static struct op ops_high_byte_of	= {28, OPGROUP_MONADIC,	OPID_HIGHBYTEOF,	"high byte of"	};
static struct op ops_bank_byte_of	= {28, OPGROUP_MONADIC,	OPID_BANKBYTEOF,	"bank byte of"	};
	// same priority for all shift operators (left-associative, though they could be argued to be made right-associative :))
static struct op ops_shift_left		= {30, OPGROUP_DYADIC,	OPID_SHIFTLEFT,	"shift left"	};
static struct op ops_asr		= {30, OPGROUP_DYADIC,	OPID_ASR,	"arithmetic shift right"	};
static struct op ops_lsr		= {30, OPGROUP_DYADIC,	OPID_LSR,	"logical shift right"	};
	// same priority for "+" and "-"
static struct op ops_add		= {32, OPGROUP_DYADIC,	OPID_ADD,	"addition"	};
static struct op ops_subtract		= {32, OPGROUP_DYADIC,	OPID_SUBTRACT,	"subtraction"	};
	// same priority for "*", "/" and "%"
static struct op ops_multiply		= {34, OPGROUP_DYADIC,	OPID_MULTIPLY,	"multiplication"	};
static struct op ops_divide		= {34, OPGROUP_DYADIC,	OPID_DIVIDE,	"division"	};
static struct op ops_intdiv		= {34, OPGROUP_DYADIC,	OPID_INTDIV,	"integer division"	};
static struct op ops_modulo		= {34, OPGROUP_DYADIC,	OPID_MODULO,	"modulo"	};
	// highest "real" priorities
static struct op ops_negate		= {36, OPGROUP_MONADIC,	OPID_NEGATE,	"negation"	};
static struct op ops_powerof		= {37, OPGROUP_DYADIC,	OPID_POWEROF,	"power of"	};	// right-associative!
static struct op ops_not		= {38, OPGROUP_MONADIC,	OPID_NOT,	"logical not"	};
static struct op ops_atindex		= {40, OPGROUP_DYADIC,	OPID_ATINDEX,	"indexing"	};
	// function calls act as if they were monadic operators.
	// they need high priorities to make sure they are evaluated once the
	// parentheses' content is known:
	// "sin(3 + 4) DYADIC_OPERATOR 5" becomes "sin 7 DYADIC_OPERATOR 5",
	// so function calls' priority must be higher than all dyadic operators.
static struct op ops_addr		= {42, OPGROUP_MONADIC, OPID_ADDRESS,	"address()"	};
static struct op ops_int		= {42, OPGROUP_MONADIC, OPID_INT,	"int()"	};
static struct op ops_float		= {42, OPGROUP_MONADIC, OPID_FLOAT,	"float()"	};
static struct op ops_sin		= {42, OPGROUP_MONADIC, OPID_SIN,	"sin()"	};
static struct op ops_cos		= {42, OPGROUP_MONADIC, OPID_COS,	"cos()"	};
static struct op ops_tan		= {42, OPGROUP_MONADIC, OPID_TAN,	"tan()"	};
static struct op ops_arcsin		= {42, OPGROUP_MONADIC, OPID_ARCSIN,	"arcsin()"	};
static struct op ops_arccos		= {42, OPGROUP_MONADIC, OPID_ARCCOS,	"arccos()"	};
static struct op ops_arctan		= {42, OPGROUP_MONADIC, OPID_ARCTAN,	"arctan()"	};
static struct op ops_len		= {42, OPGROUP_MONADIC, OPID_LEN,	"len()"	};
// CAUTION: when adding a function that returns something indexable, fix the code inserting ops_atindex!


// variables
static struct dynabuf	*errormsg_dyna_buf;	// dynamic buffer to build variable-length error messages
static struct dynabuf	*function_dyna_buf;	// dynamic buffer for fn names
// operator stack, current size and stack pointer:
static struct op	**op_stack	= NULL;
static int		opstack_size	= HALF_INITIAL_STACK_SIZE;
static int		op_sp;
// argument stack, current size and stack pointer:
static struct object	*arg_stack	= NULL;
static int		argstack_size	= HALF_INITIAL_STACK_SIZE;
static int		arg_sp;
enum alu_state {
	STATE_EXPECT_ARG_OR_MONADIC_OP,
	STATE_EXPECT_DYADIC_OP,
	STATE_TRY_TO_REDUCE_STACKS,
	STATE_MAX_GO_ON,	// "border value" to find the stoppers:
	STATE_ERROR,		// error has occurred
	STATE_END		// standard end
};
static enum alu_state	alu_state;	// deterministic finite automaton
// predefined stuff
static struct ronode	*op_tree	= NULL;	// tree to hold operators
static struct ronode	op_list[]	= {
	PREDEFNODE(s_asr,	&ops_asr),
	PREDEFNODE(s_lsr,	&ops_lsr),
	PREDEFNODE(s_asl,	&ops_shift_left),
	PREDEFNODE("lsl",	&ops_shift_left),
	PREDEFNODE("div",	&ops_intdiv),
	PREDEFNODE("mod",	&ops_modulo),
	PREDEFNODE(s_and,	&ops_and),
	PREDEFNODE(s_or,	&ops_or),
	PREDEFNODE(s_eor,	&ops_eor),		// FIXME - remove
	PREDEFLAST(s_xor,	&ops_xor),
	//    ^^^^ this marks the last element
};
static struct ronode	*function_tree	= NULL;	// tree to hold functions
static struct ronode	function_list[]	= {
	PREDEFNODE("addr",	&ops_addr),
	PREDEFNODE("address",	&ops_addr),
	PREDEFNODE("int",	&ops_int),
	PREDEFNODE("float",	&ops_float),
	PREDEFNODE("len",	&ops_len),
	PREDEFNODE(s_arcsin,	&ops_arcsin),
	PREDEFNODE(s_arccos,	&ops_arccos),
	PREDEFNODE(s_arctan,	&ops_arctan),
	PREDEFNODE(s_sin,	&ops_sin),
	PREDEFNODE(s_cos,	&ops_cos),
	PREDEFLAST(s_tan,	&ops_tan),
	//    ^^^^ this marks the last element
};

#define PUSH_OP(x)				\
do {						\
	op_stack[op_sp] = (x);			\
	if (++op_sp >= opstack_size)		\
		enlarge_operator_stack();	\
} while (0)

#define PUSH_INT_ARG(i, f, r)				\
do {							\
	arg_stack[arg_sp].type = &type_int;		\
	arg_stack[arg_sp].u.number.flags = (f);		\
	arg_stack[arg_sp].u.number.val.intval = (i);	\
	arg_stack[arg_sp++].u.number.addr_refs = (r);	\
} while (0)
#define PUSH_FP_ARG(fp, f)				\
do {							\
	arg_stack[arg_sp].type = &type_float;		\
	arg_stack[arg_sp].u.number.flags = (f);		\
	arg_stack[arg_sp].u.number.val.fpval = (fp);	\
	arg_stack[arg_sp++].u.number.addr_refs = 0;	\
} while (0)


// double the size of the operator stack
static void enlarge_operator_stack(void)
{
	opstack_size *= 2;
	op_stack = realloc(op_stack, opstack_size * sizeof(*op_stack));
	if (op_stack == NULL)
		Throw_serious_error(exception_no_memory_left);
}


// double the size of the argument stack
static void enlarge_argument_stack(void)
{
	argstack_size *= 2;
	arg_stack = realloc(arg_stack, argstack_size * sizeof(*arg_stack));
	if (arg_stack == NULL)
		Throw_serious_error(exception_no_memory_left);
}


// create dynamic buffer, operator/function trees and operator/argument stacks
void ALU_init(void)
{
	errormsg_dyna_buf = DynaBuf_create(ERRORMSG_DYNABUF_INITIALSIZE);
	function_dyna_buf = DynaBuf_create(FUNCTION_DYNABUF_INITIALSIZE);
	Tree_add_table(&op_tree, op_list);
	Tree_add_table(&function_tree, function_list);
	enlarge_operator_stack();
	enlarge_argument_stack();
}


// not-so-braindead algorithm for calculating "to the power of" function for
// integer arguments.
// my_pow(whatever, 0) returns 1.
// my_pow(0, whatever_but_zero) returns 0.
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
	// if first argument is positive or zero, ASR and LSR are equivalent,
	// so just do it and return the result:
	if (left >= 0)
		return left >> right;

	// However, if the first argument is negative, the result is
	// implementation-defined: While most compilers will do ASR, some others
	// might do LSR instead, and *theoretically*, it is even possible for a
	// compiler to define silly stuff like "shifting a negative value to the
	// right will always return -1".
	// Therefore, in case of a negative argument, we'll use this quick and
	// simple workaround:
	return ~((~left) >> right);
}


// if wanted, throw "Value not defined" error
// This function is not allowed to change DynaBuf because the symbol's name
// might be stored there!
static void is_not_defined(struct symbol *optional_symbol, char optional_prefix_char, char *name, size_t length)
{
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
// TODO - add int arg for number of pseudopc-de-refs via '&' prefix and act upon
static void get_symbol_value(scope_t scope, char optional_prefix_char, size_t name_length)
{
	struct symbol	*symbol;

	// if the symbol gets created now, mark it as unsure
	symbol = symbol_find(scope, NUMBER_EVER_UNDEFINED);
	// if needed, output "value not defined" error
	if (!(symbol->result.type->is_defined(&symbol->result)))
		is_not_defined(symbol, optional_prefix_char, GLOBALDYNABUF_CURRENT, name_length);
	// in first pass, count usage
	if (FIRST_PASS)
		symbol->usage++;
	// push argument, regardless of whether int or float
	// FIXME - if arg is list, increment ref count!
	arg_stack[arg_sp++] = symbol->result;
}


// Parse program counter ('*')
static void parse_program_counter(void)	// Now GotByte = "*"
{
	struct number	pc;

	GetByte();
	vcpu_read_pc(&pc);
	// if needed, output "value not defined" error
	if (!(pc.flags & NUMBER_IS_DEFINED))
		is_not_defined(NULL, 0, "*", 1);
	PUSH_INT_ARG(pc.val.intval, pc.flags, pc.addr_refs);
}


// make new string object
static void string_init_string(struct object *self, const char *data, int len)
{
	self->type = &type_string;
	self->u.string = safe_malloc(sizeof(*(self->u.string)) + len);
	memcpy(self->u.string->payload, data, len);
	self->u.string->payload[len] = 0;	// terminate (just for easier printf-debugging)
	self->u.string->length = len;
	self->u.string->refs = 1;
}
// parse string or character
// characters will be converted using the current encoding, strings are kept as-is.
static void parse_quoted(char closing_quote)
{
	intval_t	value;

	DYNABUF_CLEAR(GlobalDynaBuf);
	if (Input_quoted_to_dynabuf(closing_quote))
		goto fail;	// unterminated or escaping error

	// eat closing quote
	GetByte();
	// now convert to unescaped version
	if (Input_unescape_dynabuf(0))
		goto fail;	// escaping error

	// without backslash escaping, both ' and " are used for single
	// characters.
	// with backslash escaping, ' is for characters and " is for strings:
	if ((closing_quote == '"') && (config.backslash_escaping)) {
		// string //////////////////////////////////
		string_init_string(&arg_stack[arg_sp++], GLOBALDYNABUF_CURRENT, GlobalDynaBuf->size);	// create string object and put on arg stack
	} else {
		// single character ////////////////////////
		// too short?
		if (GlobalDynaBuf->size == 0) {
			Throw_error(exception_missing_string);
			goto fail;
		}

		// too long?
		if (GlobalDynaBuf->size != 1)
			Throw_error("There's more than one character.");
		// parse character
		value = (intval_t) encoding_encode_char(GLOBALDYNABUF_CURRENT[0]);
		PUSH_INT_ARG(value, NUMBER_IS_DEFINED | NUMBER_FITS_BYTE, 0);
	}
	// Now GotByte = char following closing quote (or CHAR_EOS on error)
	return;

fail:
	PUSH_INT_ARG(0, NUMBER_IS_DEFINED | NUMBER_FITS_BYTE, 0);	// dummy
	alu_state = STATE_ERROR;
}


// Parse binary value. Apart from '0' and '1', it also accepts the characters
// '.' and '#', this is much more readable. The current value is stored as soon
// as a character is read that is none of those given above.
static void parse_binary_literal(void)	// Now GotByte = "%" or "b"
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
	if (!digits)
		Throw_warning("Binary literal without any digits");	// FIXME - make into error!
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
	PUSH_INT_ARG(value, flags, 0);
	// Now GotByte = non-binary char
}


// Parse hexadecimal value. It accepts "0" to "9", "a" to "f" and "A" to "F".
// The current value is stored as soon as a character is read that is none of
// those given above.
static void parse_hex_literal(void)	// Now GotByte = "$" or "x"
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
	if (!digits)
		Throw_warning("Hex literal without any digits");	// FIXME - make into error!
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
	PUSH_INT_ARG(value, flags, 0);
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
	PUSH_FP_ARG(fpval / denominator, NUMBER_IS_DEFINED);
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
static void parse_number_literal(void)	// Now GotByte = first digit
{
	intval_t	intval	= (GotByte & 15);	// this works. it's ASCII.

	GetByte();
	// check for "0b" (binary) and "0x" (hexadecimal) prefixes
	if (intval == 0) {
		if (GotByte == 'b') {
			parse_binary_literal();
			return;
		}
		if (GotByte == 'x') {
			parse_hex_literal();
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
		PUSH_INT_ARG(intval, NUMBER_IS_DEFINED, 0);
	}
	// Now GotByte = non-decimal char
}


// Parse octal value. It accepts "0" to "7". The current value is stored as
// soon as a character is read that is none of those given above.
static void parse_octal_literal(void)	// Now GotByte = "&"
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
	if (!digits)
		Throw_warning("Octal literal without any digits");	// FIXME - make into error!
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
	PUSH_INT_ARG(value, flags, 0);
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
		PUSH_OP((struct op *) node_body);
	} else {
		Throw_error("Unknown function.");
		alu_state = STATE_ERROR;
	}
}


// make empty list
static void list_init_list(struct object *self)
{
	self->type = &type_list;
	self->u.listhead = safe_malloc(sizeof(*(self->u.listhead)));
	self->u.listhead->next = self->u.listhead;
	self->u.listhead->prev = self->u.listhead;
	self->u.listhead->length = 0;
	self->u.listhead->refs = 1;
}


// expression parser


// Expect argument or monadic operator (hopefully inlined)
// returns TRUE if it ate any non-space (-> so expression isn't empty)
// returns FALSE if first non-space is delimiter (-> end of expression)
static boolean expect_argument_or_monadic_operator(void)
{
	struct op	*op;
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
		goto now_expect_dyadic_op;

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
			goto now_expect_dyadic_op;
		}

		if (perform_negation)
			PUSH_OP(&ops_negate);
		// State doesn't change
		break;
// Real monadic operators (state doesn't change, still ExpectMonadic)
	case '!':	// NOT operator
		op = &ops_not;
		goto get_byte_and_push_monadic;

	case '<':	// LOWBYTE operator
		op = &ops_low_byte_of;
		goto get_byte_and_push_monadic;

	case '>':	// HIGHBYTE operator
		op = &ops_high_byte_of;
		goto get_byte_and_push_monadic;

	case '^':	// BANKBYTE operator
		op = &ops_bank_byte_of;
		goto get_byte_and_push_monadic;

// special operators
	case '[':	// start of list literal
		list_init_list(&arg_stack[arg_sp++]);	// put empty list on arg stack
		NEXTANDSKIPSPACE();
		if (GotByte == ']') {
			// list literal is empty, so we're basically done
			GetByte();
			alu_state = STATE_EXPECT_DYADIC_OP;
		} else {
			// non-empty list literal
			PUSH_OP(&ops_start_list);	// quasi-monadic "start of list", makes sure earlier ops do not process empty list
			PUSH_OP(&ops_list_append);	// dyadic "append to list", so next arg will be appended to list
			// no need to TRY_TO_REDUCE_STACKS, because we know the one pushed first has a lower priority anyway
			//stay in STATE_EXPECT_ARG_OR_MONADIC_OP
		}
		break;

	case '(':	// left parenthesis
		op = &ops_left_parenthesis;
		goto get_byte_and_push_monadic;

// arguments (state changes to ExpectDyadic)
	case '"':	// character (old) or string (new)
	case '\'':	// character
		// Character will be converted using current encoding
		parse_quoted(GotByte);
		// Now GotByte = char following closing quote
		goto now_expect_dyadic_op;

	case '%':	// Binary value
		parse_binary_literal();	// Now GotByte = non-binary char
		goto now_expect_dyadic_op;

	case '&':	// Octal value
		// TODO - count consecutive '&' and allow symbol afterward, for pseudopc-de-ref!
		parse_octal_literal();	// Now GotByte = non-octal char
		goto now_expect_dyadic_op;

	case '$':	// Hexadecimal value
		parse_hex_literal();
		// Now GotByte = non-hexadecimal char
		goto now_expect_dyadic_op;

	case '*':	// Program counter
		parse_program_counter();
		// Now GotByte = char after closing quote
		goto now_expect_dyadic_op;

// FIXME - find a way to tell decimal point and LOCAL_PREFIX apart!
	case '.':	// local symbol or fractional part of float value
		GetByte();	// start after '.'
		// check for fractional part of float value
		if ((GotByte >= '0') && (GotByte <= '9')) {
			parse_frac_part(0);
			// Now GotByte = non-decimal char
			goto now_expect_dyadic_op;
		}

		if (Input_read_keyword()) {
			// Now GotByte = illegal char
			get_symbol_value(section_now->local_scope, LOCAL_PREFIX, GlobalDynaBuf->size - 1);	// -1 to not count terminator
			goto now_expect_dyadic_op;
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
			goto now_expect_dyadic_op;
		}

		// if we're here, Input_read_keyword() will have thrown an error (like "no string given"):
		alu_state = STATE_ERROR;
		break;
	// decimal values and global symbols
	default:	// all other characters
		if ((GotByte >= '0') && (GotByte <= '9')) {
			parse_number_literal();
			// Now GotByte = non-decimal char
			goto now_expect_dyadic_op;
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
				PUSH_OP(&ops_not);
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
					goto now_expect_dyadic_op;
				}
			}
		} else {
			// illegal character read - so don't go on
			// we found end-of-expression instead of an argument,
			// that's either an empty expression or an erroneous one!
			PUSH_INT_ARG(0, 0, 0);	// push dummy argument so stack checking code won't bark
			if (op_stack[op_sp - 1] == &ops_start_expression) {
				PUSH_OP(&ops_end_expression);
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
		PUSH_OP(op);
		// State doesn't change
		break;

now_expect_dyadic_op:
		// bugfix: if in error state, do not change state back to valid one
		if (alu_state < STATE_MAX_GO_ON)
			alu_state = STATE_EXPECT_DYADIC_OP;
		break;
	}
	return TRUE;	// parsed something
}


// Expect dyadic operator (hopefully inlined)
static void expect_dyadic_operator(void)
{
	void		*node_body;
	struct op	*op;

	SKIPSPACE();
	switch (GotByte) {
// Single-character dyadic operators
	case '^':	// "to the power of"
		op = &ops_powerof;
		goto get_byte_and_push_dyadic;

	case '+':	// add
		op = &ops_add;
		goto get_byte_and_push_dyadic;

	case '-':	// subtract
		op = &ops_subtract;
		goto get_byte_and_push_dyadic;

	case '*':	// multiply
		op = &ops_multiply;
		goto get_byte_and_push_dyadic;

	case '/':	// divide
		op = &ops_divide;
		goto get_byte_and_push_dyadic;

	case '%':	// modulo
		op = &ops_modulo;
		goto get_byte_and_push_dyadic;

	case '&':	// bitwise AND
		op = &ops_and;
		goto get_byte_and_push_dyadic;

	case '|':	// bitwise OR
		op = &ops_or;
		goto get_byte_and_push_dyadic;

// This part is commented out because there is no XOR character defined
//	case ???:	// bitwise exclusive OR
//		op = &ops_xor;
//		goto get_byte_and_push_dyadic;

	case '=':	// is equal
		op = &ops_equals;
		// if it's "==", accept but warn
		if (GetByte() == '=') {
			Throw_first_pass_warning("C-style \"==\" comparison detected.");
			goto get_byte_and_push_dyadic;
		}
		goto push_dyadic_op;

	case '[':	// indexing operator
		GetByte();	// eat char
		PUSH_OP(&ops_atindex);	// first put high-priority dyadic on stack,
		PUSH_OP(&ops_start_index);	// then low-priority special ops_start_index
// FIXME! this would work reliably if "atindex" had the highest priority.
// but function calls have higher priority than indexing:
// fn(a+b)[c] -> fn d [c] -> e [c], but the code above would return fn(d[c]) instead
// atm, it's not a problem, because all functions return numbers, and numbers cannot
// be indexed anyway, but in the long run, this must be fixed.
// maybe call "try_to_reduce_stacks" inbetween the two PUSH_OPs above?
// or maybe add a PUSH_DYADIC_AND_TRY_TO_REDUCE(op) macro?
		alu_state = STATE_EXPECT_ARG_OR_MONADIC_OP;
		return;

// Multi-character dyadic operators
	case '!':	// "!="
		if (GetByte() == '=') {
			op = &ops_not_equal;
			goto get_byte_and_push_dyadic;
		}

		Throw_error(exception_syntax);
		alu_state = STATE_ERROR;
		break;
	case '<':	// "<", "<=", "<<" and "<>"
		switch (GetByte()) {
		case '=':	// "<=", less or equal
			op = &ops_less_or_equal;
			goto get_byte_and_push_dyadic;

		case '<':	// "<<", shift left
			op = &ops_shift_left;
			goto get_byte_and_push_dyadic;

		case '>':	// "<>", not equal
			op = &ops_not_equal;
			goto get_byte_and_push_dyadic;

		default:	// "<", less than
			op = &ops_less_than;
			goto push_dyadic_op;

		}
		//break; unreachable
	case '>':	// ">", ">=", ">>", ">>>" and "><"
		switch (GetByte()) {
		case '=':	// ">=", greater or equal
			op = &ops_greater_or_equal;
			goto get_byte_and_push_dyadic;

		case '<':	// "><", not equal
			op = &ops_not_equal;
			goto get_byte_and_push_dyadic;

		case '>':	// ">>" or ">>>", shift right
			op = &ops_asr;	// arithmetic shift right
			if (GetByte() != '>')
				goto push_dyadic_op;

			op = &ops_lsr;	// logical shift right
			goto get_byte_and_push_dyadic;

		default:	// ">", greater than
			op = &ops_greater_than;
			goto push_dyadic_op;

		}
		//break; unreachable
// end of expression or text version of dyadic operator
	default:
		// check string versions of operators
		if (BYTE_STARTS_KEYWORD(GotByte)) {
			Input_read_and_lower_keyword();
			// Now GotByte = illegal char
			// search for tree item
			if (Tree_easy_scan(op_tree, &node_body, GlobalDynaBuf)) {
				op = node_body;
				goto push_dyadic_op;
			}

			Throw_error("Unknown operator.");
			alu_state = STATE_ERROR;
		} else {
			// we found end-of-expression when expecting an operator, that's ok.
			op = &ops_end_expression;
			goto push_dyadic_op;
		}

	}
	return;	// TODO - check if anything goes here, then change that and add a Bug_found()

// shared endings
get_byte_and_push_dyadic:
	GetByte();
push_dyadic_op:
	PUSH_OP(op);
	alu_state = STATE_TRY_TO_REDUCE_STACKS;
}


// helper function: create and output error message about (argument/)operator/argument combination
static void unsupported_operation(struct object *optional, struct op *op, struct object *arg)
{
	if (optional) {
		if (op->group != OPGROUP_DYADIC)
			Bug_found("OperatorIsNotDyadic", op->id);	// FIXME - add to docs
	} else {
		if (op->group != OPGROUP_MONADIC)
			Bug_found("OperatorIsNotMonadic", op->id);	// FIXME - add to docs
	}
	DYNABUF_CLEAR(errormsg_dyna_buf);
	DynaBuf_add_string(errormsg_dyna_buf, "Operation not supported: Cannot apply \"");	// FIXME - add to docs
	DynaBuf_add_string(errormsg_dyna_buf, op->text_version);
	DynaBuf_add_string(errormsg_dyna_buf, "\" to \"");
	if (optional) {
		DynaBuf_add_string(errormsg_dyna_buf, optional->type->name);
		DynaBuf_add_string(errormsg_dyna_buf, "\" and \"");
	}
	DynaBuf_add_string(errormsg_dyna_buf, arg->type->name);
	DynaBuf_add_string(errormsg_dyna_buf, "\".");
	DynaBuf_append(errormsg_dyna_buf, '\0');
	Throw_error(errormsg_dyna_buf->buffer);
}


// int/float


// int:
// convert to float
inline static void int_to_float(struct object *self)
{
	self->type = &type_float;
	self->u.number.val.fpval = self->u.number.val.intval;
}

// float:
// convert to int
inline static void float_to_int(struct object *self)
{
	self->type = &type_int;
	self->u.number.val.intval = self->u.number.val.fpval;
}

// int/float:
// return DEFINED flag
static boolean number_is_defined(struct object *self)
{
	return !!(self->u.number.flags & NUMBER_IS_DEFINED);
}

// list/string:
// ...are always considered "defined"
static boolean object_return_true(struct object *self)
{
	return TRUE;
}

// this gets called for LSR, AND, OR, XOR with float args
// FIXME - warning is never seen if arguments are undefined in first pass!
static void warn_float_to_int(void)
{
	Throw_first_pass_warning("Converted to integer for binary logic operator.");
}

// int:
// handle monadic operator (includes functions)
static void int_handle_monadic_operator(struct object *self, struct op *op)
{
	int	refs	= 0;	// default for "addr_refs", shortens this fn

	switch (op->id) {
	case OPID_ADDRESS:
		refs = 1;	// result now is an address
		break;
	case OPID_INT:
		break;
	case OPID_FLOAT:
		int_to_float(self);
		break;
	case OPID_SIN:
	case OPID_COS:
	case OPID_TAN:
	case OPID_ARCSIN:
	case OPID_ARCCOS:
	case OPID_ARCTAN:
		// convert int to fp and ask fp handler to do the work
		int_to_float(self);
		type_float.handle_monadic_operator(self, op);	// TODO - put recursion check around this?
		return;	// float handler has done everything

	case OPID_NOT:
		self->u.number.val.intval = ~(self->u.number.val.intval);
		self->u.number.flags &= ~NUMBER_FITS_BYTE;
		refs = -(self->u.number.addr_refs);	// negate address ref count
		break;
	case OPID_NEGATE:
		self->u.number.val.intval = -(self->u.number.val.intval);
		self->u.number.flags &= ~NUMBER_FITS_BYTE;
		refs = -(self->u.number.addr_refs);	// negate address ref count as well
		break;
	case OPID_LOWBYTEOF:
		self->u.number.val.intval = (self->u.number.val.intval) & 255;
		self->u.number.flags |= NUMBER_FITS_BYTE;
		self->u.number.flags &= ~NUMBER_FORCEBITS;
		break;
	case OPID_HIGHBYTEOF:
		self->u.number.val.intval = ((self->u.number.val.intval) >> 8) & 255;
		self->u.number.flags |= NUMBER_FITS_BYTE;
		self->u.number.flags &= ~NUMBER_FORCEBITS;
		break;
	case OPID_BANKBYTEOF:
		self->u.number.val.intval = ((self->u.number.val.intval) >> 16) & 255;
		self->u.number.flags |= NUMBER_FITS_BYTE;
		self->u.number.flags &= ~NUMBER_FORCEBITS;
		break;
// add new monadic operators here
//	case OPID_:
//		break;
	default:
		unsupported_operation(NULL, op, self);
	}
	self->u.number.addr_refs = refs;	// update address refs with local copy
}

// float:
// helper function for asin/acos:
// make sure arg is in [-1, 1] range before calling function
static void float_ranged_fn(double (*fn)(double), struct object *self)
{
	if ((self->u.number.val.fpval >= -1) && (self->u.number.val.fpval <= 1)) {
		self->u.number.val.fpval = fn(self->u.number.val.fpval);
	} else {
		if (self->u.number.flags & NUMBER_IS_DEFINED)
			Throw_error("Argument out of range.");	// TODO - add number output to error message
		self->u.number.val.fpval = 0;
	}
}

// float:
// handle monadic operator (includes functions)
static void float_handle_monadic_operator(struct object *self, struct op *op)
{
	int	refs	= 0;	// default for "addr_refs", shortens this fn

	switch (op->id) {
	case OPID_ADDRESS:
		refs = 1;	// result now is an address
		break;
	case OPID_INT:
		float_to_int(self);
		break;
	case OPID_FLOAT:
		break;
	case OPID_SIN:
		self->u.number.val.fpval = sin(self->u.number.val.fpval);
		break;
	case OPID_COS:
		self->u.number.val.fpval = cos(self->u.number.val.fpval);
		break;
	case OPID_TAN:
		self->u.number.val.fpval = tan(self->u.number.val.fpval);
		break;
	case OPID_ARCSIN:
		float_ranged_fn(asin, self);
		break;
	case OPID_ARCCOS:
		float_ranged_fn(acos, self);
		break;
	case OPID_ARCTAN:
		self->u.number.val.fpval = atan(self->u.number.val.fpval);
		break;
	case OPID_NEGATE:
		self->u.number.val.fpval = -(self->u.number.val.fpval);
		self->u.number.flags &= ~NUMBER_FITS_BYTE;
		refs = -(self->u.number.addr_refs);	// negate address ref count as well
		break;
	case OPID_NOT:
	case OPID_LOWBYTEOF:
	case OPID_HIGHBYTEOF:
	case OPID_BANKBYTEOF:
		// convert fp to int and ask int handler to do the work
		float_to_int(self);
		type_int.handle_monadic_operator(self, op);	// TODO - put recursion check around this?
		return;	// int handler has done everything

// add new monadic operators here
//	case OPID_:
//		break;
	default:
		unsupported_operation(NULL, op, self);
	}
	self->u.number.addr_refs = refs;	// update address refs with local copy
}

// list:
// handle monadic operator (includes functions)
static void list_handle_monadic_operator(struct object *self, struct op *op)
{
	int	length;

	if (op->id == OPID_LEN) {
		length = self->u.listhead->length;
		self->u.listhead->refs--;	// FIXME - call some list_decrement_refs() instead...
		self->type = &type_int;
		self->u.number.flags = NUMBER_IS_DEFINED;
		self->u.number.val.intval = length;
		self->u.number.addr_refs = 0;
	} else {
		unsupported_operation(NULL, op, self);
	}
}

// string:
// handle monadic operator (includes functions)
static void string_handle_monadic_operator(struct object *self, struct op *op)
{
	int	length;

	if (op->id == OPID_LEN) {
		length = self->u.string->length;
		self->u.string->refs--;	// FIXME - call some string_decrement_refs() instead...
		self->type = &type_int;
		self->u.number.flags = NUMBER_IS_DEFINED;
		self->u.number.val.intval = length;
		self->u.number.addr_refs = 0;
	} else {
		unsupported_operation(NULL, op, self);
	}
}

// int/float:
// merge result flags
// (used by both int and float handlers for dyadic operators)
static void number_fix_result_after_dyadic(struct object *self, struct object *other)
{
	// EVER_UNDEFINED and FORCEBIT flags are ORd together
	self->u.number.flags |= other->u.number.flags & (NUMBER_EVER_UNDEFINED | NUMBER_FORCEBITS);
	// DEFINED flags are ANDed together
	self->u.number.flags &= (other->u.number.flags | ~NUMBER_IS_DEFINED);
	// FITS_BYTE is cleared
	self->u.number.flags &= ~NUMBER_FITS_BYTE;
}


// int:
// handle dyadic operator
static void int_handle_dyadic_operator(struct object *self, struct op *op, struct object *other)
{
	int	refs	= 0;	// default for "addr_refs", shortens this fn

	// first check type of second arg:
	if (other->type == &type_int) {
		// ok
	} else if (other->type == &type_float) {
		// handle according to operation
		switch (op->id) {
		case OPID_POWEROF:
		case OPID_MULTIPLY:
		case OPID_DIVIDE:
		case OPID_INTDIV:
		case OPID_ADD:
		case OPID_SUBTRACT:
		case OPID_EQUALS:
		case OPID_LESSOREQUAL:
		case OPID_LESSTHAN:
		case OPID_GREATEROREQUAL:
		case OPID_GREATERTHAN:
		case OPID_NOTEQUAL:
			// become float, delegate to float handler
			int_to_float(self);
			type_float.handle_dyadic_operator(self, op, other);	// TODO - put recursion check around this?
			return;	// float handler has done everything

		case OPID_MODULO:
		case OPID_SHIFTLEFT:
		case OPID_ASR:
			// convert other to int
			float_to_int(other);
			break;
		case OPID_LSR:
		case OPID_AND:
		case OPID_OR:
		case OPID_EOR:
		case OPID_XOR:
			// convert other to int, warning user
			float_to_int(other);
			warn_float_to_int();
			break;
// add new dyadic operators here:
//		case OPID_:
//			break;
		default:
			unsupported_operation(self, op, other);
			return;
		}
// add new types here:
//	} else if (other->type == &type_) {
//		...
	} else {
		unsupported_operation(self, op, other);
		return;
	}
	// maybe put this into an extra "int_dyadic_int" function?
	// sanity check, now "other" must be an int
	if (other->type != &type_int)
		Bug_found("SecondArgIsNotAnInt", op->id);	// FIXME - rename? then add to docs!

	// part 2: now we got rid of non-ints, perform actual operation:
	switch (op->id) {
	case OPID_POWEROF:
		if (other->u.number.val.intval >= 0) {
			self->u.number.val.intval = my_pow(self->u.number.val.intval, other->u.number.val.intval);
		} else {
			if (other->u.number.flags & NUMBER_IS_DEFINED)
				Throw_error("Exponent is negative.");
			self->u.number.val.intval = 0;
		}
		break;
	case OPID_MULTIPLY:
		self->u.number.val.intval *= other->u.number.val.intval;
		break;
	case OPID_DIVIDE:
	case OPID_INTDIV:
		if (other->u.number.val.intval) {
			self->u.number.val.intval /= other->u.number.val.intval;
			break;
		}
		// "division by zero" output is below
		/*FALLTHROUGH*/
	case OPID_MODULO:
		if (other->u.number.val.intval) {
			self->u.number.val.intval %= other->u.number.val.intval;
		} else {
			if (other->u.number.flags & NUMBER_IS_DEFINED)
				Throw_error(exception_div_by_zero);
			self->u.number.val.intval = 0;
		}
		break;
	case OPID_ADD:
		self->u.number.val.intval += other->u.number.val.intval;
		refs = self->u.number.addr_refs + other->u.number.addr_refs;	// add address references
		break;
	case OPID_SUBTRACT:
		self->u.number.val.intval -= other->u.number.val.intval;
		refs = self->u.number.addr_refs - other->u.number.addr_refs;	// subtract address references
		break;
	case OPID_SHIFTLEFT:
		self->u.number.val.intval <<= other->u.number.val.intval;
		break;
	case OPID_ASR:
		self->u.number.val.intval = my_asr(self->u.number.val.intval, other->u.number.val.intval);
		break;
	case OPID_LSR:
		self->u.number.val.intval = ((uintval_t) (self->u.number.val.intval)) >> other->u.number.val.intval;
		break;
	case OPID_LESSOREQUAL:
		self->u.number.val.intval = (self->u.number.val.intval <= other->u.number.val.intval);
		break;
	case OPID_LESSTHAN:
		self->u.number.val.intval = (self->u.number.val.intval < other->u.number.val.intval);
		break;
	case OPID_GREATEROREQUAL:
		self->u.number.val.intval = (self->u.number.val.intval >= other->u.number.val.intval);
		break;
	case OPID_GREATERTHAN:
		self->u.number.val.intval = (self->u.number.val.intval > other->u.number.val.intval);
		break;
	case OPID_NOTEQUAL:
		self->u.number.val.intval = (self->u.number.val.intval != other->u.number.val.intval);
		break;
	case OPID_EQUALS:
		self->u.number.val.intval = (self->u.number.val.intval == other->u.number.val.intval);
		break;
	case OPID_AND:
		self->u.number.val.intval &= other->u.number.val.intval;
		refs = self->u.number.addr_refs + other->u.number.addr_refs;	// add address references
		break;
	case OPID_OR:
		self->u.number.val.intval |= other->u.number.val.intval;
		refs = self->u.number.addr_refs + other->u.number.addr_refs;	// add address references
		break;
	case OPID_EOR:
		Throw_first_pass_warning("\"EOR\" is deprecated; use \"XOR\" instead.");
		/*FALLTHROUGH*/
	case OPID_XOR:
		self->u.number.val.intval ^= other->u.number.val.intval;
		refs = self->u.number.addr_refs + other->u.number.addr_refs;	// add address references
		break;
// add new dyadic operators here
//	case OPID_:
//		break;
	default:
		unsupported_operation(self, op, other);
		return;
	}
	self->u.number.addr_refs = refs;	// update address refs with local copy
	number_fix_result_after_dyadic(self, other);	// fix result flags
}

// float:
// handle dyadic operator
static void float_handle_dyadic_operator(struct object *self, struct op *op, struct object *other)
{
	int	refs	= 0;	// default for "addr_refs", shortens this fn

	// first check type of second arg:
	if (other->type == &type_float) {
		// ok
	} else if (other->type == &type_int) {
		// handle according to operation
		switch (op->id) {
		// these want two floats
		case OPID_POWEROF:
		case OPID_MULTIPLY:
		case OPID_DIVIDE:
		case OPID_INTDIV:
		case OPID_ADD:
		case OPID_SUBTRACT:
		case OPID_LESSOREQUAL:
		case OPID_LESSTHAN:
		case OPID_GREATEROREQUAL:
		case OPID_GREATERTHAN:
		case OPID_NOTEQUAL:
		case OPID_EQUALS:
			// convert other to float
			int_to_float(other);
			break;
		// these jump to int handler anyway
		case OPID_MODULO:
		case OPID_LSR:
		case OPID_AND:
		case OPID_OR:
		case OPID_EOR:
		case OPID_XOR:
		// these actually want a float and an int
		case OPID_SHIFTLEFT:
		case OPID_ASR:
			break;
// add new dyadic operators here
//		case OPID_:
//			break;
		default:
			unsupported_operation(self, op, other);
			return;
		}
// add new types here
//	} else if (other->type == &type_) {
//		...
	} else {
		unsupported_operation(self, op, other);
		return;
	}

	switch (op->id) {
	case OPID_POWEROF:
		self->u.number.val.fpval = pow(self->u.number.val.fpval, other->u.number.val.fpval);
		break;
	case OPID_MULTIPLY:
		self->u.number.val.fpval *= other->u.number.val.fpval;
		break;
	case OPID_DIVIDE:
		if (other->u.number.val.fpval) {
			self->u.number.val.fpval /= other->u.number.val.fpval;
		} else {
			if (other->u.number.flags & NUMBER_IS_DEFINED)
				Throw_error(exception_div_by_zero);
			self->u.number.val.fpval = 0;
		}
		break;
	case OPID_INTDIV:
		if (other->u.number.val.fpval) {
			self->u.number.val.intval = self->u.number.val.fpval / other->u.number.val.fpval;	// fp becomes int!
		} else {
			if (other->u.number.flags & NUMBER_IS_DEFINED)
				Throw_error(exception_div_by_zero);
			self->u.number.val.intval = 0;
		}
		self->type = &type_int;	// result is int
		break;
	case OPID_LSR:
	case OPID_AND:
	case OPID_OR:
	case OPID_EOR:
	case OPID_XOR:
		warn_float_to_int();
		/*FALLTHROUGH*/
	case OPID_MODULO:
		float_to_int(self);
		// int handler will check other and, if needed, convert to int
		type_int.handle_dyadic_operator(self, op, other);	// TODO - put recursion check around this?
		return;	// int handler has done everything

	case OPID_ADD:
		self->u.number.val.fpval += other->u.number.val.fpval;
		refs = self->u.number.addr_refs + other->u.number.addr_refs;	// add address references
		break;
	case OPID_SUBTRACT:
		self->u.number.val.fpval -= other->u.number.val.fpval;
		refs = self->u.number.addr_refs - other->u.number.addr_refs;	// subtract address references
		break;
	case OPID_SHIFTLEFT:
		if (other->type == &type_float)
			float_to_int(other);
		self->u.number.val.fpval *= pow(2.0, other->u.number.val.intval);
		break;
	case OPID_ASR:
		if (other->type == &type_float)
			float_to_int(other);
		self->u.number.val.fpval /= (1 << other->u.number.val.intval);	// FIXME - why not use pow() as in SL above?
		break;
	case OPID_LESSOREQUAL:
		self->u.number.val.intval = (self->u.number.val.fpval <= other->u.number.val.fpval);
		self->type = &type_int;	// result is int
		break;
	case OPID_LESSTHAN:
		self->u.number.val.intval = (self->u.number.val.fpval < other->u.number.val.fpval);
		self->type = &type_int;	// result is int
		break;
	case OPID_GREATEROREQUAL:
		self->u.number.val.intval = (self->u.number.val.fpval >= other->u.number.val.fpval);
		self->type = &type_int;	// result is int
		break;
	case OPID_GREATERTHAN:
		self->u.number.val.intval = (self->u.number.val.fpval > other->u.number.val.fpval);
		self->type = &type_int;	// result is int
		break;
	case OPID_NOTEQUAL:
		self->u.number.val.intval = (self->u.number.val.fpval != other->u.number.val.fpval);
		self->type = &type_int;	// result is int
		break;
	case OPID_EQUALS:
		self->u.number.val.intval = (self->u.number.val.fpval == other->u.number.val.fpval);
		self->type = &type_int;	// result is int
		break;
// add new dyadic operators here
//	case OPID_:
//		break;
	default:
		unsupported_operation(self, op, other);
		return;
	}
	self->u.number.addr_refs = refs;	// update address refs with local copy
	number_fix_result_after_dyadic(self, other);	// fix result flags
}


// helper function for lists and strings, check index
// return zero on success, nonzero on error
static int get_valid_index(int *target, int length, struct object *self, struct op *op, struct object *other)
{
	int	index;

	if (other->type == &type_float)
		float_to_int(other);
	if (other->type != &type_int) {
		unsupported_operation(self, op, other);
		return 1;
	}
	if (!(other->u.number.flags & NUMBER_IS_DEFINED)) {
		Throw_error("Index is undefined.");	// FIXME - add to docs
		return 1;
	}
	index = other->u.number.val.intval;
	// negative indices access from the end
	if (index < 0)
		index += length;
	if ((index < 0) || (index >= length)) {
		Throw_error("Index out of range.");	// FIXME - add to docs
		return 1;
	}
	*target = index;
	return 0;	// ok
}

// list:
// handle dyadic operator
static void list_handle_dyadic_operator(struct object *self, struct op *op, struct object *other)
{
	struct listitem	*item;
	int		length;
	int		index;

	length = self->u.listhead->length;
	switch (op->id) {
	case OPID_LIST_APPEND:
		item = safe_malloc(sizeof(*item));
		item->payload = *other;
		item->next = self->u.listhead;
		item->prev = self->u.listhead->prev;
		item->next->prev = item;
		item->prev->next = item;
		self->u.listhead->length++;
		// no need to check/update ref count of "other": it loses the ref on the stack and gains one in the list
		break;
	case OPID_ATINDEX:
		if (get_valid_index(&index, length, self, op, other))
			return;	// error

		item = self->u.listhead->next;
		while (index) {
			item = item->next;
			--index;
		}
		self->u.listhead->refs--;	// FIXME - call some fn for this
		*self = item->payload;	// FIXME - if item is a list, it would gain a ref by this...
		break;
	default:
		unsupported_operation(self, op, other);
	}
}

// string:
// handle dyadic operator
static void string_handle_dyadic_operator(struct object *self, struct op *op, struct object *other)
{
	int		length;
	int		index;
	intval_t	character;

	length = self->u.string->length;
	switch (op->id) {
	case OPID_ATINDEX:
		if (get_valid_index(&index, length, self, op, other))
			return;	// error

		character = (intval_t) encoding_encode_char(self->u.string->payload[index]);
		self->u.string->refs--;	// FIXME - call a function for this...
		self->type = &type_int;
		self->u.number.flags = NUMBER_IS_DEFINED | NUMBER_FITS_BYTE;
		self->u.number.val.intval = character;
		self->u.number.addr_refs = 0;
		break;
	//case OPID_ADD:	TODO?
	default:
		unsupported_operation(self, op, other);
	}
}

// int/float:
// set flags according to result
static void number_fix_result(struct object *self)
{
	// only allow a single force bit
	if (self->u.number.flags & NUMBER_FORCES_24)
		self->u.number.flags &= ~(NUMBER_FORCES_16 | NUMBER_FORCES_8);
	else if (self->u.number.flags & NUMBER_FORCES_16)
		self->u.number.flags &= ~NUMBER_FORCES_8;
}

// int:
// set flags according to result
static void int_fix_result(struct object *self)
{
	number_fix_result(self);
	// if undefined, return zero
	if (!(self->u.number.flags & NUMBER_IS_DEFINED))
		self->u.number.val.intval = 0;
	// if value is sure, check to set FITS BYTE
	else if ((!(self->u.number.flags & NUMBER_EVER_UNDEFINED))
	&& (self->u.number.val.intval <= 255)
	&& (self->u.number.val.intval >= -128))
		self->u.number.flags |= NUMBER_FITS_BYTE;
}

// float:
// set flags according to result
static void float_fix_result(struct object *self)
{
	number_fix_result(self);
	// if undefined, return zero
	if (!(self->u.number.flags & NUMBER_IS_DEFINED))
		self->u.number.val.fpval = 0;
	// if value is sure, check to set FITS BYTE
	else if ((!(self->u.number.flags & NUMBER_EVER_UNDEFINED))
	&& (self->u.number.val.fpval <= 255.0)
	&& (self->u.number.val.fpval >= -128.0))
		self->u.number.flags |= NUMBER_FITS_BYTE;
}

// list/string:
// no need to fix results
static void object_no_op(struct object *self)
{
}

// int:
// print value for user message
static void int_print(struct object *self, struct dynabuf *db)
{
	char	buffer[32];	// 11 for dec, 8 for hex

	if (self->u.number.flags & NUMBER_IS_DEFINED) {
		sprintf(buffer, "%ld (0x%lx)", (long) self->u.number.val.intval, (long) self->u.number.val.intval);
		DynaBuf_add_string(db, buffer);
	} else {
		DynaBuf_add_string(db, "<UNDEFINED INT>");
	}
}

// float:
// print value for user message
static void float_print(struct object *self, struct dynabuf *db)
{
	char	buffer[40];

	if (self->u.number.flags & NUMBER_IS_DEFINED) {
		// write up to 30 significant characters.
		// remaining 10 should suffice for sign,
		// decimal point, exponent, terminator etc.
		sprintf(buffer, "%.30g", self->u.number.val.fpval);
		DynaBuf_add_string(db, buffer);
	} else {
		DynaBuf_add_string(db, "<UNDEFINED FLOAT>");
	}
}

// list:
// print value for user message
static void list_print(struct object *self, struct dynabuf *db)
{
	char	buffer[64];	// 20 + 2*20 for 64-bit numbers, 64 bytes should be enough for anybody

	sprintf(buffer, "<LIST (len %ld, refs %ld)>", (long) self->u.listhead->length, (long) self->u.listhead->refs);
	DynaBuf_add_string(db, buffer);
}

// string:
// print value for user message
static void string_print(struct object *self, struct dynabuf *db)
{
	char	buffer[64];	// 20 + 2*20 for 64-bit numbers, 64 bytes should be enough for anybody

	sprintf(buffer, "<STRING (len %ld, refs %ld)>", (long) self->u.string->length, (long) self->u.string->refs);
	DynaBuf_add_string(db, buffer);
}

struct type	type_int	= {
	"integer",
	number_is_defined,
	int_handle_monadic_operator,
	int_handle_dyadic_operator,
	int_fix_result,
	int_print
};
struct type	type_float	= {
	"float",
	number_is_defined,
	float_handle_monadic_operator,
	float_handle_dyadic_operator,
	float_fix_result,
	float_print
};
struct type	type_list	= {
	"list",
	object_return_true,	// lists are always considered to be defined (even though they can hold undefined numbers...)
	list_handle_monadic_operator,
	list_handle_dyadic_operator,
	object_no_op,	// no need to fix list results
	list_print
};
struct type	type_string	= {
	"string",
	object_return_true,	// strings are always defined
	string_handle_monadic_operator,
	string_handle_dyadic_operator,
	object_no_op,	// no need to fix string results
	string_print
};


// handler for special operators like parentheses and start/end of expression:
// returns whether caller can remove "previous" operator from stack
static boolean handle_special_operator(struct expression *expression, enum op_id previous, enum op_id current)
{
	// when this gets called, "previous" is a special operator, and "current" has a lower priority, so it is also a special operator
	switch (previous) {
	case OPID_START_EXPRESSION:
		// the only operator with a lower priority than this
		// "start-of-expression" operator is "end-of-expression",
		// therefore we know we are done.
		// don't touch "is_parenthesized", because start/end are obviously not "real" operators
		alu_state = STATE_END;	// done
		return TRUE;	// caller can remove this operator (we are done, so not really needed, but there are sanity checks for stack pointers)

	case OPID_LEFT_PARENTHESIS:
		expression->is_parenthesized = TRUE;	// found parentheses. if this is not the outermost level, the outermost level will fix this flag later on.
		if (current != OPID_END_EXPRESSION)
			Bug_found("StrangeParenthesis", current);
		if (GotByte == ')') {
			// matching parenthesis
			GetByte();	// eat char
			op_sp -= 2;	// remove both operators
			alu_state = STATE_EXPECT_DYADIC_OP;
			return FALSE;	// we fixed the stack ourselves, so caller shouldn't touch it
		}
		// unmatched parenthesis, as in "lda ($80,x)"
		++(expression->open_parentheses);	// count
		return TRUE;	// caller can remove "OPID_LEFT_PARENTHESIS" operator from stack
//		Throw_error("Too many ')'.");	// FIXME - remove from docs!

	case OPID_START_LIST:
		if (current != OPID_END_EXPRESSION)
			Bug_found("StrangeListBracket", current);	// FIXME - add to docs!
		if (GotByte == ',') {
			GetByte();	// eat ','
			op_stack[op_sp - 1] = &ops_list_append;	// change "end of expression" to "append"
			alu_state = STATE_EXPECT_ARG_OR_MONADIC_OP;
			return FALSE;	// stack remains, so caller shouldn't touch it
		}
		if (GotByte == ']') {
			GetByte();	// eat ']'
			op_sp -= 2;	// remove both START_LIST and END_EXPRESSION
			alu_state = STATE_EXPECT_DYADIC_OP;
			return FALSE;	// we fixed the stack ourselves, so caller shouldn't touch it
		}
		Throw_error("Unterminated list");	// FIXME - add to docs!
		alu_state = STATE_ERROR;
		return TRUE;	// caller can remove LISTBUILDER operator from stack

	case OPID_START_INDEX:
		if (current != OPID_END_EXPRESSION)
			Bug_found("StrangeIndexBracket", current);	// FIXME - add to docs!
		if (GotByte == ']') {
			GetByte();	// eat ']'
			op_sp -= 2;	// remove both OPENINDEX and END_EXPRESSION
			alu_state = STATE_EXPECT_DYADIC_OP;
			return FALSE;	// we fixed the stack ourselves, so caller shouldn't touch it
		}
		Throw_error("Unterminated index spec");	// FIXME - add to docs!
		alu_state = STATE_ERROR;
		return TRUE;	// caller can remove START_INDEX operator from stack

	default:
		Bug_found("IllegalOperatorIdS", previous);
	}
	// this is unreachable
	return FALSE;	// stack is done, so caller shouldn't touch it
}


// Try to reduce stacks by performing high-priority operations
// (if the previous operator has a higher priority than the current one, do it)
static void try_to_reduce_stacks(struct expression *expression)
{
	struct op	*previous_op;
	struct op	*current_op;

	if (op_sp < 2) {
		// we only have one operator, which must be "start of expression",
		// so there isn't anything left to do, so go on trying to parse the expression
		alu_state = STATE_EXPECT_ARG_OR_MONADIC_OP;
		return;
	}

	previous_op = op_stack[op_sp - 2];
	current_op = op_stack[op_sp - 1];

	// previous operator has lower piority than current one? then do nothing.
	if (previous_op->priority < current_op->priority) {
		alu_state = STATE_EXPECT_ARG_OR_MONADIC_OP;
		return;
	}

	// previous operator has same priority as current one? then check associativity
	if ((previous_op->priority == current_op->priority)
	&& IS_RIGHT_ASSOCIATIVE(current_op->priority)) {
		alu_state = STATE_EXPECT_ARG_OR_MONADIC_OP;
		return;
	}

	// we now know that either
	// - the previous operator has higher priority, or
	// - it has the same priority and is left-associative,
	// so perform that operation!
#define ARG_PREV	(arg_stack[arg_sp - 2])
#define ARG_NOW		(arg_stack[arg_sp - 1])
	switch (previous_op->group) {
	case OPGROUP_MONADIC:	// monadic operators
		if (arg_sp < 1)
			Bug_found("ArgStackEmpty", arg_sp);	// FIXME - add to docs!
		ARG_NOW.type->handle_monadic_operator(&ARG_NOW, previous_op);
		// operation was something other than parentheses
		expression->is_parenthesized = FALSE;
		break;
	case OPGROUP_DYADIC:	// dyadic operators
		if (arg_sp < 2)
			Bug_found("NotEnoughArgs", arg_sp);	// FIXME - add to docs!
		ARG_PREV.type->handle_dyadic_operator(&ARG_PREV, previous_op, &ARG_NOW);
		// decrement argument stack pointer because dyadic operator merged two arguments into one
		--arg_sp;
		// operation was something other than parentheses
		expression->is_parenthesized = FALSE;
		break;
	case OPGROUP_SPECIAL:	// special (pseudo) operators
		if (!handle_special_operator(expression, previous_op->id, current_op->id))
			return;	// called fn has fixed the stack, so we return and don't touch it

		// both monadics and dyadics clear "is_parenthesized", but here we don't touch it!
		break;
	default:
		Bug_found("IllegalOperatorGroup", previous_op->group);	// FIXME - add to docs!
	}
// shared endings for "we did the operation indicated by previous operator":
	// fix stack:
	// remove previous operator and shift down current one
	// CAUTION - fiddling with our local copies like "previous_op = current_op" is not enough... ;)
	op_stack[op_sp - 2] = op_stack[op_sp - 1];
	--op_sp;	// decrement operator stack pointer
}


// this is what the exported functions call
static void parse_expression(struct expression *expression)
{
	struct object	*result	= &expression->result;

	// init
	expression->is_empty = TRUE;	// becomes FALSE when first valid char gets parsed
	expression->open_parentheses = 0;
	expression->is_parenthesized = FALSE;	// toplevel operator will set this: '(' to TRUE, all others to FALSE
	//expression->number will be overwritten later, so no need to init

	op_sp = 0;	// operator stack pointer
	arg_sp = 0;	// argument stack pointer
	// begin by reading an argument (or a monadic operator)
	alu_state = STATE_EXPECT_ARG_OR_MONADIC_OP;
	PUSH_OP(&ops_start_expression);
	do {
		// check stack sizes. enlarge if needed
		if (arg_sp >= argstack_size)
			enlarge_argument_stack();
		switch (alu_state) {
		case STATE_EXPECT_ARG_OR_MONADIC_OP:
			if (expect_argument_or_monadic_operator())
				expression->is_empty = FALSE;
			break;
		case STATE_EXPECT_DYADIC_OP:
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
		if (arg_sp != 1)
			Bug_found("OperandStackNotEmpty", arg_sp);
		if (op_sp != 1)
			Bug_found("OperatorStackNotEmpty", op_sp);
		// copy result
		*result = arg_stack[0];
		// if there was nothing to parse, mark as undefined	FIXME - change this! make "nothing" its own result type; only numbers may be undefined
		// (so ALU_defined_int() can react)
		if (expression->is_empty) {
			result->type = &type_int;
			result->u.number.flags = NUMBER_EVER_UNDEFINED;	// ...and without NUMBER_IS_DEFINED!
			result->u.number.val.intval = 0;
			result->u.number.addr_refs = 0;
		} else {
			// not empty. undefined?
			if (!(result->type->is_defined(result))) {
				// then count (in all passes)
				++pass.undefined_count;
			}
		}
		// do some checks depending on int/float
		result->type->fix_result(result);
	} else {
		// State is STATE_ERROR. Errors have already been reported,
		// but we must make sure not to pass bogus data to caller.
		// FIXME - just use the return value to indicate "there were errors, do not use result!"
		result->type = &type_int;
		result->u.number.flags = 0;	// maybe set DEFINED flag to suppress follow-up errors?
		result->u.number.val.intval = 0;
		result->u.number.addr_refs = 0;
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
	if (expression.result.type == &type_int)
		return expression.result.u.number.val.intval;

	if (expression.result.type == &type_float)
		return expression.result.u.number.val.fpval;

	Bug_found("Unhandled object type!", 0);
	return 0;	// inhibit compiler warning
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
	if (expression.open_parentheses)
		Throw_error(exception_paren_open);
	if (expression.is_empty)
		Throw_serious_error(exception_no_value);
	if (expression.result.type == &type_int) {
		// ok
	} else if (expression.result.type == &type_float) {
		float_to_int(&expression.result);
	} else {
		Bug_found("Unhandled object type!", 1);
	}
	if (!(expression.result.u.number.flags & NUMBER_IS_DEFINED))
		Throw_serious_error(exception_value_not_defined);
	*intresult = expression.result.u.number;
}


// Store int value and flags.
// This function allows for "paren" '(' too many. Needed when parsing indirect
// addressing modes where internal indices have to be possible.
// For empty expressions, an error is thrown.
// OPEN_PARENTHESIS: depends on arg
// UNDEFINED: allow
// EMPTY: complain
// FLOAT: convert to int
void ALU_addrmode_int(struct expression *expression, int paren)	// ACCEPT_UNDEFINED | ACCEPT_OPENPARENTHESIS
{
	parse_expression(expression);
	// convert float to int
	if (expression->result.type == &type_float)
		float_to_int(&(expression->result));
	if (expression->result.type != &type_int)
		Bug_found("Unhandled object type!", 2);
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
void ALU_any_result(struct object *result)	// ACCEPT_UNDEFINED | ACCEPT_FLOAT
{
	struct expression	expression;

	parse_expression(&expression);
	*result = expression.result;
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
		when parsing addressing modes						needvalue!

// stores value and flags (result may be either int or float)
void ALU_any_result(struct object *result)
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
