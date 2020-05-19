// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2020 Marco Baye
// Have a look at "acme.c" for further info
//
// symbol stuff
//
// 22 Nov 2007	"warn on indented labels" is now a CLI switch
// 25 Sep 2011	Fixed bug in !sl (colons in filename could be interpreted as EOS)
// 23 Nov 2014	Added label output in VICE format
#include "symbol.h"
#include <stdio.h>
#include "acme.h"
#include "alu.h"
#include "dynabuf.h"
#include "global.h"
#include "input.h"
#include "output.h"
#include "platform.h"
#include "section.h"
#include "tree.h"
#include "typesystem.h"


// variables
struct rwnode	*symbols_forest[256]	= { NULL };	// because of 8-bit hash - must be (at least partially) pre-defined so array will be zeroed!


// Dump symbol value and flags to dump file
static void dump_one_symbol(struct rwnode *node, FILE *fd)
{
	struct symbol	*symbol	= node->body;

	// if symbol is neither int nor float, skip
	if ((symbol->result.type != &type_int)
	&& (symbol->result.type != &type_float))
		return;

	// output name
	if (config.warn_on_type_mismatch
	&& symbol->result.u.number.addr_refs == 1)
		fprintf(fd, "!addr");
	fprintf(fd, "\t%s", node->id_string);
	switch (symbol->result.u.number.flags & NUMBER_FORCEBITS) {
	case NUMBER_FORCES_16:
		fprintf(fd, "+2\t= ");
		break;
	case NUMBER_FORCES_16 | NUMBER_FORCES_24:
		/*FALLTHROUGH*/
	case NUMBER_FORCES_24:
		fprintf(fd, "+3\t= ");
		break;
	default:
		fprintf(fd, "\t= ");
	}
	if (symbol->result.u.number.flags & NUMBER_IS_DEFINED) {
		if (symbol->result.type == &type_int)
			fprintf(fd, "$%x", (unsigned) symbol->result.u.number.val.intval);
		else if (symbol->result.type == &type_float)
			fprintf(fd, "%.30f", symbol->result.u.number.val.fpval);	//FIXME %g
		else
			Bug_found("BogusType", 0);	// FIXME - put in docs!
	} else {
		fprintf(fd, " ?");	// TODO - maybe write "UNDEFINED" instead? then the file could at least be parsed without errors
	}
	if (symbol->result.u.number.flags & NUMBER_EVER_UNDEFINED)
		fprintf(fd, "\t; ?");	// TODO - write "forward" instead?
	if (symbol->usage == 0)
		fprintf(fd, "\t; unused");
	fprintf(fd, "\n");
}


// output symbols in VICE format (example: "al C:09ae .nmi1")
static void dump_vice_address(struct rwnode *node, FILE *fd)
{
	struct symbol	*symbol	= node->body;

	// dump address symbols even if they are not used
	if ((symbol->result.type == &type_int)
	&& (symbol->result.u.number.flags & NUMBER_IS_DEFINED)
	&& (symbol->result.u.number.addr_refs == 1))
		fprintf(fd, "al C:%04x .%s\n", (unsigned) symbol->result.u.number.val.intval, node->id_string);
}
static void dump_vice_usednonaddress(struct rwnode *node, FILE *fd)
{
	struct symbol	*symbol	= node->body;

	// dump non-addresses that are used
	if (symbol->usage
	&& (symbol->result.type == &type_int)
	&& (symbol->result.u.number.flags & NUMBER_IS_DEFINED)
	&& (symbol->result.u.number.addr_refs != 1))
		fprintf(fd, "al C:%04x .%s\n", (unsigned) symbol->result.u.number.val.intval, node->id_string);
}
static void dump_vice_unusednonaddress(struct rwnode *node, FILE *fd)
{
	struct symbol	*symbol	= node->body;

	// dump non-addresses that are unused
	if (!symbol->usage
	&& (symbol->result.type == &type_int)
	&& (symbol->result.u.number.flags & NUMBER_IS_DEFINED)
	&& (symbol->result.u.number.addr_refs != 1))
		fprintf(fd, "al C:%04x .%s\n", (unsigned) symbol->result.u.number.val.intval, node->id_string);
}


// search for symbol. create if nonexistant. if created, give it flags "flags".
// the symbol name must be held in GlobalDynaBuf.
struct symbol *symbol_find(scope_t scope, int flags)
{
	struct rwnode	*node;
	struct symbol	*symbol;
	boolean		node_created;
	int		new_force_bits;

	node_created = Tree_hard_scan(&node, symbols_forest, scope, TRUE);
	// if node has just been created, create symbol as well
	if (node_created) {
		// create new symbol structure
		symbol = safe_malloc(sizeof(*symbol));
		// finish empty symbol item
		symbol->result.type = &type_int;
		symbol->result.u.number.flags = flags;
		symbol->result.u.number.addr_refs = 0;
		symbol->result.u.number.val.intval = 0;
		symbol->usage = 0;	// usage count
		symbol->pass = pass.number;
		symbol->has_been_reported = FALSE;
		node->body = symbol;
	} else {
		symbol = node->body;
		// make sure the force bits don't clash
		if ((symbol->result.type == &type_int)
		|| (symbol->result.type == &type_float)) {
			new_force_bits = flags & NUMBER_FORCEBITS;
			if (new_force_bits)
				if ((symbol->result.u.number.flags & NUMBER_FORCEBITS) != new_force_bits)
					Throw_error("Too late for postfix.");
		}
	}
	return symbol;
}


// assign value to symbol. the function acts upon the symbol's flag bits and
// produces an error if needed.
void symbol_set_object(struct symbol *symbol, struct object *new_value, boolean change_allowed)
{
	int	flags;	// for int/float re-definitions

	// any new type?
	if (((symbol->result.type != &type_int) && (symbol->result.type != &type_float))
	|| ((new_value->type != &type_int) && (new_value->type != &type_float))) {
		// changing value is ok, changing type needs extra flag:
		if (change_allowed || (symbol->result.type == new_value->type))
			symbol->result = *new_value;
		else
			Throw_error("Symbol already defined.");
		return;
	}

	// both old and new are either int or float, so keep old algo:

	// value stuff
	flags = symbol->result.u.number.flags;
	if (change_allowed || !(flags & NUMBER_IS_DEFINED)) {
		// symbol is not defined yet OR redefinitions are allowed
		symbol->result = *new_value;
	} else {
		// symbol is already defined, so compare new and old values
		// if different type OR same type but different value, complain
		if ((symbol->result.type != new_value->type)
		|| ((symbol->result.type == &type_float) ? (symbol->result.u.number.val.fpval != new_value->u.number.val.fpval) : (symbol->result.u.number.val.intval != new_value->u.number.val.intval)))
			Throw_error("Symbol already defined.");
	}
	// flags stuff
	// Ensure that "unsure" symbols without "isByte" state don't get that
	if ((flags & (NUMBER_EVER_UNDEFINED | NUMBER_FITS_BYTE)) == NUMBER_EVER_UNDEFINED)
		new_value->u.number.flags &= ~NUMBER_FITS_BYTE;

	if (change_allowed) {
		// take flags from new value, then OR EVER_UNDEFINED from old value
		flags = (flags & NUMBER_EVER_UNDEFINED) | new_value->u.number.flags;
	} else {
		if ((flags & NUMBER_FORCEBITS) == 0)
			if ((flags & (NUMBER_EVER_UNDEFINED | NUMBER_IS_DEFINED)) == 0)	// FIXME - this can't happen!?
				flags |= new_value->u.number.flags & NUMBER_FORCEBITS;
		flags |= new_value->u.number.flags & ~NUMBER_FORCEBITS;
	}
	symbol->result.u.number.flags = flags;
}


// parse label definition (can be either global or local).
// name must be held in GlobalDynaBuf.
void symbol_set_label(scope_t scope, int stat_flags, int force_bit, boolean change_allowed)
{
	struct number	pc;
	struct object	result;
	struct symbol	*symbol;

	symbol = symbol_find(scope, force_bit);
	// label definition
	if ((stat_flags & SF_FOUND_BLANK) && config.warn_on_indented_labels)
		Throw_first_pass_warning("Label name not in leftmost column.");
	vcpu_read_pc(&pc);
	// FIXME - if undefined, check pass.complain_about_undefined and maybe throw "value not defined"!
	result.type = &type_int;
	result.u.number.flags = pc.flags & NUMBER_IS_DEFINED;
	result.u.number.val.intval = pc.val.intval;
	result.u.number.addr_refs = pc.addr_refs;
	symbol_set_object(symbol, &result, change_allowed);
	// global labels must open new scope for cheap locals
	if (scope == SCOPE_GLOBAL)
		section_new_cheap_scope(section_now);
}


// parse symbol definition (can be either global or local, may turn out to be a label).
// name must be held in GlobalDynaBuf.
void symbol_parse_definition(scope_t scope, int stat_flags)
{
	struct object	result;
	struct symbol	*symbol;
	int		force_bit	= Input_get_force_bit();	// skips spaces after
	// FIXME - force bit is allowed for label definitions?!

	if (GotByte == '=') {
		// explicit symbol definition (symbol = <something>)
		symbol = symbol_find(scope, force_bit);
		// symbol = parsed value
		GetByte();	// skip '='
		ALU_any_result(&result);
		// if wanted, mark as address reference
		if (typesystem_says_address()) {
			// FIXME - checking types explicitly is ugly...
			if ((result.type == &type_int)
			|| (result.type == &type_float))
				result.u.number.addr_refs = 1;
		}
		symbol_set_object(symbol, &result, FALSE);
		Input_ensure_EOS();
	} else {
		// implicit symbol definition (label)
		symbol_set_label(scope, stat_flags, force_bit, FALSE);
	}
}


// set global symbol to value, no questions asked (for "-D" switch)
// Name must be held in GlobalDynaBuf.
void symbol_define(intval_t value)
{
	struct object	result;
	struct symbol	*symbol;

	result.type = &type_int;
	result.u.number.flags = NUMBER_IS_DEFINED;
	result.u.number.val.intval = value;
	symbol = symbol_find(SCOPE_GLOBAL, 0);
	symbol_set_object(symbol, &result, TRUE);
}


// dump global symbols to file
void symbols_list(FILE *fd)
{
	Tree_dump_forest(symbols_forest, SCOPE_GLOBAL, dump_one_symbol, fd);
}


void symbols_vicelabels(FILE *fd)
{
	// FIXME - if type checking is enabled, maybe only output addresses?
	// the order of dumped labels is important because VICE will prefer later defined labels
	// dump unused labels
	Tree_dump_forest(symbols_forest, SCOPE_GLOBAL, dump_vice_unusednonaddress, fd);
	fputc('\n', fd);
	// dump other used labels
	Tree_dump_forest(symbols_forest, SCOPE_GLOBAL, dump_vice_usednonaddress, fd);
	fputc('\n', fd);
	// dump address symbols
	Tree_dump_forest(symbols_forest, SCOPE_GLOBAL, dump_vice_address, fd);
}


// fix name of anonymous forward label (held in DynaBuf, NOT TERMINATED!) so it
// references the *next* anonymous forward label definition. The tricky bit is,
// each name length would need its own counter. But hey, ACME's real quick in
// finding symbols, so I'll just abuse the symbol system to store those counters.
void symbol_fix_forward_anon_name(boolean increment)
{
	struct symbol	*counter_symbol;
	unsigned long	number;

	// terminate name, find "counter" symbol and read value
	DynaBuf_append(GlobalDynaBuf, '\0');
	counter_symbol = symbol_find(section_now->local_scope, 0);
	// sanity check: it must be an int!
	if (counter_symbol->result.type != &type_int)
		Bug_found("ForwardAnonCounterNotInt", 0);
	// make sure it gets reset to zero in each new pass
	if (counter_symbol->pass != pass.number) {
		counter_symbol->pass = pass.number;
		counter_symbol->result.u.number.val.intval = 0;
	}
	number = (unsigned long) counter_symbol->result.u.number.val.intval;
	// now append to the name to make it unique
	GlobalDynaBuf->size--;	// forget terminator, we want to append
	do {
		DYNABUF_APPEND(GlobalDynaBuf, 'a' + (number & 15));
		number >>= 4;
	} while (number);
	DynaBuf_append(GlobalDynaBuf, '\0');
	if (increment)
		counter_symbol->result.u.number.val.intval++;
}
