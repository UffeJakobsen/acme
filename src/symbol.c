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
	if ((symbol->object.type != &type_int)
	&& (symbol->object.type != &type_float))
		return;

	// CAUTION: if more types are added, check for NULL before using type pointer!

	// output name
	if (config.warn_on_type_mismatch
	&& symbol->object.u.number.addr_refs == 1)
		fprintf(fd, "!addr");
	fprintf(fd, "\t%s", node->id_string);
	switch (symbol->object.u.number.flags & NUMBER_FORCEBITS) {
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
	if (symbol->object.u.number.flags & NUMBER_IS_DEFINED) {
		if (symbol->object.type == &type_int)
			fprintf(fd, "$%x", (unsigned) symbol->object.u.number.val.intval);
		else if (symbol->object.type == &type_float)
			fprintf(fd, "%.30f", symbol->object.u.number.val.fpval);	//FIXME %g
		else
			Bug_found("BogusType", 0);	// FIXME - put in docs!
	} else {
		fprintf(fd, " ?");	// TODO - maybe write "UNDEFINED" instead? then the file could at least be parsed without errors
	}
	if (symbol->object.u.number.flags & NUMBER_EVER_UNDEFINED)
		fprintf(fd, "\t; ?");	// TODO - write "forward" instead?
	if (!symbol->has_been_read)
		fprintf(fd, "\t; unused");
	fprintf(fd, "\n");
}


// output symbols in VICE format (example: "al C:09ae .nmi1")
static void dump_vice_address(struct rwnode *node, FILE *fd)
{
	struct symbol	*symbol	= node->body;

	// dump address symbols even if they are not used
	if ((symbol->object.type == &type_int)
	&& (symbol->object.u.number.flags & NUMBER_IS_DEFINED)
	&& (symbol->object.u.number.addr_refs == 1))
		fprintf(fd, "al C:%04x .%s\n", (unsigned) symbol->object.u.number.val.intval, node->id_string);
}
static void dump_vice_usednonaddress(struct rwnode *node, FILE *fd)
{
	struct symbol	*symbol	= node->body;

	// dump non-addresses that are used
	if (symbol->has_been_read
	&& (symbol->object.type == &type_int)
	&& (symbol->object.u.number.flags & NUMBER_IS_DEFINED)
	&& (symbol->object.u.number.addr_refs != 1))
		fprintf(fd, "al C:%04x .%s\n", (unsigned) symbol->object.u.number.val.intval, node->id_string);
}
static void dump_vice_unusednonaddress(struct rwnode *node, FILE *fd)
{
	struct symbol	*symbol	= node->body;

	// dump non-addresses that are unused
	if (!symbol->has_been_read
	&& (symbol->object.type == &type_int)
	&& (symbol->object.u.number.flags & NUMBER_IS_DEFINED)
	&& (symbol->object.u.number.addr_refs != 1))
		fprintf(fd, "al C:%04x .%s\n", (unsigned) symbol->object.u.number.val.intval, node->id_string);
}


// search for symbol. if it does not exist, create with NULL object (CAUTION!).
// the symbol name must be held in GlobalDynaBuf.
struct symbol *symbol_find(scope_t scope)
{
	struct rwnode	*node;
	struct symbol	*symbol;
	boolean		node_created;

	node_created = Tree_hard_scan(&node, symbols_forest, scope, TRUE);
	// if node has just been created, create symbol as well
	if (node_created) {
		// create new symbol structure
		symbol = safe_malloc(sizeof(*symbol));
		node->body = symbol;
		// finish empty symbol item
		symbol->object.type = NULL;	// no object yet (CAUTION!)
		symbol->pass = pass.number;
		symbol->has_been_read = FALSE;
		symbol->has_been_reported = FALSE;
		symbol->pseudopc = NULL;
	} else {
		symbol = node->body;
	}
	return symbol;	// now symbol->object.type can be tested to see if this was freshly created.
	// CAUTION: this only works if caller always sets a type pointer after checking! if NULL is kept, the struct still looks new later on...
}


// FIXME - temporary helper function during refactoring
static void symbol_forcebit(struct symbol *symbol, int force_bit)
{
	// if symbol has no object assigned to it, make it an int
	if (symbol->object.type == NULL) {
		// finish empty symbol item
		symbol->object.type = &type_int;
		symbol->object.u.number.flags = force_bit;
		symbol->object.u.number.addr_refs = 0;
		symbol->object.u.number.val.intval = 0;
	} else {
		// make sure the force bits don't clash
		if ((symbol->object.type == &type_int)
		|| (symbol->object.type == &type_float)) {
			if (force_bit
			&& force_bit != (symbol->object.u.number.flags & NUMBER_FORCEBITS))
				Throw_error("Too late for postfix.");
		}
	}
}
// assign value to symbol. the function acts upon the symbol's flag bits and
// produces an error if needed.
// TODO - split checks into two parts: first deal with object type. in case of number, then check value/flags/whatever
static void symbol_set_object(struct symbol *symbol, struct object *new_value, boolean change_allowed)	// FIXME - does "change_allowed" refer to type change or number value change?
{
	int	flags;	// for int/float re-definitions

	// any new type?
	if (((symbol->object.type != &type_int) && (symbol->object.type != &type_float))
	|| ((new_value->type != &type_int) && (new_value->type != &type_float))) {
		// changing value is ok, changing type needs extra flag:
		if (change_allowed || (symbol->object.type == new_value->type))
			symbol->object = *new_value;
		else
			Throw_error("Symbol already defined.");
		return;
	}

// FIXME - force bits assigned via !for or !set are lost, because due to "change_allowed", the new object struct is copied and that's it!

	// both old and new are either int or float, so keep old algo:

	// value stuff
	flags = symbol->object.u.number.flags;
	if (change_allowed || !(flags & NUMBER_IS_DEFINED)) {
		// symbol is not defined yet OR redefinitions are allowed
		symbol->object = *new_value;
	} else {
		// symbol is already defined, so compare new and old values
		// if different type OR same type but different value, complain
		if ((symbol->object.type != new_value->type)
		|| ((symbol->object.type == &type_float) ? (symbol->object.u.number.val.fpval != new_value->u.number.val.fpval) : (symbol->object.u.number.val.intval != new_value->u.number.val.intval)))
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
		if ((flags & NUMBER_FORCEBITS) == 0) {
			if ((flags & (NUMBER_EVER_UNDEFINED | NUMBER_IS_DEFINED)) == 0) {
				// FIXME - this can't happen!?	Yes, it can!
				// if the symbol was created just now to be assigned a value,
				// then both flags are clear before the assignment takes place.
				// in future this should no longer happen, because creating a
				// symbol will give it the NULL type, and flags will only
				// matter if it then gets assigned an int or float value.
				flags |= new_value->u.number.flags & NUMBER_FORCEBITS;
			}
		}
		flags |= new_value->u.number.flags & ~NUMBER_FORCEBITS;
	}
	symbol->object.u.number.flags = flags;
}
// FIXME - temporary helper function during refactoring
// used for:
//	(implicit!) label definitions, including anons	(FIXME - anons cannot have force bits. handle them elsewhere? change backward anons directly, no questions asked?)
//	setting up loop counter for "!for" (CAUTION: actual incrementing is then done directly without calling this function!)
// "change_allowed" is used by backward anons, but then force_bit is 0
// "change_allowed" is also used by "!for", then force_bit may be nonzero
void symbol_set_object2(struct symbol *symbol, struct object *result, int force_bit, boolean change_allowed)
{
	symbol_forcebit(symbol, force_bit);	// TODO - "if NULL object, make int" and "if not int, complain"
	symbol_set_object(symbol, result, change_allowed);	// FIXME - "backward anon allows number redef" is different from "!set allows object redef"!
}
// FIXME - temporary helper function during refactoring
// used for:
//	explicit assignments, including "!set"
// "po_set" means "!set", so changes are allowed
void symbol_set_object3(struct symbol *symbol, struct object *result, int force_bit, boolean po_set)
{
	// FIXME - force bit can only be used if result is number! check!
	symbol_forcebit(symbol, force_bit);
	// if this was called by !set, new force bit replaces old one:
	if (po_set) {
		// clear symbol's force bits and set new ones
		// (but only do this for numbers!)
		if (((symbol->object.type == &type_int) || (symbol->object.type == &type_float))
		&& ((result->type == &type_int) || (result->type == &type_float))) {
			// clear symbol's size flags, set new one, clear result's size flags
			symbol->object.u.number.flags &= ~(NUMBER_FORCEBITS | NUMBER_FITS_BYTE);
			if (force_bit) {
				symbol->object.u.number.flags |= force_bit;
				result->u.number.flags &= ~(NUMBER_FORCEBITS | NUMBER_FITS_BYTE);
			}
		}
		// FIXME - take a good look at the flags handling above and in the fn called below and clean this up!
	}
	symbol_set_object(symbol, result, po_set);
}


// set global symbol to integer value, no questions asked (for "-D" switch)
// Name must be held in GlobalDynaBuf.
void symbol_define(intval_t value)
{
	struct object	result;
	struct symbol	*symbol;

	result.type = &type_int;
	result.u.number.flags = NUMBER_IS_DEFINED;
	result.u.number.val.intval = value;
	symbol = symbol_find(SCOPE_GLOBAL);
	symbol->object = result;
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
// example:
//	forward anon name is "+++"
//	we look up that symbol's value in the current local scope -> $12
//	we attach hex digits to name -> "+++21"
//	that's the name of the symbol that _actually_ contains the address
// caller sets "increment" to TRUE for writing, FALSE for reading
void symbol_fix_forward_anon_name(boolean increment)
{
	struct symbol	*counter_symbol;
	unsigned long	number;

	// terminate name, find "counter" symbol and read value
	DynaBuf_append(GlobalDynaBuf, '\0');
	counter_symbol = symbol_find(section_now->local_scope);
	if (counter_symbol->object.type == NULL) {
		// finish freshly created symbol item
		counter_symbol->object.type = &type_int;
		counter_symbol->object.u.number.flags = NUMBER_IS_DEFINED;
		counter_symbol->object.u.number.addr_refs = 0;
		counter_symbol->object.u.number.val.intval = 0;
	} else if (counter_symbol->object.type != &type_int) {
		// sanity check: it must be an int!
		Bug_found("ForwardAnonCounterNotInt", 0);
	}
	// make sure it gets reset to zero in each new pass
	if (counter_symbol->pass != pass.number) {
		counter_symbol->pass = pass.number;
		counter_symbol->object.u.number.val.intval = 0;
	}
	number = (unsigned long) counter_symbol->object.u.number.val.intval;
	// now append to the name to make it unique
	GlobalDynaBuf->size--;	// forget terminator, we want to append
	do {
		DYNABUF_APPEND(GlobalDynaBuf, 'a' + (number & 15));
		number >>= 4;
	} while (number);
	DynaBuf_append(GlobalDynaBuf, '\0');
	if (increment)
		counter_symbol->object.u.number.val.intval++;
}
