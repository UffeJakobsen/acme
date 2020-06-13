// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2020 Marco Baye
// Have a look at "acme.c" for further info
//
// Flow control stuff (loops, conditional assembly etc.)
//
// Macros, conditional assembly, loops and sourcefile-includes are all based on
// parsing blocks of code. When defining macros or using loops or conditional
// assembly, the block starts with "{" and ends with "}". In the case of
// "!source", the given file is treated like a block - the outermost assembler
// function uses the same technique to parse the top level file.
//
// 24 Nov 2007	Added "!ifndef"
#include "flow.h"
#include <string.h>
#include "acme.h"
#include "alu.h"
#include "config.h"
#include "dynabuf.h"
#include "global.h"
#include "input.h"
#include "mnemo.h"
#include "symbol.h"
#include "tree.h"


// helper functions for "!for" and "!do"

// parse a loop body (TODO - also use for macro body?)
static void parse_ram_block(struct block *block)
{
	Input_now->line_number = block->start;	// set line number to loop start
	Input_now->src.ram_ptr = block->body;	// set RAM read pointer to loop
	// parse block
	Parse_until_eob_or_eof();
	if (GotByte != CHAR_EOB)
		Bug_found("IllegalBlockTerminator", GotByte);
}


// back end function for "!for" pseudo opcode
void flow_forloop(struct for_loop *loop)
{
	struct input	loop_input,
			*outer_input;
	struct object	loop_counter;

	// switching input makes us lose GotByte. But we know it's '}' anyway!
	// set up new input
	loop_input = *Input_now;	// copy current input structure into new
	loop_input.source = INPUTSRC_RAM;	// set new byte source
	// remember old input
	outer_input = Input_now;
	// activate new input
	// (not yet useable; pointer and line number are still missing)
	Input_now = &loop_input;
	// fix line number (not for block, but in case symbol handling throws errors)
	Input_now->line_number = loop->block.start;
	// init counter
	loop_counter.type = &type_int;
	loop_counter.u.number.flags = NUMBER_IS_DEFINED;
	loop_counter.u.number.val.intval = loop->counter.first;
	loop_counter.u.number.addr_refs = loop->counter.addr_refs;
	// CAUTION: next line does not have power to change symbol type, but if
	// "symbol already defined" error is thrown, the type will still have
	// been changed. this was done so the code below has a counter var.
	symbol_set_object(loop->symbol, &loop_counter, POWER_CHANGE_VALUE);
	// TODO: in versions before 0.97, force bit handling was broken
	// in both "!set" and "!for":
	// trying to change a force bit correctly raised an error, but
	// in any case, ALL FORCE BITS WERE CLEARED in symbol. only
	// cases like !set N=N+1 worked, because the force bit was
	// taken from result.
	// maybe support this behaviour via --dialect?
	if (loop->force_bit)
		symbol_set_force_bit(loop->symbol, loop->force_bit);
	loop_counter = loop->symbol->object;	// update local copy with force bit
	loop->symbol->has_been_read = TRUE;	// lock force bit
	if (loop->use_old_algo) {
		// old algo for old syntax:
		// if count == 0, skip loop
		if (loop->counter.last) {
			do {
				loop_counter.u.number.val.intval += loop->counter.increment;
				loop->symbol->object = loop_counter;	// overwrite whole struct, in case some joker has re-assigned loop counter var
				parse_ram_block(&loop->block);
			} while (loop_counter.u.number.val.intval < loop->counter.last);
		}
	} else {
		// new algo for new syntax:
		do {
			parse_ram_block(&loop->block);
			loop_counter.u.number.val.intval += loop->counter.increment;
			loop->symbol->object = loop_counter;	// overwrite whole struct, in case some joker has re-assigned loop counter var
		} while (loop_counter.u.number.val.intval != (loop->counter.last + loop->counter.increment));
	}
	// restore previous input:
	Input_now = outer_input;
}


// read condition, make copy, link to struct
static void copy_condition(struct condition *condition, char terminator)
{
	int	err;

	SKIPSPACE();
	DYNABUF_CLEAR(GlobalDynaBuf);
	while ((GotByte != terminator) && (GotByte != CHAR_EOS)) {
		// append to GlobalDynaBuf and check for quotes
		DYNABUF_APPEND(GlobalDynaBuf, GotByte);
		if ((GotByte == '"') || (GotByte == '\'')) {
			err = Input_quoted_to_dynabuf(GotByte);
			// here GotByte changes, it might become CHAR_EOS
			DYNABUF_APPEND(GlobalDynaBuf, GotByte);	// add closing quotes (or CHAR_EOS) as well
			if (err)
				break;	// on error, exit before eating CHAR_EOS via GetByte()
		}
		GetByte();
	}
	DynaBuf_append(GlobalDynaBuf, CHAR_EOS);	// ensure terminator
	condition->body = DynaBuf_get_copy(GlobalDynaBuf);
}

// try to read a condition into DynaBuf and store pointer to copy in
// given loop_condition structure.
// if no condition given, NULL is written to structure.
// call with GotByte = first interesting character
void flow_store_doloop_condition(struct condition *condition, char terminator)
{
	// write line number
	condition->line = Input_now->line_number;
	// set defaults
	condition->invert = FALSE;
	condition->body = NULL;
	// check for empty condition
	if (GotByte == terminator)
		return;

	// seems as if there really *is* a condition, so check for until/while
	if (Input_read_and_lower_keyword()) {
		if (strcmp(GlobalDynaBuf->buffer, "while") == 0) {
			//condition.invert = FALSE;
		} else if (strcmp(GlobalDynaBuf->buffer, "until") == 0) {
			condition->invert = TRUE;
		} else {
			Throw_error(exception_syntax);
			return;
		}
		// write given condition into buffer
		copy_condition(condition, terminator);
	}
}


// read a condition into DynaBuf and store pointer to copy in
// given loop_condition structure.
// call with GotByte = first interesting character
void flow_store_while_condition(struct condition *condition)
{
	condition->line = Input_now->line_number;
	condition->invert = FALSE;
	copy_condition(condition, CHAR_SOB);
}


// check a condition expression
static boolean check_condition(struct condition *condition)
{
	struct number	intresult;

	// first, check whether there actually *is* a condition
	if (condition->body == NULL)
		return TRUE;	// non-existing conditions are always true

	// set up input for expression evaluation
	Input_now->line_number = condition->line;
	Input_now->src.ram_ptr = condition->body;
	GetByte();	// proceed with next char
	ALU_defined_int(&intresult);
	if (GotByte)
		Throw_serious_error(exception_syntax);
	return condition->invert ? !intresult.val.intval : !!intresult.val.intval;
}


// back end function for "!do" and "!while" pseudo opcodes
void flow_do_while(struct do_while *loop)
{
	struct input	loop_input;
	struct input	*outer_input;

	// set up new input
	loop_input = *Input_now;	// copy current input structure into new
	loop_input.source = INPUTSRC_RAM;	// set new byte source
	// remember old input
	outer_input = Input_now;
	// activate new input (not useable yet, as pointer and
	// line number are not yet set up)
	Input_now = &loop_input;
	for (;;) {
		// check head condition
		if (!check_condition(&loop->head_cond))
			break;
		parse_ram_block(&loop->block);
		// check tail condition
		if (!check_condition(&loop->tail_cond))
			break;
	}
	// restore previous input:
	Input_now = outer_input;
	GotByte = CHAR_EOS;	// CAUTION! Very ugly kluge.
	// But by switching input, we lost the outer input's GotByte. We know
	// it was CHAR_EOS. We could just call GetByte() to get real input, but
	// then the main loop could choke on unexpected bytes. So we pretend
	// that we got the outer input's GotByte value magically back.
}


// parse a whole source code file
void flow_parse_and_close_file(FILE *fd, const char *filename)
{
	//TODO - check for bogus/malformed BOM and ignore!
	// be verbose
	if (config.process_verbosity > 2)
		printf("Parsing source file '%s'\n", filename);
	// set up new input
	Input_new_file(filename, fd);
	// Parse block and check end reason
	Parse_until_eob_or_eof();
	if (GotByte != CHAR_EOF)
		Throw_error("Found '}' instead of end-of-file.");
	// close sublevel src
	fclose(Input_now->src.fd);
}
