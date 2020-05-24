// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2020 Marco Baye
// Have a look at "acme.c" for further info
//
// flow control stuff (loops, conditional assembly etc.)
#ifndef flow_H
#define flow_H


#include <stdio.h>
#include "config.h"


struct block {
	int	start;	// line number of start of block
	char	*body;
};

// struct to pass "!for" loop stuff from pseudoopcodes.c to flow.c
struct for_loop {
	struct symbol	*symbol;
	boolean		use_old_algo;
	struct {
		intval_t	first,
				last,
				increment;
		int		addr_refs;	// address reference count
	} counter;
	struct block	block;
};

// structs to pass "!do"/"!while" stuff from pseudoopcodes.c to flow.c
struct condition {
	int	line;	// original line number
	boolean	invert;	// only set for UNTIL conditions
	char	*body;	// pointer to actual expression
};
struct do_while {
	struct condition	head_cond;
	struct block		block;
	struct condition	tail_cond;
};


// back end function for "!for" pseudo opcode
extern void flow_forloop(struct for_loop *loop);
// try to read a condition into DynaBuf and store pointer to copy in
// given condition structure.
// if no condition given, NULL is written to structure.
// call with GotByte = first interesting character
extern void flow_store_doloop_condition(struct condition *condition, char terminator);
// read a condition into DynaBuf and store pointer to copy in
// given condition structure.
// call with GotByte = first interesting character
extern void flow_store_while_condition(struct condition *condition);
// back end function for "!do" pseudo opcode
extern void flow_do_while(struct do_while *loop);
// parse a whole source code file
extern void flow_parse_and_close_file(FILE *fd, const char *filename);


#endif
