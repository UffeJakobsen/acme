// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2024 Marco Baye
// Have a look at "acme.c" for further info
//
// Output stuff
// 24 Nov 2007	Added possibility to suppress segment overlap warnings
// 25 Sep 2011	Fixed bug in !to (colons in filename could be interpreted as EOS)
//  5 Mar 2014	Fixed bug where setting *>0xffff resulted in hangups.
// 19 Nov 2014	Merged Johann Klasek's report listing generator patch
// 22 Sep 2015	Added big-endian output functions
// 20 Apr 2019	Prepared for "make segment overlap warnings into errors" later on
#include "output.h"
#include <string.h>	// for memset()
#include "global.h"


// constants
#define NO_SEGMENT_START	(-1)	// invalid value to signal "not in a segment"


// structure for linked list of segment data
struct segment {
	struct segment	*next,
			*prev;
	intval_t	start,
			length;
};

// structure for all output stuff:
struct output {
	// output buffer stuff
	char		*buffer;	// holds assembled code (size is config.outbuf_size)
	intval_t	write_idx;	// index of next write
	intval_t	lowest_written;		// smallest address used
	intval_t	highest_written;	// largest address used
	struct {
		intval_t	start;	// start of current segment (or NO_SEGMENT_START)
		intval_t	max;	// highest address segment may use
		bits		flags;	// segment flags ("overlay" and "invisible", see header file)
		struct segment	list_head;	// head element of doubly-linked ring list
	} segment;	// FIXME - rename either this component or "struct segment"!
	char		xor;		// output modifier
};

// for offset assembly:
// struct to describe a pseudopc context (each label gets a pointer to this)
struct pseudopc {
	struct pseudopc	*outer;	// next layer (to be able to "unpseudopc" labels by more than one level)
	intval_t	offset;	// inner minus outer pc
	enum numtype	ntype;	// type of outer pc (INT/UNDEFINED)
};
static struct pseudopc	outermost_pseudopc_context;	// dummy struct when "!pseudopc" not in use
static struct pseudopc	*pseudopc_current_context	= &outermost_pseudopc_context;	// current struct


// variables
static struct output	default_output;
static struct output	*out	= &default_output;	// FIXME - never changes! is the ptr a preparation for "assembling several different parts in one go"?
static int		statement_size;	// add to PC after statement
static intval_t		program_counter;	// current program counter (pseudopc value)
static enum numtype	pc_ntype;

// report binary output
static void report_binary(char value)
{
	if (report->bin_used == 0)
		report->bin_address = out->write_idx;	// remember address at start of line
	if (report->bin_used < REPORT_BINBUFSIZE)
		report->bin_buf[report->bin_used++] = value;
}


// set up new out->segment.max value according to the given address.
// just find the next segment start and subtract 1.
static void find_segment_max(intval_t new_pc)
{
	struct segment	*test_segment	= out->segment.list_head.next;

	// search for smallest segment start address that
	// is larger than given address
	// use list head as sentinel
// FIXME - if +1 overflows intval_t, we have an infinite loop!
	out->segment.list_head.start = new_pc + 1;
	while (test_segment->start <= new_pc)
		test_segment = test_segment->next;
	if (test_segment == &out->segment.list_head)
		out->segment.max = config.outbuf_size - 1;
	else
		out->segment.max = test_segment->start - 1;	// last free address available
}


//
static void border_crossed(int current_offset)
{
	// FIXME - find a way to make this a normal error instead of a serious one,
	// so it can be suppressed until we are sure the program won't shrink any
	// further:
	if (current_offset >= config.outbuf_size)
		Throw_serious_error("Reached memory limit.");
	if (pass.flags.do_segment_checks) {
		throw_message(config.debuglevel_segmentprobs, "Segment reached another one, overwriting it.");
		find_segment_max(current_offset + 1);	// find new (next) limit
	}
}


// function ptr to write byte into output buffer (might point to real fn or error trigger)
void (*output_byte)(intval_t byte);


// send low byte to output buffer and remember to later increase program counter
static void real_output(intval_t byte)
{
	// CAUTION - there are two copies of these checks!
	// TODO - add additional check for current segment's "limit" value
	// did we reach next segment?
	if (out->write_idx > out->segment.max)
		border_crossed(out->write_idx);
	// new minimum address?
	if (out->write_idx < out->lowest_written)
		out->lowest_written = out->write_idx;
	// new maximum address?
	if (out->write_idx > out->highest_written)
		out->highest_written = out->write_idx;
	// write byte and advance ptrs
	if (report->fd)
		report_binary(byte & 0xff);	// file for reporting
	out->buffer[out->write_idx++] = (byte & 0xff) ^ out->xor;
	++statement_size;	// count this byte
}


// throw error (pc undefined) and use fake pc from now on
static void no_output(intval_t byte)
{
	Throw_error(exception_pc_undefined);
	// now change fn ptr to not complain again.
	output_byte = real_output;
	output_byte(byte);	// try again
}


// skip over some bytes in output buffer without starting a new segment
// (used by "!skip", and also called by "!binary" if really calling
// output_byte would be a waste of time)
void output_skip(int size)
{
	if (size < 1) {
		// ok for zero, but why is there no error message
		// output for negative values?
		// ...because caller should have caught those!
		// FIXME - add BUG() for those!
		return;
	}

	// check whether ptr undefined
	if (output_byte == no_output) {
		output_byte(0);	// trigger error with a dummy byte
		--size;	// fix amount to cater for dummy byte
	}
	// CAUTION - there are two copies of these checks!
	// TODO - add additional check for current segment's "limit" value
	// did we reach next segment?
	if (out->write_idx + size - 1 > out->segment.max)
		border_crossed(out->write_idx + size - 1);
	// new minimum address?
	if (out->write_idx < out->lowest_written)
		out->lowest_written = out->write_idx;
	// new maximum address?
	if (out->write_idx + size - 1 > out->highest_written)
		out->highest_written = out->write_idx + size - 1;
	// advance ptrs
	out->write_idx += size;
	statement_size += size;	// count bytes so PC will be adjusted correctly after this
}


// fill output buffer with given byte value
static void fill_completely(char value)
{
	memset(out->buffer, value, config.outbuf_size);
}


// default value for empty memory has changed (called by "!initmem" pseudo opcode)
void output_newdefault(void)
{
	// init memory
	fill_completely(config.mem_init_value);
	// enforce another pass
	if (pass.undefined_count == 0)
		pass.undefined_count = 1;
	//if (pass.needvalue_count == 0)	FIXME - use? instead or additionally?
	//	pass.needvalue_count = 1;
// enforcing another pass is not needed if there hasn't been any
// output yet. But that's tricky to detect without too much overhead.
// The old solution was to add &&(out->lowest_written < out->highest_written+1) to "if" above
// in future, just allocate and init outbuf at the start of the "last" pass!
}

// remember current outbuf index as start/limit of output file
static boolean	force_file_start	= FALSE;
static intval_t	forced_start_idx;
void outbuf_set_outfile_start(void)
{
	// check whether ptr undefined
	if (output_byte == no_output) {
		Throw_error(exception_pc_undefined);
	} else {
		force_file_start = TRUE;
		forced_start_idx = out->write_idx;
	}
}
static boolean	force_file_limit	= FALSE;
static intval_t	forced_limit_idx;
void outbuf_set_outfile_limit(void)
{
	// check whether ptr undefined
	if (output_byte == no_output) {
		Throw_error(exception_pc_undefined);
	} else {
		force_file_limit = TRUE;
		forced_limit_idx = out->write_idx;
	}
}


// init output struct
void output_createbuffer(void)
{
	out->buffer = safe_malloc(config.outbuf_size);
// FIXME - in future, do both of these only at start of "last" pass:
	// fill memory with initial value
	if (config.mem_init_value == NO_VALUE_GIVEN) {
		fill_completely(0);	// default value
	} else {
		fill_completely(config.mem_init_value & 0xff);
	}
	// init ring list of segments
	out->segment.list_head.next = &out->segment.list_head;
	out->segment.list_head.prev = &out->segment.list_head;
}


// link segment data into segment ring
static void link_segment(intval_t start, intval_t length)
{
	struct segment	*new_segment,
			*test_segment	= out->segment.list_head.next;

	// init new segment
	new_segment = safe_malloc(sizeof(*new_segment));
	new_segment->start = start;
	new_segment->length = length;
	// use ring head as sentinel
	out->segment.list_head.start = start;
	out->segment.list_head.length = length + 1;	// +1 to make sure sentinel exits loop
	// walk ring to find correct spot
	while ((test_segment->start < new_segment->start)
	|| ((test_segment->start == new_segment->start) && (test_segment->length < new_segment->length)))
		test_segment = test_segment->next;
	// link into ring
	new_segment->next = test_segment;
	new_segment->prev = test_segment->prev;
	new_segment->next->prev = new_segment;
	new_segment->prev->next = new_segment;
}


// check whether given PC is inside segment.
// only call in first pass, otherwise too many warnings might be thrown
// FIXME - do it the other way round and only complain if there were no other errors!
static void check_segment(intval_t new_pc)
{
	struct segment	*test_segment	= out->segment.list_head.next;

	// use list head as sentinel
	out->segment.list_head.start = new_pc + 1;	// +1 to make sure sentinel exits loop
	out->segment.list_head.length = 1;
	// search ring for matching entry
	while (test_segment->start <= new_pc) {
		if ((test_segment->start + test_segment->length) > new_pc) {
			// TODO - include overlap size in error message!
			throw_message(config.debuglevel_segmentprobs, "Segment starts inside another one, overwriting it.");
			return;
		}

		test_segment = test_segment->next;
	}
}


// clear segment list and disable output
void output_passinit(void)
{
//	struct segment	*temp;

//FIXME - why clear ring list in every pass?
// Because later pass shouldn't complain about overwriting the same segment from earlier pass!
// Currently this does not happen because segment warnings are only generated in first pass. FIXME!
	// delete segment list (and free blocks)
//	while ((temp = segment_list)) {
//		segment_list = segment_list->next;
//		free(temp);
//	}

	// invalidate start and end (first byte actually written will fix them)
	out->lowest_written = config.outbuf_size - 1;
	out->highest_written = 0;
	// deactivate output - any byte written will trigger error:
	output_byte = no_output;
	out->write_idx = 0;	// same as pc on pass init!
	out->segment.start = NO_SEGMENT_START;	// TODO - "no active segment" could be made a segment flag!
	out->segment.max = config.outbuf_size - 1;	// TODO - use end of bank?
	out->segment.flags = 0;
	out->xor = 0;

	//vcpu stuff:
	pc_ntype = NUMTYPE_UNDEFINED;	// not defined yet
	// FIXME - number type is "undefined", but still the intval 0 below will
	// be used to calculate diff when pc is first set.
	program_counter = 0;	// same as output's write_idx on pass init
	statement_size = 0;	// increase PC by this at end of statement

	// pseudopc stuff:
	// init dummy pseudopc struct
	outermost_pseudopc_context.outer = NULL;
	outermost_pseudopc_context.offset = 0;
	outermost_pseudopc_context.ntype = NUMTYPE_UNDEFINED;
	// and use it:
	pseudopc_current_context = &outermost_pseudopc_context;

// this was moved over from caller - does it make sense to merge into some if/else?

	// if start address was given on command line, use it:
	if (config.initial_pc != NO_VALUE_GIVEN)
		vcpu_set_pc(config.initial_pc, 0);	// 0 -> no segment flags
}


// show start and end of current segment
// called whenever a new segment begins, and at end of pass.
static void end_segment(void)
{
	intval_t	amount;

	// only do in first or last pass
	if (!pass.flags.do_segment_checks)
		return;

	// if there is no segment, there is nothing to do
	if (out->segment.start == NO_SEGMENT_START)
		return;

	// ignore "invisible" segments
	if (out->segment.flags & SEGMENT_FLAG_INVISIBLE)
		return;

	// ignore empty segments
	amount = out->write_idx - out->segment.start;
	if (amount == 0)
		return;

	// link to segment list
	link_segment(out->segment.start, amount);
	// announce
	if (config.process_verbosity >= 2)
		// TODO - change output to start, limit, size, name:
		// TODO - output hex numbers as %04x? What about limit 0x10000?
		printf("Segment size is %d (0x%x) bytes (0x%x - 0x%x exclusive).\n",
			amount, amount, out->segment.start, out->write_idx);
}


// make sure last code segment is closed
// (this gets called exactly once at the end of each pass, as opposed to
// the static fn "end_segment" which is also called when a new segment starts)
void output_endofpass(void)
{
	end_segment();
}


// change output pointer and enable output
static void start_segment(intval_t address_change, bits segment_flags)
{
	// properly finalize previous segment (link to list, announce)
	end_segment();

	// calculate start of new segment
	out->write_idx = (out->write_idx + address_change) & (config.outbuf_size - 1);
	out->segment.start = out->write_idx;
	out->segment.flags = segment_flags;
	// allow writing to output buffer
	output_byte = real_output;
	// in first/last pass, check for other segments and maybe issue warning
	if (pass.flags.do_segment_checks) {
		if (!(segment_flags & SEGMENT_FLAG_OVERLAY))
			check_segment(out->segment.start);
		find_segment_max(out->segment.start);
	}
}


// get/set "encryption" byte
char output_get_xor(void)
{
	return out->xor;
}
void output_set_xor(char xor)
{
	out->xor = xor;
}


// set program counter to defined value (FIXME - allow for undefined!)
// if start address was given on command line, main loop will call this before each pass.
// in addition to that, it will be called on each "*= VALUE".
void vcpu_set_pc(intval_t new_pc, bits segment_flags)
{
	intval_t	pc_change;

	pc_change = new_pc - program_counter;
	program_counter = new_pc;	// FIXME - oversized values are accepted without error and will be wrapped at end of statement!
	pc_ntype = NUMTYPE_INT;	// FIXME - remove when allowing undefined!
	// now tell output buffer to start a new segment
	start_segment(pc_change, segment_flags);
}
/*
TODO - overhaul program counter and memory pointer stuff:
general stuff: PC and mem ptr might be marked as "undefined" via flags field.
However, their "value" fields are still updated, so we can calculate differences.

on pass init:
	if value given on command line, set PC and out ptr to that value
	otherwise, set both to zero and mark as "undefined"
when ALU asks for "*":
	return current PC (value and flags)
when encountering "!pseudopc VALUE { BLOCK }":
	parse new value (NEW: might be undefined!)
	remember difference between current and new value
	set PC to new value
	after BLOCK, use remembered difference to change PC back
when encountering "*= VALUE":
	parse new value (NEW: might be undefined!)
	calculate difference between current PC and new value
	set PC to new value
	tell outbuf to add difference to mem ptr (starting a new segment)
		if new value is undefined, tell outbuf to disable output

Problem: always check for "undefined"; there are some problematic combinations.
I need a way to return the size of a generated code block even if PC undefined.
Maybe like this:
	*= new_address [, invisible] [, overlay] [, size_counter = symbol {]
		...code...
	[} ; at end of block, size is written to symbol given above!]
*/


// get program counter
void vcpu_read_pc(struct number *target)
{
	target->ntype = pc_ntype;
	target->flags = 0;	// FIXME - if defined, check for FITS_BYTE etc.? use pc_flags?
	target->val.intval = program_counter;
	target->addr_refs = 1;	// yes, PC counts as address
}


// get size of current statement (until now) - needed for "!bin" verbose output
int output_get_statement_size(void)
{
	return statement_size;
}


// adjust program counter (called at end of each statement)
void output_end_statement(void)
{
	// FIXME - that '&' cannot be right!
	// it makes sense from a cpu point of view (which wraps around to 0),
	// but not from "outbuf" point of view.
	program_counter = (program_counter + statement_size) & (config.outbuf_size - 1);
	statement_size = 0;	// reset
}


// return start and size of memory block to write to output file,
// along with load address for cbm/apple headers.
void output_get_result(const char **ptr, intval_t *size, intval_t *loadaddr)
{
	intval_t	start,
			limit,	// end+1
			amount;

	start = out->lowest_written;
	limit = out->highest_written + 1;
	// if pseudo opcodes were used, they override the actual values:
	if (force_file_start)
		start = forced_start_idx;
	if (force_file_limit)
		limit = forced_limit_idx;
	// if cli args were given, they override even harder:
	if (config.outfile_start != NO_VALUE_GIVEN)
		start = config.outfile_start;
	if (config.outfile_limit != NO_VALUE_GIVEN)
		limit = config.outfile_limit;

	if (limit <= start) {
		// nothing written
		start = 0;	// I could try to use some segment start, but what for?
		amount = 0;
		// FIXME - how about not writing anything in this case?
		// a CBM file would consist of a bogus load address and nothing else!
	} else {
		amount = limit - start;
	}

	*ptr = out->buffer + start;
	*size = amount;
	*loadaddr = start;
}


// pseudopc stuff:

// start offset assembly
void pseudopc_start(struct number *new_pc)
{
	struct pseudopc	*new_context;

	new_context = safe_malloc(sizeof(*new_context));	// create new struct (this must never be freed, as it gets linked to labels!)
	new_context->outer = pseudopc_current_context;	// let it point to previous one
	pseudopc_current_context = new_context;	// make it the current one

	new_context->ntype = pc_ntype;
	new_context->offset = new_pc->val.intval - program_counter;
	program_counter = new_pc->val.intval;
	pc_ntype = NUMTYPE_INT;	// FIXME - remove when allowing undefined!
	//new: pc_flags = new_pc->flags & (NUMBER_IS_DEFINED | NUMBER_EVER_UNDEFINED);
}
// end offset assembly
void pseudopc_end(void)
{
	// FIXME - check this "wraparound" stuff, it may no longer make sense!
	program_counter = (program_counter - pseudopc_current_context->offset) & (config.outbuf_size - 1);	// pc might have wrapped around
	pc_ntype = pseudopc_current_context->ntype;
	pseudopc_current_context = pseudopc_current_context->outer;	// go back to outer block
	if (pseudopc_current_context == NULL)
		BUG("PseudoPCContext", 0);
}
// un-pseudopc a label value by given number of levels
// returns nonzero on error (if level too high)
int pseudopc_unpseudo(struct number *target, struct pseudopc *context, unsigned int levels)
{
	while (levels--) {
		if (target->ntype == NUMTYPE_UNDEFINED)
			return 0;	// ok (no sense in trying to unpseudo this, and it might be an unresolved forward ref anyway)

		if (context == NULL) {
			Throw_error("Un-pseudopc operator '&' only works on addresses.");
			return 1;	// error
		}
		if (context == &outermost_pseudopc_context) {
			Throw_error("Un-pseudopc operator '&' has no !pseudopc context.");
			return 1;	// error
		}
		// FIXME - in future, check both target and context for NUMTYPE_UNDEFINED!
		target->val.intval = (target->val.intval - context->offset) & (config.outbuf_size - 1);	// FIXME - is masking really needed?	TODO
		context = context->outer;
	}
	return 0;	// ok
}
// return pointer to current "pseudopc" struct
// this gets called when parsing label definitions
struct pseudopc *pseudopc_get_context(void)
{
	return pseudopc_current_context;
}
// returns nonzero if "!pseudopc" is in effect, zero otherwise
int pseudopc_isactive(void)
{
	return pseudopc_current_context != &outermost_pseudopc_context;
}
