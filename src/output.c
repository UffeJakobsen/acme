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
#include "cpu.h"
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
	char		*buffer;	// holds assembled code (size is needed_bufsize)
	intval_t	write_idx;	// index of next write
	intval_t	lowest_written;		// smallest address used
	intval_t	highest_written;	// largest address used
	intval_t	forced_start_idx;	// first index to go into output file
	intval_t	forced_limit_idx;	// first index to not go into output file
	struct {
		intval_t	start;	// start of current segment (or NO_SEGMENT_START)
		intval_t	max;	// highest address segment may use
		bits		flags;	// segment flags ("overlay" and "invisible", see header file)
		struct segment	list_head;	// head element of doubly-linked ring list
	} segm;
	char		xor;		// output modifier
	intval_t	needed_bufsize;	// calculated at end of each pass
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
	struct segment	*test_segment	= out->segm.list_head.next;

	// search for smallest segment start address that
	// is larger than given address
	// use list head as sentinel
// FIXME - if +1 overflows intval_t, we have an infinite loop!
	out->segm.list_head.start = new_pc + 1;
	while (test_segment->start <= new_pc)
		test_segment = test_segment->next;
	if (test_segment == &out->segm.list_head)
		out->segm.max = OUTBUF_MAXSIZE - 1;
	else
		out->segm.max = test_segment->start - 1;	// last free address available
}


//
static void border_crossed(int current_offset)
{
	// FIXME - find a way to make this a normal error instead of a serious one,
	// so it can be suppressed until we are sure the program won't shrink any
	// further:
	if (current_offset >= OUTBUF_MAXSIZE)
		Throw_serious_error("Reached memory limit.");
	if (pass.flags.throw_segment_messages) {
		throw_message(config.debuglevel_segmentprobs, "Segment reached another one, overwriting it.", NULL);
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
	if (out->write_idx > out->segm.max)
		border_crossed(out->write_idx);
	// new minimum address?
	if (out->write_idx < out->lowest_written)
		out->lowest_written = out->write_idx;
	// new maximum address?
	if (out->write_idx > out->highest_written)
		out->highest_written = out->write_idx;
	// tell report listing about byte
	if (report->fd)
		report_binary(byte & 0xff);	// file for reporting
	// write byte to output buffer
	if (out->buffer)
		out->buffer[out->write_idx] = (byte & 0xff) ^ out->xor;
	// advance pointer
	out->write_idx++;
	++statement_size;	// count this byte
}

// throw "program counter not defined" error and then use a fake pc so this does
// not happen again
static void complain_and_use_dummy_pc(void)
{
	Throw_error(exception_pc_undefined);
	vcpu_set_pc(cpu_current_type->dummy_pc, 0);	// 0 = no flags
}

// throw error (pc undefined) and use fake pc from now on
static void no_output(intval_t byte)
{
	complain_and_use_dummy_pc();
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
	if (output_byte == no_output)
		complain_and_use_dummy_pc();

	// CAUTION - there are two copies of these checks!
	// TODO - add additional check for current segment's "limit" value
	// did we reach next segment?
	if (out->write_idx + size - 1 > out->segm.max)
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


// remember current outbuf index as start/limit of output file
void outbuf_set_outfile_start(void)
{
	// check whether ptr undefined
	if (output_byte == no_output)
		complain_and_use_dummy_pc();
	out->forced_start_idx = out->write_idx;
}
void outbuf_set_outfile_limit(void)
{
	// check whether ptr undefined
	if (output_byte == no_output)
		complain_and_use_dummy_pc();
	out->forced_limit_idx = out->write_idx;
}


// link segment data into segment ring
static void link_segment(intval_t start, intval_t length)
{
	struct segment	*new_segment,
			*test_segment	= out->segm.list_head.next;

	// init new segment
	new_segment = safe_malloc(sizeof(*new_segment));
	new_segment->start = start;
	new_segment->length = length;
	// use ring head as sentinel
	out->segm.list_head.start = start;
	out->segm.list_head.length = length + 1;	// +1 to make sure sentinel exits loop
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
// FIXME - move this fn to its single caller!
static void check_segment(intval_t new_pc)
{
	struct segment	*test_segment	= out->segm.list_head.next;

	// use list head as sentinel
	out->segm.list_head.start = new_pc + 1;	// +1 to make sure sentinel exits loop
	out->segm.list_head.length = 1;
	// search ring for matching entry
	while (test_segment->start <= new_pc) {
		if ((test_segment->start + test_segment->length) > new_pc) {
			// TODO - include overlap size in error message!
			throw_message(config.debuglevel_segmentprobs, "Segment starts inside another one, overwriting it.", NULL);
			return;
		}

		test_segment = test_segment->next;
	}
}


// init structs
void output_init(void)
{
	// init ring list of segments (FIXME - move to passinit)
	out->segm.list_head.next = &out->segm.list_head;
	out->segm.list_head.prev = &out->segm.list_head;
	// init the one field not initialized by output_passinit:
	out->needed_bufsize = NO_VALUE_GIVEN;
}


// clear segment list and disable output
void output_passinit(void)
{
	//struct segment	*temp;

	// init output struct:
	if (pass.flags.generate_output) {
		// we are supposed to actually generate correct output, so
		// allocate and init output buffer:
		if (out->needed_bufsize == NO_VALUE_GIVEN) {
			// this is not an error. it happens when the source code
			// does not create a single output byte, for example if
			// someone uses ACME simply for "!info 3456 / 78"
			out->needed_bufsize = 16;	// actually 1 would suffice...
		}
		if (out->needed_bufsize > OUTBUF_MAXSIZE) {
			Throw_serious_error("Output buffer size exceeds maximum.");
		}
		//fprintf(stderr, "Allocating outbuf of size 0x%06x.\n", out->needed_bufsize);
		out->buffer = safe_malloc(out->needed_bufsize);
		// fill output buffer with initial byte value
		if (config.mem_init_value == NO_VALUE_GIVEN) {
			memset(out->buffer, 0, out->needed_bufsize);	// default is zero
		} else {
			memset(out->buffer, config.mem_init_value & 0xff, out->needed_bufsize);
		}
	} else {
		out->buffer = NULL;
	}
	// FIXME - this should be NO_VALUE_GIVEN, but then pseudopc offset would be off, and
	// we cannot use NO_VALUE_GIVEN for pc as long as setting pc uses diff to old value!
	out->write_idx = 0;	// ...so we set it to the same value as pc on pass init!
	// invalidate start and end (first byte actually written will fix them)
	out->lowest_written = OUTBUF_MAXSIZE;	// FIXME - add code so OUTBUF_MAXSIZE-1 is a hard limit!
	out->highest_written = -1;
	// no overrides for start and end yet:
	out->forced_start_idx = NO_VALUE_GIVEN;
	out->forced_limit_idx = NO_VALUE_GIVEN;
	// not in a segment
	out->segm.start = NO_SEGMENT_START;	// TODO - "no active segment" could be made a segment flag!
	out->segm.max = OUTBUF_MAXSIZE - 1;
	out->segm.flags = 0;
//FIXME - why clear ring list in every pass?
// Because later pass shouldn't complain about overwriting the same segment from earlier pass!
// Currently this does not happen because segment warnings are only generated in first pass.
// FIXME - in future we want to _output_ the errors/warnings in the final pass only,
// but we need to _check_ for them in every pass because only the results of the second-to-last
// pass are important. so for the time being, keep this code:
	// delete segment list (and free blocks)
//	while ((temp = segment_list)) {
//		segment_list = segment_list->next;
//		free(temp);
//	}
	// no "encryption":
	out->xor = 0;
	// needed size of buffer will be calculated at end of pass, so
	//out->needed_bufsize = do not init

	// deactivate output - any byte written will trigger error:
	output_byte = no_output;

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
	// FIXME - do the _checking_ in all passes, but only throw errors/warnings in final pass!
	if (!pass.flags.throw_segment_messages)
		return;

	// if there is no segment, there is nothing to do
	if (out->segm.start == NO_SEGMENT_START)
		return;

	// ignore "invisible" segments
	if (out->segm.flags & SEGMENT_FLAG_INVISIBLE)
		return;

	// ignore empty segments
	amount = out->write_idx - out->segm.start;
	if (amount == 0)
		return;

	// link to segment list
	link_segment(out->segm.start, amount);
	// announce
	if (config.process_verbosity >= 2)
		// TODO - change output to start, limit, size, name:
		// TODO - output hex numbers as %04x? What about limit 0x10000?
		printf("Segment size is %d (0x%x) bytes (0x%x - 0x%x exclusive).\n",
			amount, amount, out->segm.start, out->write_idx);
}


// make sure last code segment is closed
// (this gets called exactly once at the end of each pass, as opposed to
// the static fn "end_segment" which is also called when a new segment starts)
void output_endofpass(void)
{
	end_segment();
	// calculate size of output buffer
	if (out->highest_written >= out->lowest_written) {
		out->needed_bufsize = out->highest_written + 1;
	} else {
		out->needed_bufsize = NO_VALUE_GIVEN;
	}
	//fprintf(stderr, "Need outbuf size of 0x%04x bytes.\n", out->needed_bufsize);
}


// change output pointer and enable output
static void start_segment(intval_t address_change, bits segment_flags)
{
	// properly finalize previous segment (link to list, announce)
	end_segment();

	// calculate start of new segment
	out->write_idx = (out->write_idx + address_change);
	if (out->write_idx < 0) {
		Throw_serious_error("Tried to write to negative addresses.");
	} else if (out->write_idx >= OUTBUF_MAXSIZE) {
		Throw_serious_error("Reached memory limit.");
	}
	out->segm.start = out->write_idx;
	out->segm.flags = segment_flags;
	// allow "writing to buffer" (even though buffer does not really exist until final pass)
	output_byte = real_output;
	// in first/last pass, check for other segments and maybe issue warning
	if (pass.flags.throw_segment_messages) {
		if (!(segment_flags & SEGMENT_FLAG_OVERLAY))
			check_segment(out->segm.start);
		find_segment_max(out->segm.start);
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


// set program counter to defined value
// if start address was given on command line, main loop will call this before each pass.
// in addition to that, it will be called on each "*= VALUE".
void vcpu_set_pc(intval_t new_pc, bits segment_flags)
{
	intval_t	pc_change;

	pc_change = new_pc - program_counter;
	program_counter = new_pc;	// FIXME - oversized values are accepted without error and will be wrapped at end of statement!
	pc_ntype = NUMTYPE_INT;
	// now tell output buffer to start a new segment
	start_segment(pc_change, segment_flags);
}


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
	program_counter += statement_size;
	statement_size = 0;	// reset
}


// return start and size of memory block to write to output file,
// along with load address for cbm/apple headers.
void output_get_result(const char **ptr, intval_t *size, intval_t *loadaddr)
{
	intval_t	start,
			limit,	// end+1
			amount;

	if (out->buffer == NULL)
		BUG("noOutBuf", 0);

	start = out->lowest_written;
	limit = out->highest_written + 1;
	// if pseudo opcodes were used, they override the actual values:
	if (out->forced_start_idx != NO_VALUE_GIVEN)
		start = out->forced_start_idx;
	if (out->forced_limit_idx != NO_VALUE_GIVEN)
		limit = out->forced_limit_idx;
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

	// check whether ptr undefined
	if (output_byte == no_output)
		complain_and_use_dummy_pc();

	new_context = safe_malloc(sizeof(*new_context));	// create new struct (this must never be freed, as it gets linked to labels!)
	new_context->outer = pseudopc_current_context;	// let it point to previous one
	pseudopc_current_context = new_context;	// make it the current one

	new_context->ntype = pc_ntype;
	new_context->offset = new_pc->val.intval - program_counter;
	program_counter = new_pc->val.intval;
	pc_ntype = NUMTYPE_INT;
	//new: pc_flags = new_pc->flags & (NUMBER_IS_DEFINED | NUMBER_EVER_UNDEFINED);
}
// end offset assembly
void pseudopc_end(void)
{
	program_counter = program_counter - pseudopc_current_context->offset;
// FIXME - if pc can wrap around, then we should have fixed the offset!
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
		target->val.intval = target->val.intval - context->offset;
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
