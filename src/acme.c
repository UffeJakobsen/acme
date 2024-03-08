// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2024 Marco Baye
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#include "acme.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "alu.h"
#include "cliargs.h"
#include "config.h"
#include "cpu.h"
#include "dynabuf.h"
#include "encoding.h"
#include "flow.h"
#include "global.h"
#include "input.h"
#include "macro.h"
#include "mnemo.h"
#include "output.h"
#include "platform.h"
#include "pseudoopcodes.h"
#include "section.h"
#include "symbol.h"
#include "version.h"


// constants
static const char	FILE_WRITETEXT[]	= "w";
static const char	FILE_WRITEBINARY[]	= "wb";
// names for error messages
static const char	name_outfile[]		= "output filename";
static const char	arg_symbollist[]	= "symbol list filename";
static const char	arg_reportfile[]	= "report filename";
static const char	arg_vicelabels[]	= "VICE labels filename";
// long options
#define OPTION_HELP		"help"
#define OPTION_FORMAT		"format"
#define OPTION_OUTFILE		"outfile"
#define OPTION_LABELDUMP	"labeldump"	// old
#define OPTION_SYMBOLLIST	"symbollist"	// new
#define OPTION_VICELABELS	"vicelabels"
#define OPTION_REPORT		"report"
#define OPTION_SETPC		"setpc"
#define OPTION_FROM_TO		"from-to"
#define OPTION_CPU		"cpu"
#define OPTION_INITMEM		"initmem"
#define OPTION_MAXERRORS	"maxerrors"
#define OPTION_MAXDEPTH		"maxdepth"
#define OPTION_USE_STDOUT	"use-stdout"
#define OPTION_VERSION		"version"
#define OPTION_MSVC		"msvc"
#define OPTION_COLOR		"color"
#define OPTION_FULLSTOP		"fullstop"
#define OPTION_IGNORE_ZEROES	"ignore-zeroes"
#define OPTION_STRICT_SEGMENTS	"strict-segments"
#define OPTION_STRICT		"strict"
#define OPTION_DIALECT		"dialect"
#define OPTION_DEBUGLEVEL	"debuglevel"
#define OPTION_TEST		"test"
// options for "-W"
#define OPTIONWNO_LABEL_INDENT	"no-label-indent"
#define OPTIONWNO_OLD_FOR	"no-old-for"
#define OPTIONWNO_BIN_LEN	"no-bin-len"
#define OPTIONWTYPE_MISMATCH	"type-mismatch"


// variables
static const char	**toplevel_sources_plat;	// source file names given on command line (platform-style)
static int		toplevel_src_count	= 0;


// show release and platform info (and exit, if wanted)
static void show_version(int exit_after)
{
	puts(
"This is ACME, release " RELEASE " (\"" CODENAME "\"), " CHANGE_DATE " " CHANGE_YEAR "\n"
"  " PLATFORM_VERSION);
	if (exit_after)
		exit(EXIT_SUCCESS);
}


// show full help (headline, release/platform/version, copyright, dedication,
// warranty disclaimer, usage) and exit program (SUCCESS)
static void show_help_and_exit(void)
{
	puts(
"ACME - the ACME Crossassembler for Multiple Environments\n"
"  Copyright (C) 1998-" CHANGE_YEAR " Marco Baye");
	show_version(FALSE);
	puts(
"ACME comes with ABSOLUTELY NO WARRANTY; for details read the help file.\n"
"  This is free software, and you are welcome to redistribute it under\n"
"  certain conditions; as outlined in the GNU General Public License.\n"
"Dedicated to the wisest being I ever had the pleasure of reading\n"
"  books of (currently spending some time dead for tax reasons).\n"
"The newest version can be found at the ACME homepage:\n"
"  " HOME_PAGE "\n"
"\n"
"Usage:\n"
"acme [OPTION...] [FILE]...\n"
"\n"
"Options:\n"
"  -h, --" OPTION_HELP "                show this help and exit\n"
"  -f, --" OPTION_FORMAT " FORMAT       set output file format\n"
"  -o, --" OPTION_OUTFILE " FILE        set output file name\n"
"  -r, --" OPTION_REPORT " FILE         set report file name\n"
"  -l, --" OPTION_SYMBOLLIST " FILE     set symbol list file name\n"
"      --" OPTION_LABELDUMP "           (old name for --" OPTION_SYMBOLLIST ")\n"
"      --" OPTION_VICELABELS " FILE     set file name for label dump in VICE format\n"
"      --" OPTION_SETPC " VALUE         set program counter\n"
"      --" OPTION_FROM_TO " VALUE VALUE set start and end+1 of output file\n"
"      --" OPTION_CPU " CPU             set target processor\n"
"      --" OPTION_INITMEM " VALUE       define 'empty' memory\n"
"      --" OPTION_MAXERRORS " NUMBER    set number of errors before exiting\n"
"      --" OPTION_MAXDEPTH " NUMBER     set recursion depth for macro calls and !src\n"
"      --" OPTION_IGNORE_ZEROES "       do not determine number size by leading zeroes\n"
"      --" OPTION_STRICT_SEGMENTS "     turn segment overlap warnings into errors\n"
"      --" OPTION_STRICT "              treat all warnings like errors\n"
"  -vDIGIT                   set verbosity level\n"
"  -DSYMBOL=VALUE            define global symbol\n"
"  -I PATH/TO/DIR            add search path for input files\n"
// TODO: replace these:
"  -W" OPTIONWNO_LABEL_INDENT "         suppress warnings about indented labels\n"
"  -W" OPTIONWNO_OLD_FOR "              (old, use \"--dialect 0.94.8\" instead)\n"
"  -W" OPTIONWNO_BIN_LEN "              suppress warnings about lengths of binary literals\n"
"  -W" OPTIONWTYPE_MISMATCH "           enable type checking (warn about type mismatch)\n"
// with this line and add a separate function:
//"  -W                     show warning level options\n"
"      --" OPTION_USE_STDOUT "          fix for 'Relaunch64' IDE (see docs)\n"
"      --" OPTION_MSVC "                output errors in MS VS format\n"
"      --" OPTION_COLOR "               use ANSI color codes for error output\n"
"      --" OPTION_FULLSTOP "            use '.' as pseudo opcode prefix\n"
"      --" OPTION_DIALECT " VERSION     behave like different version\n"
"      --" OPTION_DEBUGLEVEL " VALUE    drop all higher-level debug messages\n"
"      --" OPTION_TEST "                enable experimental features\n"
PLATFORM_OPTION_HELP
"  -V, --" OPTION_VERSION "             show version and exit\n");
	exit(EXIT_SUCCESS);
}


// initialise report struct
static void report_init(struct report *report)
{
	report->fd = NULL;
	report->asc_used = 0;
	report->bin_used = 0;
	report->last_input = NULL;
}
// open report file
static int report_open(struct report *report, const char *filename)
{
	report->fd = fopen(filename, FILE_WRITETEXT);
	if (report->fd == NULL) {
		fprintf(stderr, "Error: Cannot open report file \"%s\".\n", filename);
		return 1;
	}
	return 0;	// success
}
// close report file
static void report_close(struct report *report)
{
	if (report && report->fd) {
		fclose(report->fd);
		report->fd = NULL;
	}
}


// error handling

// tidy up before exiting by saving symbol list and close other output files
int ACME_finalize(int exit_code)
{
	FILE	*fd;

	report_close(report);
	if (config.symbollist_filename) {
		fd = fopen(config.symbollist_filename, FILE_WRITETEXT);	// FIXME - what if filename is given via !sl in sub-dir? fix path!
		if (fd) {
			symbols_list(fd);
			fclose(fd);
			PLATFORM_SETFILETYPE_TEXT(config.symbollist_filename);
		} else {
			fprintf(stderr, "Error: Cannot open symbol list file \"%s\".\n", config.symbollist_filename);
			exit_code = EXIT_FAILURE;
		}
	}
	if (config.vicelabels_filename) {
		fd = fopen(config.vicelabels_filename, FILE_WRITETEXT);
		if (fd) {
			symbols_vicelabels(fd);
			fclose(fd);
			PLATFORM_SETFILETYPE_TEXT(config.vicelabels_filename);
		} else {
			fprintf(stderr, "Error: Cannot open VICE label dump file \"%s\".\n", config.vicelabels_filename);
			exit_code = EXIT_FAILURE;
		}
	}
	return exit_code;
}


// save output file
static void save_output_file(void)
{
	const char	*body;
	intval_t	amount,
			loadaddr;
	unsigned char	header[4];
	int		headersize	= 0;
	FILE		*fd;

	// if no output file chosen, tell user and do nothing
	if (config.output_filename == NULL) {
		fputs("No output file specified (use the \"-o\" option or the \"!to\" pseudo opcode).\n", stderr);
		return;
	}

	// get memory pointer, block size and load address
	output_get_result(&body, &amount, &loadaddr);

	// generate header according to file format
	switch (config.outfile_format) {
	case OUTFILE_FORMAT_UNSPECIFIED:
	case OUTFILE_FORMAT_PLAIN:
		headersize = 0;	// no header
		break;
	case OUTFILE_FORMAT_CBM:
		if (loadaddr > 0xffff) {
			fprintf(stderr, "Error: Load address 0x%04lx too large for cbm file format.\n", loadaddr);
			exit(EXIT_FAILURE);
		}
		header[0] = loadaddr & 255;
		header[1] = loadaddr >> 8;
		headersize = 2;	// 16-bit load address, little-endian
		break;
	case OUTFILE_FORMAT_APPLE:
		if (loadaddr > 0xffff) {
			fprintf(stderr, "Error: Load address 0x%04lx too large for apple file format.\n", loadaddr);
			exit(EXIT_FAILURE);
		}
		if (amount > 0xffff) {
			fprintf(stderr, "Error: File size 0x%04lx too large for apple file format.\n", loadaddr);
			exit(EXIT_FAILURE);
		}
		header[0] = loadaddr & 255;
		header[1] = loadaddr >> 8;
		header[2] = amount & 255;
		header[3] = amount >> 8;
		headersize = 4;	// 16-bit load address and 16-bit length, little-endian
		break;
	default:
		BUG("IllegalOutformat1", config.outfile_format);
	}

	// open file
	fd = fopen(config.output_filename, FILE_WRITEBINARY);	// FIXME - what if filename is given via !to in sub-dir? fix path!
	if (fd == NULL) {
		fprintf(stderr, "Error: Cannot open output file \"%s\".\n", config.output_filename);
		exit(EXIT_FAILURE);
	}

	if (config.process_verbosity) {
		printf("Saving %ld (0x%04lx) bytes (0x%04lx - 0x%04lx exclusive).\n",
			amount, amount, loadaddr, loadaddr + amount);
	}

	// write header and body
	fwrite(header, headersize, 1, fd);
	fwrite(body, amount, 1, fd);
	fclose(fd);

	// set file type
	switch (config.outfile_format) {
	case OUTFILE_FORMAT_UNSPECIFIED:
	case OUTFILE_FORMAT_PLAIN:
		PLATFORM_SETFILETYPE_PLAIN(config.output_filename);
		break;
	case OUTFILE_FORMAT_APPLE:
		PLATFORM_SETFILETYPE_APPLE(config.output_filename);
		break;
	case OUTFILE_FORMAT_CBM:
		PLATFORM_SETFILETYPE_CBM(config.output_filename);
		break;
	default:
		BUG("IllegalOutformat2", config.outfile_format);
	}
}


// increment pass number and perform a single pass
static void perform_pass(void)
{
	FILE	*fd;
	int	ii;

	++pass.number;
	if (config.process_verbosity > 1)
		printf("Pass %d:\n", pass.number);
	cputype_passinit();	// set default cpu type
	output_passinit();	// set initial pc or start with undefined pc
	encoding_passinit();	// set default encoding
	section_passinit();	// set initial zone (untitled)
	// init variables
	pass.undefined_count = 0;
	//pass.needvalue_count = 0;	FIXME - use
	pass.error_count = 0;
	pass.warning_count = 0;
	// Process toplevel files
	for (ii = 0; ii < toplevel_src_count; ++ii) {
		if ((fd = fopen(toplevel_sources_plat[ii], FILE_READBINARY))) {
			input_parse_and_close_platform_file(toplevel_sources_plat[ii], fd);
		} else {
			fprintf(stderr, "Error: Cannot open toplevel file \"%s\".\n", toplevel_sources_plat[ii]);
			if (toplevel_sources_plat[ii][0] == '-')
				fprintf(stderr, "Options (starting with '-') must be given _before_ source files!\n");
 			++pass.error_count;
		}
	}
	output_endofpass();	// make sure last code segment is closed
	// TODO: atm "--from-to" reads two number literals. if that is changed
	// in the future to two general expressions, this is the point where
	// they would need to be evaluated.
	if (config.process_verbosity > 8)
		printf("Found %d undefined expressions.\n", pass.undefined_count);
	if (pass.error_count)
		exit(ACME_finalize(EXIT_FAILURE));
}


static struct report	global_report;
// do passes until done (or errors occurred).
static void do_actual_work(void)
{
	int	undefs_before;	// number of undefined results in previous pass

	report = &global_report;	// let global pointer point to something
	report_init(report);	// we must init struct before doing passes

	sanity.macro_recursions_left = config.sanity_limit;
	sanity.source_recursions_left = config.sanity_limit;
	sanity.passes_left = config.sanity_limit;

	pass.complain_about_undefined = FALSE;	// disable until error pass needed
	perform_pass();	// first pass
	// pretend there has been a previous pass, with one more undefined result
	undefs_before = pass.undefined_count + 1;
	// keep doing passes as long as the number of undefined results keeps decreasing.
	// stop on zero (FIXME - zero-check pass.needvalue_count instead!)
	while (pass.undefined_count && (pass.undefined_count < undefs_before)) {
		undefs_before = pass.undefined_count;
		perform_pass();
		if (--sanity.passes_left < 0) {
			// FIXME - exit with error
			// ...or maybe do one additional pass where all errors are reported, including "not defined" and "value has changed".
			//puts("Exceeded maximum number of passes, please check your sources.");
			//break;
		}
	}
	// any errors left?
	if (pass.undefined_count == 0) {	// FIXME - use pass.needvalue_count instead!
		// if listing report is wanted and there were no errors,
		// do another pass to generate listing report
		if (config.report_filename) {
			if (config.process_verbosity > 1)
				puts("Extra pass to generate listing report.");
			if (report_open(report, config.report_filename) == 0) {
				perform_pass();
				report_close(report);
			}
			// FIXME - add some code here to do "if there were errors, call BUG()"
		}
		save_output_file();
	} else {
		// There are still errors (unsolvable by doing further passes),
		// so perform additional pass to find and show them.
		if (config.process_verbosity > 1)
			puts("Extra pass to display errors.");
		pass.complain_about_undefined = TRUE;	// activate error output
		perform_pass();	// perform pass, but now show "value undefined"
		// FIXME - perform_pass() calls exit() when there were errors,
		// so if controls returns here, call BUG()!
		// (this can be triggered using ifdef/ifndef)
	}
}


// copy string to DynaBuf
static void keyword_to_dynabuf(const char keyword[])
{
	dynabuf_clear(GlobalDynaBuf);
	dynabuf_add_string(GlobalDynaBuf, keyword);
	dynabuf_append(GlobalDynaBuf, '\0');
	dynabuf_to_lower(GlobalDynaBuf, GlobalDynaBuf);	// convert to lower case
}


// set output format
static void set_output_format(const char format_name[])
{
	// caution, name may be NULL!
	if (format_name) {
		keyword_to_dynabuf(format_name);
		config.outfile_format = outputformat_find();
		if (config.outfile_format != OUTFILE_FORMAT_UNSPECIFIED)
			return;	// ok

		fputs("Error: Unknown output format.\n", stderr);
	} else {
		fputs("Error: No output format specified.\n", stderr);
	}
	fprintf(stderr, "Supported formats are:\n\n\t%s\n\n", outputformat_names);
	exit(EXIT_FAILURE);
}


// set CPU type
static void set_starting_cpu(const char cpu_name[])
{
	const struct cpu_type	*new_cpu_type;

	// caution, name may be NULL!
	if (cpu_name) {
		keyword_to_dynabuf(cpu_name);
		new_cpu_type = cputype_find();
		if (new_cpu_type) {
			config.initial_cpu_type = new_cpu_type;
			return;	// ok
		}
		fputs("Error: Unknown CPU type.\n", stderr);
	} else {
		fputs("Error: No CPU type specified.\n", stderr);
	}
	fprintf(stderr, "Supported types are:\n\n\t%s\n\n", cputype_names);
	exit(EXIT_FAILURE);
}


static void could_not_parse(const char strange[])
{
	fprintf(stderr, "%sCould not parse '%s'.\n", cliargs_error, strange);
	exit(EXIT_FAILURE);
}


// return signed long representation of string.
// copes with hexadecimal if prefixed with "$", "0x" or "0X".
// copes with octal if prefixed with "&".	FIXME - add "0o" prefix?
// copes with binary if prefixed with "%".	FIXME - add "0b" prefix!
// assumes decimal otherwise.
static signed long string_to_number(const char *string)
{
	signed long	result;
	char		*end;
	int		base	= 10;

	if (*string == '%') {
		base = 2;
		++string;
	} else if (*string == '&') {
		base = 8;
		++string;
	} else if (*string == '$') {
		base = 16;
		++string;
	} else if ((*string == '0') && ((string[1] == 'x') || (string[1] == 'X'))) {
		base = 16;
		string += 2;
	}
	result = strtol(string, &end, base);
	if (*end)
		could_not_parse(end);
	return result;
}
// wrapper for fn above: complain about negative numbers
static signed long string_to_nonneg_number(const char *string)
{
	signed long	result	= string_to_number(string);

	if (result < 0) {
		fprintf(stderr, "%sInvalid value, number is negative: '%s'.\n", cliargs_error, string);
		exit(EXIT_FAILURE);
	}
	return result;
}


// set initial memory contents
static void set_mem_contents(const char expression[])
{
	config.mem_init_value = string_to_number(expression);
	if ((config.mem_init_value < -128) || (config.mem_init_value > 255)) {
		fprintf(stderr, "%sInitmem value out of range (0-0xff).\n", cliargs_error);
		exit(EXIT_FAILURE);
	}
}


// define symbol
static void define_symbol(const char definition[])
{
	const char	*walk	= definition;
	signed long	value;

	// copy definition to GlobalDynaBuf until '=' reached	
	dynabuf_clear(GlobalDynaBuf);
	while ((*walk != '=') && (*walk != '\0'))
		dynabuf_append(GlobalDynaBuf, *walk++);
	if ((*walk == '\0') || (walk[1] == '\0'))
		could_not_parse(definition);
	// TODO - if first char is double quote, maybe interpret as string instead of number?
	value = string_to_number(walk + 1);
	dynabuf_append(GlobalDynaBuf, '\0');
	symbol_define(value);
}


struct dialect_info {
	enum dialect	dialect;
	const char	*version;
	const char	*description;
};
struct dialect_info	dialects[]	= {
	{V0_85__OLDEST_SUPPORTED,	"0.85",		"(the oldest version supported)"},
	{V0_86__DEPRECATE_REALPC,	"0.86",		"\"!realpc\" gives a warning, \"!to\" wants a file format"},
//	{V0_93__SHORTER_SETPC_WARNING,	"0.93",		"\"*=\" in offset assembly gives shorter warning but still switches off"},
	{V0_94_6__RIGHT_ASSOC_POWER,	"0.94.6",	"\"power of\" is now right-associative"},
//	{V,				"0.94.7",	"empty code segments are no longer included in output file"},
	{V0_94_8__DISABLED_OBSOLETE,	"0.94.8",	"\"*=\" works inside \"!pseudopc\", disabled \"!cbm/!realpc/!subzone\""},
	{V0_94_12__NEW_FOR_SYNTAX,	"0.94.12",	"new \"!for\" syntax"},
	{V0_95_2__NEW_ANC_OPCODE,	"0.95.2",	"changed ANC#8 from 0x2b to 0x0b"},
	{V0_97__BACKSLASH_ESCAPING,	"0.97",		"backslash escaping and strings"},
//	{V0_98__PATHS_AND_SYMBOLCHANGE,	"0.98",		"paths are relative to current file"},
//	{V__CURRENT_VERSION,		"default",	"default"},
	{V__FUTURE_VERSION,		"future",	"enable all experimental features"},
	{0,				NULL,		NULL}	// NULLs terminate
};

// choose dialect (mimic behaviour of different version)
static void set_dialect(const char version[])
{
	struct dialect_info	*dia;

	// caution, version may be NULL!
	if (version) {
		// scan array
		for (dia = dialects; dia->version; ++dia) {
			if (strcmp(version, dia->version) == 0) {
				config.dialect = dia->dialect;
				return;	// found
			}
		}
		fputs("Error: Unknown dialect specifier.\n", stderr);
	} else {
		fputs("Error: No dialect specified.\n", stderr);
	}
	// output table of possible versions and die
	fputs(
"Supported dialects are:\n"
"\n"
"\tdialect\t\tdescription\n"
"\t-------\t\t-----------\n", stderr);
	for (dia = dialects; dia->version; ++dia)
		fprintf(stderr, "\t%s\t\t%s\n", dia->version, dia->description);
	fputc('\n', stderr);
	exit(EXIT_FAILURE);
}


// handle long options (like "--example"). Return unknown string.
static const char *long_option(const char *string)
{
	if (strcmp(string, OPTION_HELP) == 0)
		show_help_and_exit();
	else if (strcmp(string, OPTION_FORMAT) == 0)
		set_output_format(cliargs_get_next());	// NULL is ok (handled like unknown)
	else if (strcmp(string, OPTION_OUTFILE) == 0)
		config.output_filename = cliargs_safe_get_next(name_outfile);
	else if (strcmp(string, OPTION_LABELDUMP) == 0)	// old
		config.symbollist_filename = cliargs_safe_get_next(arg_symbollist);
	else if (strcmp(string, OPTION_SYMBOLLIST) == 0)	// new
		config.symbollist_filename = cliargs_safe_get_next(arg_symbollist);
	else if (strcmp(string, OPTION_VICELABELS) == 0)
		config.vicelabels_filename = cliargs_safe_get_next(arg_vicelabels);
	else if (strcmp(string, OPTION_REPORT) == 0)
		config.report_filename = cliargs_safe_get_next(arg_reportfile);
	else if (strcmp(string, OPTION_SETPC) == 0)
		config.initial_pc = string_to_nonneg_number(cliargs_safe_get_next("program counter"));
	else if (strcmp(string, OPTION_FROM_TO) == 0) {
		config.outfile_start = string_to_nonneg_number(cliargs_safe_get_next("start address of output file"));
		config.outfile_limit = string_to_nonneg_number(cliargs_safe_get_next("end+1 of output file"));
	} else if (strcmp(string, OPTION_CPU) == 0)
		set_starting_cpu(cliargs_get_next());	// NULL is ok (handled like unknown)
	else if (strcmp(string, OPTION_INITMEM) == 0)
		set_mem_contents(cliargs_safe_get_next("initmem value"));
	else if (strcmp(string, OPTION_MAXERRORS) == 0)
		config.max_errors = string_to_number(cliargs_safe_get_next("maximum error count"));
	else if (strcmp(string, OPTION_MAXDEPTH) == 0)
		config.sanity_limit = string_to_number(cliargs_safe_get_next("recursion depth"));
	else if (strcmp(string, OPTION_USE_STDOUT) == 0)
		config.msg_stream = stdout;
	else if (strcmp(string, OPTION_MSVC) == 0)
		config.format_msvc = TRUE;
	else if (strcmp(string, OPTION_FULLSTOP) == 0)
		config.pseudoop_prefix = '.';
	else if (strcmp(string, OPTION_IGNORE_ZEROES) == 0)
		config.honor_leading_zeroes = FALSE;
	else if (strcmp(string, OPTION_STRICT_SEGMENTS) == 0)
		config.debuglevel_segmentprobs = DEBUGLEVEL_ERROR;
	else if (strcmp(string, OPTION_STRICT) == 0)
		config.all_warnings_are_errors = TRUE;
	else if (strcmp(string, OPTION_DIALECT) == 0)
		set_dialect(cliargs_get_next());	// NULL is ok (handled like unknown)
	else if (strcmp(string, OPTION_DEBUGLEVEL) == 0)
		config.debuglevel = string_to_number(cliargs_safe_get_next("debug level"));
	else if (strcmp(string, OPTION_TEST) == 0) {
		config.dialect = V__FUTURE_VERSION;
		config.test_new_features = TRUE;
		config.outbuf_size = 0x1000000;	// 16 MiB (FIXME - give it its own cli switch!)
	} PLATFORM_LONGOPTION_CODE
	else if (strcmp(string, OPTION_COLOR) == 0)
		config.format_color = TRUE;
	else if (strcmp(string, OPTION_VERSION) == 0)
		show_version(TRUE);
	else
		return string;
	return NULL;
}


// handle short options (like "-e"). Return unknown character.
static char short_option(const char *argument)
{
	while (*argument) {
		switch (*argument) {
		case 'D':	// "-D" define constants
			define_symbol(argument + 1);
			goto done;
		case 'f':	// "-f" selects output format
			set_output_format(cliargs_get_next());	// NULL is ok (handled like unknown)
			break;
		case 'h':	// "-h" shows help
			show_help_and_exit();
			break;
		case 'I':	// "-I" adds an include directory
			if (argument[1])
				includepaths_add(argument + 1);
			else
				includepaths_add(cliargs_safe_get_next("include path"));
			goto done;
		case 'l':	// "-l" selects symbol list filename
			config.symbollist_filename = cliargs_safe_get_next(arg_symbollist);
			break;
		case 'o':	// "-o" selects output filename
			config.output_filename = cliargs_safe_get_next(name_outfile);
			break;
		case 'r':	// "-r" selects report filename
			config.report_filename = cliargs_safe_get_next(arg_reportfile);
			break;
		case 'v':	// "-v" changes verbosity
			++config.process_verbosity;
			if ((argument[1] >= '0') && (argument[1] <= '9'))
				config.process_verbosity = *(++argument) - '0';
			break;
		// platform specific switches are inserted here
			PLATFORM_SHORTOPTION_CODE
		case 'V':	// "-V" shows version
			show_version(TRUE);
			break;
		case 'W':	// "-W" tunes warning level
			if (strcmp(argument + 1, OPTIONWNO_LABEL_INDENT) == 0) {
				config.warn_on_indented_labels = FALSE;
				goto done;
			} else if (strcmp(argument + 1, OPTIONWNO_OLD_FOR) == 0) {
				config.dialect = V0_94_12__NEW_FOR_SYNTAX - 1;
				goto done;
			} else if (strcmp(argument + 1, OPTIONWNO_BIN_LEN) == 0) {
				config.warn_bin_mask = 0;
				goto done;
			} else if (strcmp(argument + 1, OPTIONWTYPE_MISMATCH) == 0) {
				config.warn_on_type_mismatch = TRUE;
				goto done;
			} else {
				fprintf(stderr, "%sUnknown warning level.\n", cliargs_error);
				exit(EXIT_FAILURE);
			}
			break;
		default:	// unknown ones: program termination
			return *argument;
		}
		++argument;
	}
done:
	return '\0';
}


// guess what
int main(int argc, const char *argv[])
{
	config_default(&config);
	// if called without any arguments, show usage info (not full help)
	if (argc == 1)
		show_help_and_exit();
	cliargs_init(argc, argv);
	// init var that may be needed for -DSYMBOL=VALUE
	pass.number = PASS_NUMBER_EARLY;
	// init platform-specific stuff.
	// this may read the library path from an environment variable.
	PLATFORM_INIT;
	// handle command line arguments
	cliargs_handle_options(short_option, long_option);
	// generate list of files to process
	cliargs_get_rest(&toplevel_src_count, &toplevel_sources_plat, "No top level sources given");

	// now that we have processed all cli switches, check a few values for
	// valid range:
	if ((config.initial_pc != NO_VALUE_GIVEN) && (config.initial_pc >= config.outbuf_size)) {
		fprintf(stderr, "%sProgram counter exceeds outbuffer size.\n", cliargs_error);
		exit(EXIT_FAILURE);
	}
	if ((config.outfile_start != NO_VALUE_GIVEN) && (config.outfile_start >= config.outbuf_size)) {
		fprintf(stderr, "%sStart address of output file exceeds outbuffer size.\n", cliargs_error);
		exit(EXIT_FAILURE);
	}
	// "limit" is end+1 and therefore we need ">" instead of ">=":
	if ((config.outfile_limit != NO_VALUE_GIVEN) && (config.outfile_limit > config.outbuf_size)) {
		fprintf(stderr, "%sEnd+1 of output file exceeds outbuffer size.\n", cliargs_error);
		exit(EXIT_FAILURE);
	}
	if ((config.outfile_start != NO_VALUE_GIVEN)
	&& (config.outfile_limit != NO_VALUE_GIVEN)
	&& (config.outfile_start >= config.outfile_limit)) {
		fprintf(stderr, "%sStart address of output file exceeds end+1.\n", cliargs_error);
		exit(EXIT_FAILURE);
	}

	// init output buffer
	output_createbuffer();
	// do the actual work
	do_actual_work();
	return ACME_finalize(EXIT_SUCCESS);	// dump labels, if wanted
}
