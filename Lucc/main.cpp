#include "Compiler/Compiler.h"
#include "CLI/CLI.hpp"

using namespace lucc;

void PrintHelp();

int main(int argc, char** argv)
{
	CLI::App cli_parser{ "lucc - little & useless C compiler" };
	cli_parser.remove_option(cli_parser.get_help_ptr());
	bool help = false;
	cli_parser.add_flag("-h,--help", help);
	CLI::Option* debug = cli_parser.add_flag("--debug");
	CLI::Option* no_linking = cli_parser.add_flag("-c");
	CLI::Option* no_assembling = cli_parser.add_flag("-S");
	CLI::Option* only_preprocessor = cli_parser.add_flag("-E");
	CLI::Option* ast_dump = cli_parser.add_flag("--astdump");
	CLI::Option* dll = cli_parser.add_flag("--dll");
	CLI::Option* lib = cli_parser.add_flag("--lib");
	CLI::Option* test = cli_parser.add_flag("--test");

	std::vector<std::string> input_files;
	CLI::Option* input_files_opt = cli_parser.add_option("-i", input_files, "Files to be compiled");

	std::string output_file;
	CLI::Option* output_file_opt = cli_parser.add_option("-o", output_file);
	
	std::string file_directory;
	CLI::Option* file_directory_opt = cli_parser.add_option("-d", file_directory);
	
	std::string test_input;
	CLI::Option* test_input_opt = cli_parser.add_option("-t", test_input);
	{
		dll->excludes(lib);
		lib->excludes(dll);
		ast_dump->excludes(only_preprocessor);
		output_file_opt->default_val("a");
		file_directory_opt->default_val("");
		test_input_opt->needs(test);
	}
	CLI11_PARSE(cli_parser, argc, argv);

	if (help)
	{
		PrintHelp();
		return 0;
	}

	if (test)
	{
		int32 exit_code = CompileTest(test_input, debug);
		return exit_code;
	}

	CompilerFlags flags = CompilerFlag_None;
	if (no_linking) flags |= CompilerFlag_NoLinking;
	if (no_assembling) flags |= CompilerFlag_NoAssembling;
	if (only_preprocessor) flags |= CompilerFlag_OnlyPreprocessor;
	if (ast_dump) flags |= CompilerFlag_DumpAST;
	if (debug) flags |= CompilerFlag_OutputDebugInfo;

	CompilerInput compiler_input{};
	compiler_input.flags = flags;
	compiler_input.input_directory = file_directory;
	compiler_input.sources = input_files;
	compiler_input.output_file = output_file;
	if (dll) compiler_input.output_type = CompilerOutput::Dll;
	else if (lib) compiler_input.output_type = CompilerOutput::Lib;
	else compiler_input.output_type = CompilerOutput::Exe;

	int exit_code = Compile(compiler_input);
	return exit_code;
}

void PrintHelp()
{
	printf("The following options are available:\n");
	printf("-h: for displaying available compile options\n");
	printf("-c: no linking is preformed, only .obj files are produced\n");
	printf("-S: no assembling is preformed, only .asm files are produced \n");
	printf("-E: Only preprocessor is run\n");
	printf("-d: Directory where the source files are located \n");
	printf("-o: Output file \n");
	printf("-i: Input files \n");
	printf("--dll: builds a dll \n");
	printf("--lib: builds a lib \n");
	printf("--astdump: Dump AST to console\n");
	printf("--test: used for running g-tests\n");
	printf("--ti: input test code (used for g-tests)\n");
}

