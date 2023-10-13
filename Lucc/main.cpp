#include "Compiler/Compiler.h"
#include "CLI/CLI.hpp"
#include "Core/Logger.h"

using namespace lucc;

int main(int argc, char** argv)
{
	CLI::App cli_parser{ "lucc - little & useless C compiler" };

	CLI::Option* debug = cli_parser.add_flag("--debug", "Print debug information during compilation process");
	CLI::Option* no_linking = cli_parser.add_flag("-c",  "no linking is preformed, only .obj files are produced");
	CLI::Option* no_assembling = cli_parser.add_flag("-S", "no assembling is preformed, only .asm files are produced");
	CLI::Option* only_pp = cli_parser.add_flag("-E", "Only preprocessor is run");
	CLI::Option* ast_dump = cli_parser.add_flag("--astdump", "Print AST tree");
	CLI::Option* dll = cli_parser.add_flag("--dll", "Build a dll file");
	CLI::Option* lib = cli_parser.add_flag("--lib", "Build a lib file");
	CLI::Option* test = cli_parser.add_flag("--test", "used for running g-tests");

	std::vector<std::string> input_files;
	CLI::Option* input_files_opt = cli_parser.add_option("-i", input_files, "Input files");

	std::string output_file;
	CLI::Option* output_file_opt = cli_parser.add_option("-o", output_file, "Output file");
	
	std::string file_directory;
	CLI::Option* file_directory_opt = cli_parser.add_option("--directory", file_directory, "File directory of input files");
	
	std::string test_input;
	CLI::Option* test_input_opt = cli_parser.add_option("-t", test_input, "input test code in form of a string");

	CLI11_PARSE(cli_parser, argc, argv);

	if (input_files.empty() && test_input.empty())
	{
		LU_WARN("No input files provided!");
		return 0;
	}
	if(output_file.empty()) output_file = "out";

	if (*test)
	{
		int32 exit_code = CompileTest(test_input, (bool)*debug);
		return exit_code;
	}

	CompilerFlags flags = CompilerFlag_None;
	if (*no_linking) flags |= CompilerFlag_NoLinking;
	if (*no_assembling) flags |= CompilerFlag_NoAssembling;
	if (*only_pp) flags |= CompilerFlag_OnlyPreprocessor;
	if (*ast_dump) flags |= CompilerFlag_DumpAST;
	if (*debug) flags |= CompilerFlag_OutputDebugInfo;

	CompilerInput compiler_input{};
	compiler_input.flags = flags;
	compiler_input.input_directory = file_directory;
	compiler_input.sources = input_files;
	compiler_input.output_file = output_file;
	if (*dll) compiler_input.output_type = CompilerOutput::Dll;
	else if (*lib) compiler_input.output_type = CompilerOutput::Lib;
	else compiler_input.output_type = CompilerOutput::Exe;

	int32 exit_code = Compile(compiler_input);
	return exit_code;
}
