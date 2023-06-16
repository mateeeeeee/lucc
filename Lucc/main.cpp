#include "Compiler/Compiler.h"
#include "Core/CLIParser.h"
#include <iostream>

using namespace lucc;

void PrintHelp();

int main(int argc, char** argv)
{
	CLIParser parser{};
	CLIArg& help = parser.AddArg(false, "-h", "--help");
	CLIArg& no_linking = parser.AddArg(false, "-c", "--nolink");
	CLIArg& no_assembling = parser.AddArg(false, "-S", "--noassembly");
	CLIArg& only_preprocessor = parser.AddArg(false, "-E", "--only-pp");
	CLIArg& ast_dump = parser.AddArg(false, "-ast-dump");
	CLIArg& file_directory = parser.AddArg(true, "-d");
	CLIArg& output_debug = parser.AddArg(false, "-debug");
	CLIArg& test = parser.AddArg(false, "-test");
	CLIArg& input = parser.AddArg(true, "-i");

	parser.Parse(argc, argv);

	if (help)
	{
		PrintHelp();
		return 0;
	}

	if (test)
	{
		int exit_code = CompileTest(input.AsStringOr(""), output_debug);
		return exit_code;
	}

	CompilerFlags flags = CompilerFlag_None;
	if (no_linking) flags |= CompilerFlag_NoLinking;
	if (no_assembling) flags |= CompilerFlag_NoAssembling;
	if (only_preprocessor) flags |= CompilerFlag_OnlyPreprocessor;
	if (ast_dump) flags |= CompilerFlag_DumpAST;
	if (output_debug) flags |= CompilerFlag_OutputDebugInfo;

	CompilerInput compiler_input{};
	compiler_input.input_directory = file_directory.AsStringOr("");
	compiler_input.sources = { "test.c" };
	compiler_input.exe_file = "test.exe";
	compiler_input.flags = flags;
	int exit_code = Compile(compiler_input);
	return exit_code;
}

void PrintHelp()
{
	printf("The following options are available:\n");
	printf("-h, --help: for displaying available compile options\n");
	printf("-c, --nolink: no linking is preformed, only .obj files are produced\n");
	printf("-S, --noassembly: no assembling is preformed, only .asm files are produced \n");
	printf("-E, --only-pp: Only preprocessor is run, the o \n");
	printf("-d: Directory where the source files are located \n");
	printf("-ast-dump: Only preprocessor is run \n");
	printf("-test: used for running g-tests \n");
	printf("-i: input test code \n");
}

