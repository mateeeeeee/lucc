#ifndef _LIB

#include "Compiler/Compiler.h"
#include "Core/CLIParser.h"
#include <iostream>

using namespace lucc;

int main(int argc, char** argv)
{
	CLIParser parser{};
	CLIArg& opt = parser.AddArg(false, "-opt");
	parser.Parse(argc, argv);

	CompilerInput compiler_input{};
	compiler_input.sources = { "test.txt" };
	compiler_input.exe_file = "test.exe";
	compiler_input.flags = CompilerFlag_OutputDebugInfo;
	int exit_code = Compile(compiler_input);
	printf("Exit code: %d", exit_code);
}

#endif
