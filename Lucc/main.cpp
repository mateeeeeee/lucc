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
	compiler_input.source = "test.txt";
	compiler_input.assembly = "test.asm";
	compiler_input.object = "test.obj";
	compiler_input.executable = "test.exe";
	compiler_input.flags = CompilerFlag_OutputDebugInfo;
	bool result = Compile(compiler_input);
}