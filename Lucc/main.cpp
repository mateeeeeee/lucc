#include "Compiler.h"
#include "Core/CLIParser.h"
#include <iostream>

using namespace lucc;

int main(int argc, char** argv)
{
	CLIParser parser{};
	CLIArg& opt = parser.AddArg(false, "-opt");
	parser.Parse(argc, argv);

	CompilerInput compiler_input{};
	compiler_input.input = "test.txt";
	compiler_input.output = "test2.txt";
	compiler_input.options = CompilerFlag_None;
	bool result = Compile(compiler_input);
}