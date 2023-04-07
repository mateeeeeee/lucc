#include "Compiler.h"
#include "Core/CLIParser.h"
#include <iostream>

using namespace lucc;

int main(int argc, char** argv)
{
	//cmd
	CLIParser parser{};
	CLIArg& opt = parser.AddArg(false, "-opt");
	parser.Parse(argc, argv);

	//#todo add diagnostics 
	lucc::CompilerInput compiler_input{};
	compiler_input.input = "test.txt";
	compiler_input.options = lucc::CompilerOpt_None;
	bool result = Compile(compiler_input);
}