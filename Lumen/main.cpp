#include "Compiler.h"
#include "Core/CLIParser.h"
#include <iostream>

using namespace lu;

int main(int argc, char** argv)
{
	//cmd
	CLIParser parser{};
	CLIArg& opt = parser.AddArg(false, "-opt");
	parser.Parse(argc, argv);

	//#todo add diagnostics 
	lu::CompilerInput compiler_input{};
	compiler_input.input = "test.txt";
	compiler_input.options = lu::CompilerOpt_None;
	bool result = Compile(compiler_input);
}