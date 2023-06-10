#include "pch.h"
#include "../Lucc/Compiler/Compiler.h"

using namespace lucc;

TEST(Lucc, TestName) 
{
	CompilerInput compiler_input{};
	compiler_input.input_directory = "Test1";
	compiler_input.sources = { "test.txt" };
	compiler_input.exe_file =  "test.exe";
	compiler_input.flags = CompilerFlag_OutputDebugInfo;
	int exit_code = Compile(compiler_input);
	EXPECT_EQ(exit_code, 16);
}