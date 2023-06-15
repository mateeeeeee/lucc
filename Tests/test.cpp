#include "pch.h"
#include "../Lucc/Core/Defines.h"
#include "../Lucc/Compiler/Compiler.h"
#include "../Lucc/Frontend/Diagnostics.h"

using namespace lucc;
using namespace diag;

#define LU_TEST(...) LU_STRINGIFY(__VA_ARGS__)

TEST(Lucc, Test1)
{
	//CompilerInput compiler_input{};
	//compiler_input.input_directory = "Test1";
	//compiler_input.exe_file = "test.exe";
	//compiler_input.flags = CompilerFlag_None;
	//
	//compiler_input.sources = { "test1.c" };
	//int exit_code = Compile(compiler_input);
	//EXPECT_EQ(exit_code, 16);

	EXPECT_EQ(CompileTest(LU_TEST(int x; x = 5; return x << 2;)), 20);
	EXPECT_EQ(CompileTest(LU_TEST(int x, y; x = 5, y = 7; return x * y;)), 35);
}