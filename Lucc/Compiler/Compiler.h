#pragma once
#include <vector>
#include "Core/Enums.h"

namespace lucc
{
	enum CompilerFlag : uint32
	{
		CompilerFlag_None = 0x0,
		CompilerFlag_NoLinking = 0x1,
		CompilerFlag_NoAssembling = 0x2,
		CompilerFlag_OnlyPreprocessor = 0x4,
		CompilerFlag_OutputDebugInfo = 0x10,
		CompilerFlag_DumpAST = 0x20,
		CompilerFlag_NoDefaultLibs = 0x40
	};
	DEFINE_ENUM_BIT_OPERATORS(CompilerFlag);
	using CompilerFlags = uint32;

	enum class CompilerOutput
	{
		Exe,
		Dll,
		Lib
	};

	struct CompilerInput
	{
		CompilerFlags flags;
		std::string input_directory;
		std::vector<std::string> sources;
		std::string output_file;
		CompilerOutput output_type = CompilerOutput::Exe;
	};

	int Compile(CompilerInput const&);
	int CompileTest(std::string_view, bool debug = false);
}