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
	};
	DEFINE_ENUM_BIT_OPERATORS(CompilerFlag);
	using CompilerFlags = uint32;

	struct CompilerInput
	{
		std::string input_directory;
		std::vector<std::string_view> sources;
		std::string_view exe_file;
		CompilerFlags	 flags;
	};

	int Compile(CompilerInput const&);
}