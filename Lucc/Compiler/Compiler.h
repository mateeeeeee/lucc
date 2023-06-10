#pragma once
#include <vector>
#include "Core/Enums.h"

namespace lucc
{
	enum CompilerFlag : uint32 
	{
		CompilerFlag_None = 0x0,
		CompilerFlag_OutputDebugInfo = 0x10,
		CompilerFlag_DumpAST = 0x20,
	};
	DEFINE_ENUM_BIT_OPERATORS(CompilerFlag);
	using CompilerFlags = uint32;

	struct CompilerInput
	{
		std::vector<std::string_view> sources;
		std::string_view exe_file;
		CompilerFlags	 flags;
	};

	void Compile(CompilerInput const&);
}