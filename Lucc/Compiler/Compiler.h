#pragma once
#include "Core/Enums.h"

namespace lucc
{
	enum CompilerFlag : uint32 
	{
		CompilerFlag_None = 0x0,
		CompilerFlag_OutputDebugInfo = 0x10,
	};
	DEFINE_ENUM_BIT_OPERATORS(CompilerFlag);
	using CompilerFlags = uint32;

	struct CompilerInput
	{
		std::string_view source;
		std::string_view assembly;
		std::string_view object;
		std::string_view executable;
		CompilerFlags	 flags;
	};

	bool Compile(CompilerInput const&);
}