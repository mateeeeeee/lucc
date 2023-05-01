#pragma once
#include "Core/Enums.h"

namespace lucc
{
	enum CompilerFlag : uint32 
	{
		CompilerFlag_None = 0x0,
	};
	DEFINE_ENUM_BIT_OPERATORS(CompilerFlag);
	using CompilerFlags = uint32;

	struct CompilerInput
	{
		std::string_view input;
		std::string_view output;
		CompilerFlags	 options;
	};

	bool Compile(CompilerInput&);
}