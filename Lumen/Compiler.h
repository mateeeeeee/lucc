#pragma once
#include "Core/Enums.h"

namespace lu
{

	enum CompilerOpt : uint32 
	{
		CompilerOpt_None = 0x0,
		CompilerOpt_UseVM = 0x1,
		CompilerOpt_SkipOptimizations = 0x2,
	};
	DEFINE_ENUM_BIT_OPERATORS(CompilerOpt);
	using CompilerOpts = uint32;

	struct CompilerInput
	{
		std::string_view input;
		std::string_view output;
		CompilerOpts	 options;
	};

	bool Compile(CompilerInput&);
}