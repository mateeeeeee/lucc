#pragma once

namespace lumen
{
	struct SourceLocation
	{
		char const* filename = nullptr;
		uint32 line = 0;
		uint32 column = 0;
	};
}