#pragma once
#include <string>

namespace lucc
{
	struct SourceLocation
	{
		std::string filename = "";
		uint32 line = 1;
		uint32 column = 1;

		SourceLocation operator+(int32 i)
		{
			return SourceLocation
			{
				.filename = filename,
				.line = line,
				.column = column + i
			};
		}

		void NewChar()
		{
			++column;
		}
		void NewChars(int32 i)
		{
			column += i;
		}
		void NewLine()
		{
			++line;
			column = 1;
		}
	};
}