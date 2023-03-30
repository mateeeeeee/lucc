#pragma once
#include "Frontend/Token.h"


namespace lumen
{
	class Lexer
	{
	public:
		explicit Lexer(SourceBuffer const& source);



	private:
		char const* buf_start;
		char const* buf_end;
		char const* buf_ptr;

	private:

	};
}