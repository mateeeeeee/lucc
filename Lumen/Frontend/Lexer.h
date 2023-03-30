#pragma once
#include <vector>
#include "Frontend/Token.h"

namespace lumen
{
	class SourceBuffer;

	enum class LexerResult : uint8
	{
		Ok,
		UnknownToken
	};

	class Lexer
	{
		friend class Preprocessor;

	public:
		explicit Lexer(SourceBuffer const& source);
		Lexer(const Lexer&) = delete;
		Lexer& operator=(const Lexer&) = delete;

		LexerResult Lex();

		std::vector<Token> const& GetTokens() const { return tokens; }

	private:
		char const* buf_start;
		char const* buf_end;
		char const* buf_ptr;

		SourceLocation loc;
		std::vector<Token> tokens;
	private:

		bool NextToken(Token&);

		char PeekChar()
		{
			return *(buf_ptr + 1);
		}
		char CurrentChar()
		{
			return *buf_ptr;
		}
		char NextChar()
		{
			return *buf_ptr++;
		}
		void PopChar(char& c)
		{
			c = *buf_ptr;
			--buf_ptr;
		}
	};
}