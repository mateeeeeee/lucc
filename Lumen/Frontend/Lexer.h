#pragma once
#include <vector>
#include <string>
#include <functional>
#include "Frontend/Token.h"

namespace lu
{
	class SourceBuffer;

	template<typename P>
	concept CharPredicate = requires(P p, char a)
	{
		{ p(a) } -> std::convertible_to<bool>;
	};

	class Lexer
	{
		friend class Preprocessor;

	public:
		explicit Lexer(SourceBuffer const& source);
		Lexer(Lexer const&) = delete;
		Lexer& operator=(Lexer const&) = delete;

		bool Lex();

		std::vector<Token> const& GetTokens() const { return tokens; }

	private:
		char const* buf_ptr;
		char const* cur_ptr;

		SourceLocation loc;
		std::vector<Token> tokens;
	private:

		bool LexToken(Token&);
		bool LexNumber(Token&);
		bool LexIdentifier(Token&);
		bool LexString(Token&);
		bool LexEndOfFile(Token&);
		bool LexNewLine(Token&);
		bool LexComment(Token&);
		bool LexPunctuator(Token&);

		void UpdatePointersAndLocation()
		{
			loc.NewChars(static_cast<int32>(cur_ptr - buf_ptr));
			buf_ptr = cur_ptr;
		}

		template<CharPredicate P>
		void ReadUntil(char const*& start, P&& predicate)
		{
			for (; predicate(*start); ++start);
		}

		void FillToken(Token& t, TokenType type, char const* end)
		{
			t.SetLocation(loc);
			t.SetType(type);
			t.SetData(cur_ptr, end);
			cur_ptr = end;
		}
		template<CharPredicate P>
		void FillToken(Token& t, TokenType type, P&& predicate)
		{
			t.SetLocation(loc);
			t.SetType(type);
			char const* tmp_ptr = cur_ptr;
			ReadUntil(tmp_ptr, std::forward<P>(predicate));
			t.SetData(cur_ptr, tmp_ptr);
			cur_ptr = tmp_ptr;
		}
	};
}