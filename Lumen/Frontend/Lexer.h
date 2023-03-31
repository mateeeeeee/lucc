#pragma once
#include <vector>
#include <string>
#include <functional>
#include "Frontend/Token.h"

namespace lumen
{
	class SourceBuffer;

	template<typename P>
	concept CharPredicate = requires(P p, char a)
	{
		{ p(a) } -> std::convertible_to<bool>;
	};

	template<typename F, typename T>
	concept CArrayToValueTransform = requires(F f, char const* start, char const* end)
	{
		{ f(start, end) } -> std::convertible_to<T>;
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
		bool LexComment(Token&);
		bool LexPunctuator(Token&);

		void UpdatePointersAndLocation()
		{
			loc.NewChars(cur_ptr - buf_ptr);
			buf_ptr = cur_ptr;
		}

		template<CharPredicate P>
		void ReadUntil(char const* start, P&& predicate)
		{
			while (predicate(start++));
		}

		template<CharPredicate P>
		void FillToken(Token& t, TokenType type, char const* start, P&& predicate)
		{
			t.SetLocation(loc);
			t.SetType(type);
			char const* tmp_ptr = cur_ptr;
			ReadUntil(tmp_ptr, std::forward<P>(predicate));
			cur_ptr = tmp_ptr;
		}
		template<typename T, CArrayToValueTransform<T> F, CharPredicate P>
		void FillToken(Token& t, TokenType type, char const* start, P&& predicate, F&& transform)
		{
			t.SetLocation(loc);
			t.SetType(type);
			char const* tmp_ptr = cur_ptr;
			ReadUntil(tmp_ptr, std::forward<P>(predicate));
			t.SetData<T>(transform(cur_ptr, tmp_ptr));
			cur_ptr = tmp_ptr;
		}
	};
}