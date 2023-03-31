#include "Lexer.h"
#include "SourceBuffer.h"

namespace lumen
{
	namespace
	{
		std::string StringTransform(const char* start, const char* end)
		{
			return std::string(start, end - start);
		}

		int32 IntegerTransform(const char* start, const char* end)
		{
			std::string number_string(start, end - start);
			return std::stoi(number_string, nullptr, 0);
		}
	}

	Lexer::Lexer(SourceBuffer const& source) : buf_ptr(source.GetBufferStart()), cur_ptr(buf_ptr),
											   loc{ .filename = source.GetRefName().data()} {}

	bool Lexer::Lex()
	{
		Token current_token{};
		do
		{
			current_token.Reset();
			bool result = LexToken(current_token);

			if (!result) return false;
			if (tokens.back().Is(TokenType::newline)) current_token.SetFlag(TokenFlagBit_BeginningOfLine);

			tokens.push_back(current_token);
		} while (!current_token.Is(TokenType::eof));
		return true;
	}

	bool Lexer::LexToken(Token& token)
	{
		UpdatePointersAndLocation();
		if ((*cur_ptr == ' ') || (*cur_ptr == '\t'))
		{
			++cur_ptr;
			while ((*cur_ptr == ' ') || (*cur_ptr == '\t')) ++cur_ptr;
			token.SetFlag(TokenFlagBit_LeadingSpace);
			UpdatePointersAndLocation();
		}

		char c = *cur_ptr++;
		switch (c)
		{
		case '\0':
			return LexEndOfFile(token);
		case '\n':
		{
			loc.NewLine();
			token.ClearFlag(TokenFlagBit_LeadingSpace);
			return LexNewLine(token);
		}
		case '\\':
		{
			if (*cur_ptr == '\\')
			{
				++cur_ptr;
				return LexComment(token);
			}
			else break;
		}
		case '"':
		{
			return LexString(token);
		}
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
		{
			--cur_ptr;
			return LexNumber(token);
		}
		case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
		case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
		case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
		case 'V': case 'W': case 'X': case 'Y': case 'Z':
		case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
		case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
		case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
		case 'v': case 'w': case 'x': case 'y': case 'z':
		case '_':
		{
			--cur_ptr;
			return LexIdentifier(token);
		}
		case '[': case ']': case '(': case ')': case '{': case '}': case '.':
		case '&': case '*': case '+': case '-': case '~': case '!': case '/':
		case '%': case '<': case '>': case '^': case '|': case '?': case ':':
		case ';': case '=': case ',': case '#':
		{
			--cur_ptr;
			return LexPunctuator(token);
		}
		}

		return false;
	}

	bool Lexer::LexNumber(Token& t)
	{
		FillToken<int32>(t, TokenType::number, [](char c) -> bool { return std::isdigit(c); }, IntegerTransform);
		if (std::isalpha(*cur_ptr)) return false;
		UpdatePointersAndLocation();
		return true;
	}

	bool Lexer::LexIdentifier(Token& t)
	{
		FillToken<std::string>(t, TokenType::identifier, [](char c) -> bool { return std::isalnum(c) || c == '_'; }, StringTransform);
		char const* identifier = t.GetData<std::string>().c_str();
		if (IsKeyword(identifier))
		{
			t.SetType(GetKeywordType(identifier));
		}
		UpdatePointersAndLocation();
		return true;
	}

	bool Lexer::LexString(Token& t)
	{
		FillToken<std::string>(t, TokenType::string_literal, [](char c) -> bool { return c != '"'; }, StringTransform);
		++cur_ptr; //skip the closing "
		UpdatePointersAndLocation();
		return true;
	}

	bool Lexer::LexEndOfFile(Token& t)
	{
		t.SetType(TokenType::eof);
		t.SetLocation(loc);
		return true;
	}

	bool Lexer::LexNewLine(Token& t)
	{
		t.SetType(TokenType::newline);
		t.SetLocation(loc);
		return true;
	}

	bool Lexer::LexComment(Token& t)
	{
		FillToken<std::string>(t, TokenType::comment, [](char c) -> bool { return c != '\n' && c != '\0'; }, StringTransform);
		UpdatePointersAndLocation();
		return true;
	}

	bool Lexer::LexPunctuator(Token& t)
	{
		char c = *cur_ptr++;
		switch (c)
		{
		case '?':
			t.SetType(TokenType::question);
			break;
		case '[':
			t.SetType(TokenType::left_square);
			break;
		case ']':
			t.SetType(TokenType::right_square);
			break;
		case '(':
			t.SetType(TokenType::left_round);
			break;
		case ')':
			t.SetType(TokenType::right_round);
			break;
		case '{':
			t.SetType(TokenType::left_brace);
			break;
		case '}':
			t.SetType(TokenType::right_brace);
			break;
		case '#':
			if (*cur_ptr == '#')
			{
				t.SetType(TokenType::hash_hash);
				++cur_ptr;
			}
			else t.SetType(TokenType::hash);
			break;
		case '.':
			if (cur_ptr[0] == '.' && cur_ptr[1] == '.')
			{
				t.SetType(TokenType::ellipsis);
				cur_ptr += 2;
			}
			else 
			{
				t.SetType(TokenType::period);
			}
		}
		t.SetLocation(loc);
		UpdatePointersAndLocation();
		return true;
	}

}

