#pragma once
#include <string_view>

namespace lu
{
	enum class TokenKind : uint16
	{
		#define TOKEN(X) X,
		#include "TokenDef.h"
		Count
	};
	char const* GetTokenName(TokenKind t);

	bool IsKeyword(std::string_view identifer);
	TokenKind GetKeywordType(std::string_view identifer);

	bool IsPreprocessorKeyword(std::string_view identifer);
	TokenKind GetPreprocessorKeywordType(std::string_view identifer);

}