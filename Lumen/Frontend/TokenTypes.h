#pragma once
#include <string_view>

namespace lu
{
	enum class TokenType : uint16
	{
		#define TOKEN(X) X,
		#include "TokenDef.h"
		Count
	};
	char const* GetTokenName(TokenType t);

	bool IsKeyword(std::string_view identifer);
	TokenType GetKeywordType(std::string_view identifer);

	bool IsPreprocessorKeyword(std::string_view identifer);
	TokenType GetPreprocessorKeywordType(std::string_view identifer);

}