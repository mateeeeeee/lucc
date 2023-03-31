#pragma once

namespace lumen
{
	enum class TokenType : uint16
	{
		#define TOKEN(X) X,
		#include "TokenDef.h"
		Count
	};
	char const* GetTokenName(TokenType t);

	bool IsKeyword(char const* identifer);
	TokenType GetKeywordType(char const* identifer);

	bool IsPreprocessorKeyword(char const* identifer);
	TokenType GetPreprocessorKeywordType(char const* identifer);

}