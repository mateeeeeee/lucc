#include "TokenTypes.h"
#include <unordered_set>
#include <unordered_map>

namespace lumen
{
	namespace
	{
		char const* const token_names[] = {
		#define TOKEN(X) #X,
		#include "TokenDef.h"
		"Count"
		};

		std::unordered_set<std::string_view> const keywords =
		{
			#define KEYWORD(X) #X,
			#include "TokenDef.h"
		};
		std::unordered_map<std::string_view, TokenType> keywords_map =
		{
			#define KEYWORD(X) {#X, TokenType::##KW_##X},
			#include "TokenDef.h"
		};
		std::unordered_set<std::string_view> const pp_keywords =
		{
			#define PP_KEYWORD(X) #X,
			#include "TokenDef.h"
		};

		std::unordered_map<std::string_view, TokenType> pp_keywords_map =
		{
			#define PP_KEYWORD(X) {#X, TokenType::##PP_##X},
			#include "TokenDef.h"
		};
	}

	char const* GetTokenName(TokenType t)
	{
		return token_names[(size_t)t];
	}

	bool IsKeyword(std::string_view identifer)
	{
		return keywords.contains(identifer);
	}
	TokenType GetKeywordType(std::string_view identifer)
	{
		return keywords_map[std::string(identifer)];
	}

	bool IsPreprocessorKeyword(std::string_view identifer)
	{
		return pp_keywords.contains(identifer);
	}
	TokenType GetPreprocessorKeywordType(std::string_view identifer)
	{
		return pp_keywords_map[std::string(identifer)];
	}

}

