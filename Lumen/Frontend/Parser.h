#pragma once
#include <vector>
#include "Token.h"

namespace lu
{

	class Parser
	{
		using TokenPtr = std::vector<Token>::iterator;
	public:
		explicit Parser(std::vector<Token> const& tokens);

		bool Parse();

	private:
		std::vector<Token> tokens;
		TokenPtr current_token;

	private:
		bool Consume(TokenKind t)
		{
			if (current_token->Is(t))
			{
				++current_token; return true;
			}
			else return false;
		}

		bool ParseTranslationUnit();
		bool ParseExternalDeclaration();
		bool ParseDeclarationSpecifiers();
	};
}