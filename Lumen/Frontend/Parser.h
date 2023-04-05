#pragma once
#include <vector>
#include <memory>
#include "Token.h"


namespace lu
{
	struct AST;
	class DeclAST;

	class Parser
	{
		using TokenPtr = std::vector<Token>::iterator;
	public:

		explicit Parser(std::vector<Token> const& tokens);
		bool Parse();
		AST* GetAST() const { return ast.get(); }

	private:
		std::vector<Token> tokens;
		TokenPtr current_token;

		std::unique_ptr<AST> ast;
	private:
		bool Consume(TokenKind k)
		{
			if (current_token->Is(k))
			{
				++current_token; return true;
			}
			else return false;
		}

		bool ParseTranslationUnit();
		std::unique_ptr<DeclAST> ParseExternalDeclaration();
	};
}