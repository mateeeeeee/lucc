#include "Parser.h"
#include "Lexer.h"
#include "AST.h"

namespace lu
{

	Parser::Parser(std::vector<Token> const& _tokens)
		: tokens(_tokens), current_token(tokens.begin())
	{}

	bool Parser::Parse()
	{
		ast = std::make_unique<AST>();
		return ParseTranslationUnit();
	}

	//<translation-unit> ::= { <external-declaration> }*
	bool Parser::ParseTranslationUnit()
	{
		while (current_token->IsNot(TokenKind::eof))
		{
			std::unique_ptr<DeclAST> ext_decl = ParseExternalDeclaration();
			if (!ext_decl) return false;
			ast->tr_unit->AddExternalDeclaration(std::move(ext_decl));
		}
		return true;
	}

	std::unique_ptr<DeclAST> Parser::ParseExternalDeclaration()
	{
		return nullptr;
	}

}

