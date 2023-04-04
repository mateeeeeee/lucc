#include "Parser.h"
#include "Lexer.h"

namespace lu
{

	Parser::Parser(std::vector<Token> const& _tokens) 
		: tokens(_tokens), current_token(tokens.begin())
	{}

	bool Parser::Parse()
	{
		return ParseTranslationUnit();
	}

	//translation_unit --> : external_declaration | translation_unit external_declaration;
	bool Parser::ParseTranslationUnit()
	{
		while (current_token->IsNot(TokenKind::eof))
		{
			if (!ParseExternalDeclaration()) return false;
		}
		return true;
	}

	//external_declaration : function_definition | declaration;
	bool Parser::ParseExternalDeclaration()
	{
		bool is_typedef = Consume(TokenKind::KW_typedef);

		return true;
	}

	bool Parser::ParseDeclarationSpecifiers()
	{
		return true;
	}

}

