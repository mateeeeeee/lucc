#pragma once
#include <vector>
#include <memory>
#include "Token.h"

namespace lucc
{
	class ExprAST;
	class StmtAST;
	class ExprStmtAST;
	class IntegerLiteralAST;
	struct AST;

	class Parser
	{
		using TokenPtr = std::vector<Token>::iterator;
	public:

		explicit Parser(std::vector<Token> const& tokens);
		~Parser();
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
		template<typename... Ts>
		bool Consume(TokenKind k, Ts... ts)
		{
			if (current_token->IsOneOf(k, ts...))
			{
				++current_token; return true;
			}
			else return false;
		}
		bool Expect(TokenKind k);

		[[nodiscard]] bool ParseTranslationUnit();

		[[nodiscard]] std::unique_ptr<StmtAST> ParseStatement();
		[[nodiscard]] std::unique_ptr<ExprStmtAST> ParseExpressionStatement();

		[[nodiscard]] std::unique_ptr<ExprAST> ParseExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseAdditiveExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseMultiplicativeExpression();
		[[nodiscard]] std::unique_ptr<IntegerLiteralAST> ParseIntegerLiteral();
	};
}