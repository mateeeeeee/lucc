#pragma once
#include <vector>
#include <memory>
#include "Token.h"
#include "Diagnostics.h"

namespace lucc
{
	namespace diag
	{
		enum class Code : uint16;
	}
	enum class BinaryExprKind : uint8;
	class SymbolTable;

	struct AST;
	class DeclAST;
	class ExprAST;
	class StmtAST;
	class ExprStmtAST;
	class CompoundStmtAST;
	class IfStmtAST;
	class IntegerLiteralAST;
	class Parser;

	using ExprParseFn = std::unique_ptr<ExprAST>(Parser::*)();

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

		std::unique_ptr<SymbolTable> globals_symtable;
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
		template<typename... Ts>
		bool Expect(TokenKind k, Ts... ts)
		{
			if (!Consume(k, ts...))
			{
				Report(diag::unexpected_token);
				return false;
			}
			return true;
		}
		void Report(diag::Code);

		[[nodiscard]] bool ParseTranslationUnit();

		[[nodiscard]] std::unique_ptr<DeclAST> ParseDeclaration();

		[[nodiscard]] std::unique_ptr<StmtAST> ParseStatement();
		[[nodiscard]] std::unique_ptr<ExprStmtAST> ParseExpressionStatement();
		[[nodiscard]] std::unique_ptr<CompoundStmtAST> ParseCompoundStatement();
		[[nodiscard]] std::unique_ptr<IfStmtAST> ParseIfStatement();

		[[nodiscard]] std::unique_ptr<ExprAST> ParseExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseAssignExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseConditionalExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseLogicalOrExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseLogicalAndExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseInclusiveOrExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseExclusiveOrExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseAndExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseEqualityExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseRelationalExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseShiftExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseAdditiveExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseMultiplicativeExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseCastExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseUnaryExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParsePostFixExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseSizeofExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseAlignofExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParsePrimaryExpression();


		[[nodiscard]] std::unique_ptr<IntegerLiteralAST> ParseIntegerLiteral();

		template<ExprParseFn ParseFn, TokenKind token_kind, BinaryExprKind op_kind>
		std::unique_ptr<ExprAST> ParseBinaryExpression();
	};
	

}