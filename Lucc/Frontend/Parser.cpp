#include "Parser.h"
#include "AST.h"
#include "Diagnostics.h"

namespace lucc
{
	namespace
	{
		constexpr BinaryExprKind TokenKindToBinaryExprType(TokenKind t)
		{
			switch (t)
			{
			case TokenKind::plus: return BinaryExprKind::Add;
			case TokenKind::minus: return BinaryExprKind::Subtract;
			case TokenKind::star: return BinaryExprKind::Multiply;
			case TokenKind::slash: return BinaryExprKind::Divide;
			}
			return BinaryExprKind::Invalid;
		};
	}

	Parser::Parser(std::vector<Token> const& _tokens)
		: tokens(_tokens), current_token(tokens.begin()) {}
	Parser::~Parser() = default;
	bool Parser::Parse()
	{
		ast = std::make_unique<AST>();
		return ParseTranslationUnit();
	}
	bool Parser::Expect(TokenKind k)
	{
		if (!Consume(k))
		{
			Report(diag::unexpected_token, current_token->GetLocation());
			return false;
		}
		return true;
	}

	/*#todo
	real :   <translation-unit>  ::= {<external - declaration>}*
	current: <translation-unit>  ::= {<expression>}* 
	*/
	bool Parser::ParseTranslationUnit()
	{
		while (current_token->IsNot(TokenKind::eof))
		{
			std::unique_ptr<StmtAST> stmt = ParseStatement();
			if (!stmt) return false;
			ast->translation_unit->AddStatement(std::move(stmt));
		}
		return true;
	}


	std::unique_ptr<StmtAST> Parser::ParseStatement()
	{
		return ParseExpressionStatement();
	}

	std::unique_ptr<ExprStmtAST> Parser::ParseExpressionStatement()
	{
		if (Consume(TokenKind::semicolon)) return std::make_unique<NullStmtAST>();
		std::unique_ptr<ExprAST> expression = ParseExpression();
		if (!expression || !Expect(TokenKind::semicolon)) return nullptr;
		return std::make_unique<ExprStmtAST>(std::move(expression));
	}

	std::unique_ptr<ExprAST> Parser::ParseExpression()
	{
		return ParseAdditiveExpression();
	}

	std::unique_ptr<ExprAST> Parser::ParseAdditiveExpression()
	{
		std::unique_ptr<ExprAST> lhs = ParseMultiplicativeExpression();
		if (!lhs) return nullptr;

		if (current_token->IsOneOf(TokenKind::eof, TokenKind::semicolon)) return lhs;
		BinaryExprKind type = TokenKindToBinaryExprType(current_token->GetKind());
		if (type == BinaryExprKind::Invalid) return nullptr;

		while (true)
		{
			++current_token;
			std::unique_ptr<ExprAST> rhs = ParseMultiplicativeExpression();
			std::unique_ptr<BinaryExprAST> parent = std::make_unique<BinaryExprAST>(type);
			parent->SetLHS(std::move(lhs));
			parent->SetRHS(std::move(rhs));
			lhs = std::move(parent);

			if (current_token->IsOneOf(TokenKind::eof, TokenKind::semicolon)) break;
			type = TokenKindToBinaryExprType(current_token->GetKind());
			if (type == BinaryExprKind::Invalid) return nullptr;
		}
		return lhs;
	}

	std::unique_ptr<ExprAST> Parser::ParseMultiplicativeExpression()
	{
		std::unique_ptr<ExprAST> lhs = ParseIntegerLiteral();
		if (!lhs) return nullptr;

		if (current_token->IsOneOf(TokenKind::eof, TokenKind::semicolon)) return lhs;

		while (current_token->Is(TokenKind::star) || current_token->Is(TokenKind::slash)) 
		{
			BinaryExprKind type = TokenKindToBinaryExprType(current_token->GetKind());
			std::unique_ptr<BinaryExprAST> parent = std::make_unique<BinaryExprAST>(type);

			++current_token;
			std::unique_ptr<ExprAST> rhs = ParseIntegerLiteral();

			parent->SetLHS(std::move(lhs));
			parent->SetRHS(std::move(rhs));
			lhs = std::move(parent);
			if (current_token->IsOneOf(TokenKind::eof, TokenKind::semicolon)) return lhs;
		}
		return lhs;
	}

	std::unique_ptr<IntegerLiteralAST> Parser::ParseIntegerLiteral()
	{
		if (current_token->IsNot(TokenKind::number))
		{
			return nullptr;
		}
		std::string_view string_number = current_token->GetIdentifier();
		int64 value = std::stoll(current_token->GetIdentifier().data(), nullptr, 0);
		++current_token;
		return std::make_unique<IntegerLiteralAST>(value);
	}


}