#include "Parser.h"
#include "AST.h"
#include "Symbol.h"


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
			case TokenKind::equal: return BinaryExprKind::Assign;
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
		globals_symtable = std::make_unique<SymbolTable>(Scope_Global);
		return ParseTranslationUnit();
	}
	bool Parser::Expect(TokenKind k)
	{
		if (!Consume(k))
		{
			Report(diag::unexpected_token);
			return false;
		}
		return true;
	}
	void Parser::Report(diag::Code code)
	{
		--current_token;
		diag::Report(code, current_token->GetLocation());
		++current_token;
	}

	bool Parser::ParseTranslationUnit()
	{
		while (current_token->IsNot(TokenKind::eof))
		{
			std::unique_ptr<DeclAST> decl = ParseDeclaration();
			if (!decl) return false;
			ast->translation_unit->AddDeclarations(std::move(decl));
		}
		return true;
	}

	std::unique_ptr<DeclAST> Parser::ParseDeclaration()
	{
		while (Consume(TokenKind::semicolon)) Report(diag::empty_statement);
		while (current_token->IsDeclSpec()) ++current_token;

		//#todo parse declaration specifier

		if (current_token->IsNot(TokenKind::identifier)) Report(diag::missing_name);
		std::string_view name = current_token->GetIdentifier();
		++current_token;

		//function declaration
		if (Consume(TokenKind::left_round))
		{
			std::unique_ptr<FunctionDeclAST> func = std::make_unique<FunctionDeclAST>(name);
			Expect(TokenKind::right_round);
			if (current_token->Is(TokenKind::left_brace))
			{
				std::unique_ptr<CompoundStmtAST> body = ParseCompoundStatement();
				func->SetFunctionBody(std::move(body));
			}
			return func;
		}
		else
		{
			std::unique_ptr<VarDeclAST> var_decl = std::make_unique<VarDeclAST>(name);
			if (Consume(TokenKind::equal))
			{
				std::unique_ptr<ExprAST> init_expr = ParseExpression();
				var_decl->SetInitExpression(std::move(init_expr));
			}
			if (Consume(TokenKind::semicolon)) return var_decl;
		}

		return nullptr;
	}

	std::unique_ptr<StmtAST> Parser::ParseStatement()
	{
		switch (current_token->GetKind())
		{
		case TokenKind::left_brace: return ParseCompoundStatement();
		case TokenKind::KW_if: return ParseIfStatement();
		//case TokenKind::KW_while: return ParseWhileStatement();
		//case TokenKind::KW_do: return ParseDoWhileStatement();
		//case TokenKind::KW_for: return ParseForStmt();
		//case TokenKind::KW_switch: return ParseSwitchStmt();
		//case TokenKind::KW_continue: return ParseContinueStmt();
		//case TokenKind::KW_break: return ParseBreakStmt();
		//case TokenKind::KW_return: return ParseReturnStatement();
		//case TokenKind::KW_case: return ParseCaseStmt();
		//case TokenKind::KW_default: return ParseCaseStmt();
		default:
			--current_token;
			return ParseExpressionStatement();
		}
		return nullptr;
	}

	std::unique_ptr<ExprStmtAST> Parser::ParseExpressionStatement()
	{
		if (Consume(TokenKind::semicolon)) return std::make_unique<NullStmtAST>();
		std::unique_ptr<ExprAST> expression = ParseExpression();
		if (!expression || !Expect(TokenKind::semicolon)) return nullptr;
		return std::make_unique<ExprStmtAST>(std::move(expression));
	}

	std::unique_ptr<CompoundStmtAST> Parser::ParseCompoundStatement()
	{
		Expect(TokenKind::left_brace);
		std::unique_ptr<CompoundStmtAST> compound_stmt = std::make_unique<CompoundStmtAST>();
		while (current_token->IsNot(TokenKind::right_brace))
		{
			if (current_token->IsDeclSpec())
			{
				std::unique_ptr<DeclAST> decl = ParseDeclaration();
				compound_stmt->AddStatement(std::make_unique<DeclStmtAST>(std::move(decl)));
			}
			else
			{
				std::unique_ptr<StmtAST> stmt = ParseStatement();
				compound_stmt->AddStatement(std::move(stmt));
			}
		}
		Expect(TokenKind::right_brace);
		return compound_stmt;
	}

	std::unique_ptr<IfStmtAST> Parser::ParseIfStatement()
	{
		Expect(TokenKind::KW_if);
		if (!Consume(TokenKind::left_round))
		{
			Report(diag::if_condition_not_in_parentheses);
			return nullptr;
		}
		std::unique_ptr<ExprAST> condition = ParseExpression();
		if (condition == nullptr) return nullptr;
		if (!Consume(TokenKind::right_round))
		{
			Report(diag::if_condition_not_in_parentheses);
			return nullptr;
		}

		std::unique_ptr<IfStmtAST> if_stmt;
		std::unique_ptr<StmtAST> then_stmt = ParseStatement();
		if_stmt = std::make_unique<IfStmtAST>(std::move(condition), std::move(then_stmt));
		if (Consume(TokenKind::KW_else))
		{
			std::unique_ptr<StmtAST> else_stmt = ParseStatement();
			if_stmt->AddElseStatement(std::move(else_stmt));
		}

		return if_stmt;
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
		if (current_token->IsNot(TokenKind::number)) return nullptr;
		std::string_view string_number = current_token->GetIdentifier();
		int64 value = std::stoll(current_token->GetIdentifier().data(), nullptr, 0);
		++current_token;
		return std::make_unique<IntegerLiteralAST>(value);
	}


}