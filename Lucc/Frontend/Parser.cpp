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

	template<ExprParseFn ParseFn, TokenKind token_kind, BinaryExprKind op_kind>
	std::unique_ptr<ExprAST> Parser::ParseBinaryExpression()
	{
		std::unique_ptr<ExprAST> lhs = (this->*ParseFn)();
		while (Consume(token_kind)) 
		{
			std::unique_ptr<ExprAST> rhs = (this->*ParseFn)();
			std::unique_ptr<BinaryExprAST> parent = std::make_unique<BinaryExprAST>(op_kind);
			parent->SetLHS(std::move(lhs));
			parent->SetRHS(std::move(rhs));
			lhs = std::move(parent);
		}
		return lhs;
	}

	//<expression> ::=  <assignment - expression>
	//					| <expression>, <assignment - expression>
	std::unique_ptr<ExprAST> Parser::ParseExpression()
	{
		return ParseBinaryExpression<&Parser::ParseAssignExpression, TokenKind::comma, BinaryExprKind::Comma>();
	}

	//<assignment - expression> ::= <conditional - expression>
	//								| <unary - expression> <assignment - operator> <assignment - expression>
	std::unique_ptr<ExprAST> Parser::ParseAssignExpression()
	{
		std::unique_ptr<ExprAST>  lhs = ParseConditionalExpression();
		BinaryExprKind arith_op_kind = BinaryExprKind::Assign;
		switch (current_token->GetKind()) 
		{
		case TokenKind::equal: break;
		case TokenKind::star_equal: arith_op_kind = BinaryExprKind::Multiply; break;
		case TokenKind::slash_equal: arith_op_kind = BinaryExprKind::Divide; break;
		case TokenKind::modulo_equal: arith_op_kind = BinaryExprKind::Modulo; break;
		case TokenKind::plus_equal: arith_op_kind = BinaryExprKind::Add; break;
		case TokenKind::minus_equal: arith_op_kind = BinaryExprKind::Subtract; break;
		case TokenKind::less_less_equal: arith_op_kind = BinaryExprKind::ShiftLeft; break;
		case TokenKind::greater_great_equal: arith_op_kind = BinaryExprKind::ShiftRight; break;
		case TokenKind::amp_equal: arith_op_kind = BinaryExprKind::BitAnd; break;
		case TokenKind::pipe_equal: arith_op_kind = BinaryExprKind::BitOr; break;
		case TokenKind::caret_equal: arith_op_kind = BinaryExprKind::BitXor; break;
		default:
			return lhs;
		}
		++current_token;
		std::unique_ptr<ExprAST> rhs = ParseAssignExpression();
		if (arith_op_kind != BinaryExprKind::Assign)
		{
			std::unique_ptr<BinaryExprAST> parent = std::make_unique<BinaryExprAST>(arith_op_kind);
			parent->SetLHS(std::move(lhs));
			parent->SetRHS(std::move(rhs));
			rhs = std::move(parent);
		}

		std::unique_ptr<BinaryExprAST> parent = std::make_unique<BinaryExprAST>(BinaryExprKind::Assign);
		parent->SetLHS(std::move(lhs));
		parent->SetRHS(std::move(rhs));
		return parent;
	}

	//<conditional - expression> ::=  <logical - or -expression>
	//								| <logical - or -expression> ? <expression> : <conditional - expression>
	std::unique_ptr<ExprAST> Parser::ParseConditionalExpression()
	{
		std::unique_ptr<ExprAST> cond = ParseLogicalOrExpression();
		if (Consume(TokenKind::question)) 
		{
			std::unique_ptr<ExprAST> true_expr = ParseExpression();
			Expect(TokenKind::colon);
			std::unique_ptr<ExprAST> false_expr = ParseConditionalExpression();
			return std::make_unique<TernaryExprAST>(std::move(cond), std::move(true_expr), std::move(false_expr));
		}
		return cond;
	}

	//<logical - or -expression> :: = <logical - and -expression>
	//								| <logical - or -expression> || <logical - and -expression>
	std::unique_ptr<ExprAST> Parser::ParseLogicalOrExpression()
	{
		return ParseBinaryExpression<&Parser::ParseLogicalAndExpression, TokenKind::pipe_pipe, BinaryExprKind::LogicalOr>();
	}

	//	<logical - and - expression> :: = <inclusive - or - expression>
	//									| <logical - and - expression> && <inclusive - or - expression>
	std::unique_ptr<ExprAST> Parser::ParseLogicalAndExpression()
	{
		return ParseBinaryExpression<&Parser::ParseInclusiveOrExpression, TokenKind::amp_amp, BinaryExprKind::LogicalAnd>();
	}

	//<inclusive - or - expression> :: = <exclusive - or - expression>
	//								  | <inclusive - or - expression> | <exclusive - or - expression>
	std::unique_ptr<ExprAST> Parser::ParseInclusiveOrExpression()
	{
		return ParseBinaryExpression<&Parser::ParseExclusiveOrExpression, TokenKind::pipe, BinaryExprKind::BitOr>();
	}

	//<exclusive - or - expression> ::= <and - expression>
	//								| <exclusive - or - expression> ^ <and - expression>
	std::unique_ptr<ExprAST> Parser::ParseExclusiveOrExpression()
	{
		return ParseBinaryExpression<&Parser::ParseAndExpression, TokenKind::caret, BinaryExprKind::BitXor>();
	}
	
	//<and-expression> :: = <equality - expression>
	//					  | <and - expression> & <equality - expression>
	std::unique_ptr<ExprAST> Parser::ParseAndExpression()
	{
		return ParseBinaryExpression<&Parser::ParseEqualityExpression, TokenKind::amp, BinaryExprKind::BitAnd>();
	}

	//<equality - expression> :: = <relational - expression>
	//							 | <equality - expression> == <relational - expression>
	//							 | <equality - expression> != <relational - expression>
	std::unique_ptr<ExprAST> Parser::ParseEqualityExpression()
	{
		std::unique_ptr<ExprAST> lhs = ParseRelationalExpression();
		while (true)
		{
			BinaryExprKind op_kind = BinaryExprKind::Invalid;
			switch (current_token->GetKind()) 
			{
			case TokenKind::equal:		op_kind = BinaryExprKind::Equal; break;
			case TokenKind::not_equal:	op_kind = BinaryExprKind::NotEqual; break;
			default:
				return lhs;
			}
			++current_token;
			std::unique_ptr<ExprAST> rhs = ParseRelationalExpression();
			std::unique_ptr<BinaryExprAST> parent = std::make_unique<BinaryExprAST>(op_kind);
			parent->SetLHS(std::move(lhs));
			parent->SetRHS(std::move(rhs));
			lhs = std::move(parent);
		}
	}

	//<relational - expression> :: = <shift - expression>
	//							   | <relational - expression> < <shift - expression>
	//							   | <relational - expression> > <shift - expression>
	//							   | <relational - expression> <= <shift - expression>
	//							   | <relational - expression> >= <shift - expression>
	std::unique_ptr<ExprAST> Parser::ParseRelationalExpression()
	{
		std::unique_ptr<ExprAST> lhs = ParseShiftExpression();
		while (true) 
		{
			BinaryExprKind op_kind = BinaryExprKind::Invalid;
			switch (current_token->GetKind())
			{
			case TokenKind::less:			op_kind = BinaryExprKind::Less; break;
			case TokenKind::less_equal:		op_kind = BinaryExprKind::LessEqual; break;
			case TokenKind::greater:		op_kind = BinaryExprKind::Greater; break;
			case TokenKind::greater_equal:	op_kind = BinaryExprKind::GreaterEqual; break;
			default:
				return lhs;
			}
			++current_token;
			std::unique_ptr<ExprAST> rhs = ParseShiftExpression();
			std::unique_ptr<BinaryExprAST> parent = std::make_unique<BinaryExprAST>(op_kind);
			parent->SetLHS(std::move(lhs));
			parent->SetRHS(std::move(rhs));
			lhs = std::move(parent);
		}
	}

	//<shift - expression> :: = <additive - expression>
	//						| <shift - expression> << <additive - expression>
	//						| <shift - expression> >> <additive - expression>
	std::unique_ptr<ExprAST> Parser::ParseShiftExpression()
	{
		std::unique_ptr<ExprAST> lhs = ParseAdditiveExpression();
		while (true)
		{
			BinaryExprKind op_kind = BinaryExprKind::Invalid;
			switch (current_token->GetKind())
			{
			case TokenKind::less_less:			op_kind = BinaryExprKind::ShiftLeft; break;
			case TokenKind::greater_greater:	op_kind = BinaryExprKind::ShiftRight; break;
			default:
				return lhs;
			}
			++current_token;
			std::unique_ptr<ExprAST> rhs = ParseAdditiveExpression();
			std::unique_ptr<BinaryExprAST> parent = std::make_unique<BinaryExprAST>(op_kind);
			parent->SetLHS(std::move(lhs));
			parent->SetRHS(std::move(rhs));
			lhs = std::move(parent);
		}
	}

	//<additive - expression> :: = <multiplicative - expression>
	//							| <additive - expression> +<multiplicative - expression>
	//							| <additive - expression> -<multiplicative - expression>
	std::unique_ptr<ExprAST> Parser::ParseAdditiveExpression()
	{
		std::unique_ptr<ExprAST> lhs = ParseMultiplicativeExpression();
		while (true) 
		{
			BinaryExprKind op_kind = BinaryExprKind::Invalid;
			switch (current_token->GetKind())
			{
			case TokenKind::plus:			op_kind = BinaryExprKind::Add; break;
			case TokenKind::minus:			op_kind = BinaryExprKind::Subtract; break;
			default:
				return lhs;
			}
			++current_token;
			std::unique_ptr<ExprAST> rhs = ParseMultiplicativeExpression();
			std::unique_ptr<BinaryExprAST> parent = std::make_unique<BinaryExprAST>(op_kind);
			parent->SetLHS(std::move(lhs));
			parent->SetRHS(std::move(rhs));
			lhs = std::move(parent);
		}
	}

	//<multiplicative - expression>   ::= <cast - expression>
	//									| <multiplicative - expression> *<cast - expression>
	//									| <multiplicative - expression> / <cast - expression>
	//									| <multiplicative - expression> % <cast - expression>
	std::unique_ptr<ExprAST> Parser::ParseMultiplicativeExpression()
	{
		std::unique_ptr<ExprAST> lhs = ParseCastExpression();
		while (true) 
		{
			BinaryExprKind op_kind = BinaryExprKind::Invalid;
			switch (current_token->GetKind())
			{
			case TokenKind::star:	op_kind = BinaryExprKind::Multiply; break;
			case TokenKind::slash:	op_kind = BinaryExprKind::Divide; break;
			case TokenKind::modulo: op_kind = BinaryExprKind::Modulo; break;
			default:
				return lhs;
			}
			++current_token;
			std::unique_ptr<ExprAST> rhs = ParseCastExpression();
			std::unique_ptr<BinaryExprAST> parent = std::make_unique<BinaryExprAST>(op_kind);
			parent->SetLHS(std::move(lhs));
			parent->SetRHS(std::move(rhs));
			lhs = std::move(parent);
		}
	}

	//<cast - expression> :: = <unary - expression>
	//					   | (<type - name>) < cast - expression >
	std::unique_ptr<ExprAST> Parser::ParseCastExpression()
	{
		if (current_token->Is(TokenKind::left_round) && (current_token + 1)->IsDeclSpec())
		{
			//#todo
			return nullptr;
		}
		else return ParseUnaryExpression();
	}


	//<unary - expression> :: = <postfix - expression>
	//						| ++ <unary - expression>
	//						| -- <unary - expression>
	//						| <unary - operator> <cast - expression>
	//						| sizeof <unary - expression>
	//						| sizeof <type - name>

	std::unique_ptr<ExprAST> Parser::ParseUnaryExpression()
	{
		std::unique_ptr<UnaryExprAST> unary_expr;
		switch (current_token->GetKind()) 
		{
		case TokenKind::plus_plus:
			unary_expr = std::make_unique<UnaryExprAST>(UnaryExprKind::PreIncrement);
			break;
		case TokenKind::minus_minus: 
			unary_expr = std::make_unique<UnaryExprAST>(UnaryExprKind::PreDecrement);
			break;
		case TokenKind::amp: 
			unary_expr = std::make_unique<UnaryExprAST>(UnaryExprKind::AddressOf);
			break;
		case TokenKind::star: 
			unary_expr = std::make_unique<UnaryExprAST>(UnaryExprKind::Dereference);
			break;
		case TokenKind::plus:
			unary_expr = std::make_unique<UnaryExprAST>(UnaryExprKind::Plus);
			break;
		case TokenKind::minus:
			unary_expr = std::make_unique<UnaryExprAST>(UnaryExprKind::Minus);
			break;
		case TokenKind::tilde:
			unary_expr = std::make_unique<UnaryExprAST>(UnaryExprKind::BitNot);
			break;
		case TokenKind::exclaim:
			unary_expr = std::make_unique<UnaryExprAST>(UnaryExprKind::LogicalNot);
			break;
		case TokenKind::KW_sizeof: return ParseSizeofExpression();
		case TokenKind::KW__Alignas: return ParseAlignofExpression();
		default:
			return ParsePostFixExpression();
		}
		++current_token;
		unary_expr->SetOperand(ParseUnaryExpression());
		return unary_expr;
	}

	//<postfix - expression> :: = <primary - expression>
	//							| <postfix - expression>[<expression>]
	//							| <postfix - expression> ({ <assignment - expression> }*)
	//							| <postfix - expression> . <identifier>
	//							| <postfix - expression> -> <identifier>
	//							| <postfix - expression> ++
	//							| <postfix - expression> --
	std::unique_ptr<ExprAST> Parser::ParsePostFixExpression()
	{
		//std::unique_ptr<ExprAST> expr;
		//if (current_token->Is(TokenKind::left_round) && (current_token + 1)->IsDeclSpec())
		//{
		//	//expr = ParseCompoundLiteral();
		//}
		//else expr = ParsePrimaryExpression();
		//return ParsePostfixExprTail(expr);
		return ParsePrimaryExpression();
	}

	std::unique_ptr<ExprAST> Parser::ParseSizeofExpression()
	{
		//#todo
		return nullptr;
	}

	std::unique_ptr<ExprAST> Parser::ParseAlignofExpression()
	{
		//#todo
		return nullptr;
	}

	//<primary - expression> :: = <identifier>
	//							| <constant>
	//							| <string>
	//							| (<expression>)
	std::unique_ptr<ExprAST> Parser::ParsePrimaryExpression()
	{
		std::unique_ptr<ExprAST> expr = ParseIntegerLiteral();
		//switch (current_token->GetKind())
		//{
		//case TokenKind::left_round: return ParseParenExpression();
		//case TokenKind::identifier: expr = ParseIdentifier(); break;
		//case TokenKind::number: expr = ParseNumber(); break;
		//case TokenKind::string_literal: expr = ParseStrLiterals(); break;
		//default:
		//	Report(diag::unexpected_token);
		//}
		//++current_token;
		return expr;
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