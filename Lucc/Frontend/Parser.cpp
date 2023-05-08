#include "Parser.h"
#include "AST.h"
#include "Symbol.h"
#include "Core/Defines.h"


namespace lucc
{
	namespace builtin_types
	{
		static constexpr Type Void = VoidType();
		static constexpr ArithmeticType Bool = ArithmeticType(ArithmeticType::Bool);
		static constexpr ArithmeticType Char = ArithmeticType(ArithmeticType::Char);
		static constexpr ArithmeticType UnsignedChar = ArithmeticType(ArithmeticType::Char, true);
		static constexpr ArithmeticType Short = ArithmeticType(ArithmeticType::Short);
		static constexpr ArithmeticType UnsignedShort = ArithmeticType(ArithmeticType::Short, true);
		static constexpr ArithmeticType Int = ArithmeticType(ArithmeticType::Int);
		static constexpr ArithmeticType UnsignedInt = ArithmeticType(ArithmeticType::Int, true);
		static constexpr ArithmeticType Long = ArithmeticType(ArithmeticType::Long);
		static constexpr ArithmeticType UnsignedLong = ArithmeticType(ArithmeticType::Long, true);
		static constexpr ArithmeticType LongLong = ArithmeticType(ArithmeticType::LongLong);
		static constexpr ArithmeticType UnsignedLongLong = ArithmeticType(ArithmeticType::LongLong, true);
		static constexpr ArithmeticType Float = ArithmeticType(ArithmeticType::Float);
		static constexpr ArithmeticType Double = ArithmeticType(ArithmeticType::Double);
		static constexpr ArithmeticType LongDouble = ArithmeticType(ArithmeticType::Double | ArithmeticType::Long);
	}
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

	struct Parser::DeclSpecInfo
	{
		size_t align = 0;
		QualifiedType qtype = builtin_types::Int;
		Storage storage = Storage::None;
		FunctionSpecifier func_spec = FunctionSpecifier::None;
	};
	struct Parser::DeclaratorInfo
	{
		std::string name = "";
		QualifiedType qtype{};
		SourceLocation loc;
	};
	struct Parser::DeclarationInfo
	{
		DeclarationInfo() {}
		DeclarationInfo(DeclSpecInfo const& decl_spec, DeclaratorInfo const& declarator)
			: name(declarator.name), qtype(declarator.qtype), loc(declarator.loc),
			align(decl_spec.align), storage(decl_spec.storage), func_spec(decl_spec.func_spec)
		{}
		std::string name = "";
		QualifiedType qtype{};
		SourceLocation loc;
		size_t align = 0;
		Storage storage = Storage::None;
		FunctionSpecifier func_spec = FunctionSpecifier::None;
	};

	Parser::Parser(std::vector<Token> const& _tokens)
		: tokens(_tokens), current_token(tokens.begin()) {}
	Parser::~Parser() = default;
	bool Parser::Parse()
	{
		ast = std::make_unique<AST>();
		ctx.identifier_sym_table = std::make_unique<SymbolTable>();
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
			auto decls = ParseDeclaration();
			for(auto&& decl : decls) ast->translation_unit->AddDeclarations(std::move(decl));
		}
		return true;
	}

	std::vector<std::unique_ptr<DeclAST>> Parser::ParseDeclaration()
	{
		while (Consume(TokenKind::semicolon)) Report(diag::empty_statement);

		std::vector<std::unique_ptr<DeclAST>> decls;

		DeclSpecInfo decl_spec{};
		if (!ParseDeclSpec(decl_spec))
		{
			Report(diag::declarator_specifier_error);
			return decls;
		}

		if (decl_spec.storage == Storage::Typedef)
		{
			auto typedef_decls = ParseTypedefDeclaration(decl_spec);
			for (auto&& typedef_decl : typedef_decls) decls.push_back(std::move(typedef_decl));
			return decls;
		}

		do 
		{
			DeclaratorInfo declarator_info{};
			ParseDeclarator(decl_spec, declarator_info);
			LU_ASSERT(declarator_info.qtype.HasRawType());
			DeclarationInfo declaration_info(decl_spec, declarator_info);

			if (declarator_info.qtype->Is(PrimitiveTypeKind::Function))
			{
				std::unique_ptr<FunctionDeclAST> func = ParseFunctionDeclaration(declaration_info);
				if (func->IsDefinition())
				{
					LU_ASSERT(decls.empty());
					
					decls.push_back(std::move(func));
					return decls;
				}
				decls.push_back(std::move(func));
			}
			else
			{
				std::string_view name = declarator_info.name;
				std::unique_ptr<VarDeclAST> var_decl = std::make_unique<VarDeclAST>(name);
				if (Consume(TokenKind::equal))
				{
					std::unique_ptr<ExprAST> init_expr = ParseExpression();
					var_decl->SetInitExpression(std::move(init_expr));
				}
				decls.push_back(std::move(var_decl));

				ctx.identifier_sym_table->Insert(declaration_info.name, declaration_info.qtype, declaration_info.storage);
			}
		} while (Consume(TokenKind::comma));
		Expect(TokenKind::semicolon);
		return decls;
	}

	std::vector<std::unique_ptr<TypedefDeclAST>> Parser::ParseTypedefDeclaration(DeclSpecInfo const& decl_spec)
	{
		bool first = true;
		std::vector<std::unique_ptr<TypedefDeclAST>> typedefs{};
		while (!Consume(TokenKind::semicolon))
		{
			if (!first) Expect(TokenKind::comma);
			first = false;

			DeclaratorInfo typedef_info{};
			ParseDeclarator(decl_spec, typedef_info);

			if (typedef_info.name.empty())
			{
				Report(diag::typedef_name_empty);
				return {};
			}
			typedefs.push_back(std::make_unique<TypedefDeclAST>(typedef_info.name));
			ctx.identifier_sym_table->Insert(typedef_info.name, typedef_info.qtype, Storage::Typedef);
		}
		return typedefs;
	}

	std::unique_ptr<FunctionDeclAST> Parser::ParseFunctionDeclaration(DeclarationInfo const& decl_info)
	{
		LU_ASSERT(ctx.identifier_sym_table->IsGlobal());
		ctx.identifier_sym_table->EnterScope();

		std::string_view func_name = decl_info.name;
		if (func_name.empty())
		{
			Report(diag::missing_name);
			return nullptr;
		}

		FuncType const& func_type = TypeCast<FuncType const&>(decl_info.qtype);
		std::unique_ptr<FunctionDeclAST> func_decl = std::make_unique<FunctionDeclAST>(func_name);
		for (auto&& func_param : func_type.GetParamTypes())
		{
			std::unique_ptr<VarDeclAST> param_decl = std::make_unique<VarDeclAST>(func_param.name);
			func_decl->AddParamDeclaration(std::move(param_decl));
		}
		func_type.EncounterPrototype();
		ctx.identifier_sym_table->Insert(std::string(func_name), decl_info.qtype, decl_info.storage);
		if (current_token->Is(TokenKind::left_brace))
		{
			ctx.identifier_sym_table->EnterScope();
			ctx.current_func_type = &func_type;
			func_type.EncounteredDefinition();

			std::unique_ptr<CompoundStmtAST> compound_stmt = ParseCompoundStatement();
			func_decl->SetFunctionBody(std::move(compound_stmt));
			ctx.current_func_type = nullptr;
			ctx.identifier_sym_table->ExitScope();
		}
		ctx.identifier_sym_table->ExitScope();
		return func_decl;
	}

	std::unique_ptr<StmtAST> Parser::ParseStatement()
	{
		switch (current_token->GetKind())
		{
		case TokenKind::left_brace: return ParseCompoundStatement();
		case TokenKind::KW_if: return ParseIfStatement();
		case TokenKind::KW_while: return ParseWhileStatement();
		case TokenKind::KW_for: return ParseForStatement();
		//case TokenKind::KW_do: return ParseDoWhileStatement();
		//case TokenKind::KW_switch: return ParseSwitchStmt();
		//case TokenKind::KW_continue: return ParseContinueStmt();
		//case TokenKind::KW_break: return ParseBreakStmt();
		case TokenKind::KW_return: return ParseReturnStatement();
		//case TokenKind::KW_case: return ParseCaseStmt();
		//case TokenKind::KW_default: return ParseCaseStmt();
		default:
			return ParseExpressionStatement();
		}
		return nullptr;
	}

	std::unique_ptr<ExprStmtAST> Parser::ParseExpressionStatement()
	{
		if (Consume(TokenKind::semicolon)) return std::make_unique<NullStmtAST>();
		std::unique_ptr<ExprAST> expression = ParseExpression();
		Expect(TokenKind::semicolon);
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
				std::vector<std::unique_ptr<DeclAST>> decl = ParseDeclaration();
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
		std::unique_ptr<ExprAST> condition = ParseParenthesizedExpression();

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

	//<while - statement> ::= while (<expression>) < statement >
	std::unique_ptr<WhileStmtAST> Parser::ParseWhileStatement()
	{
		Expect(TokenKind::KW_while);
		std::unique_ptr<ExprAST> condition = ParseParenthesizedExpression();
		std::unique_ptr<StmtAST> body = ParseStatement();
		return std::make_unique<WhileStmtAST>(std::move(condition), std::move(body));
	}

	//<for - statement> ::= for ( {<init>}? ; {<expression>}? ; {<expression>}? ) <statement>
	std::unique_ptr<ForStmtAST> Parser::ParseForStatement()
	{
		Expect(TokenKind::KW_for);
		Expect(TokenKind::left_round);

		std::unique_ptr<StmtAST> init = nullptr;
		if (current_token->IsDeclSpec())
		{
			std::vector<std::unique_ptr<DeclAST>> decl = ParseDeclaration();
			init = std::make_unique<DeclStmtAST>(std::move(decl));
		}
		else init = ParseExpressionStatement();

		std::unique_ptr<ExprStmtAST> cond_expr_stmt = ParseExpressionStatement();
		std::unique_ptr<ExprAST> cond_expr = *cond_expr_stmt;

		std::unique_ptr<ExprAST> iter_expr = nullptr;
		if (!Consume(TokenKind::right_round))
		{
			iter_expr = ParseExpression();
			Expect(TokenKind::right_round);
		}
		std::unique_ptr<StmtAST> stmt = ParseStatement();

		std::unique_ptr<ForStmtAST> for_stmt = std::make_unique<ForStmtAST>(std::move(stmt));
		for_stmt->SetInit(std::move(init));
		for_stmt->SetConditionExpression(std::move(cond_expr));
		for_stmt->SetIterExpression(std::move(iter_expr));
		
		return for_stmt;
	}

	std::unique_ptr<ReturnStmtAST> Parser::ParseReturnStatement()
	{
		std::unique_ptr<ExprStmtAST> ret_expr = ParseExpressionStatement();
		return std::make_unique<ReturnStmtAST>(std::move(ret_expr));
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

	//<parenthesized - expression> :: = (<expression>)
	std::unique_ptr<ExprAST> Parser::ParseParenthesizedExpression()
	{
		Expect(TokenKind::left_round);
		std::unique_ptr<ExprAST> expr = ParseExpression();
		Expect(TokenKind::right_round);
		return expr;
	}

	//<assignment - expression> ::= <conditional - expression>
	//								| <unary - expression> <assignment - operator> <assignment - expression>
	std::unique_ptr<ExprAST> Parser::ParseAssignExpression()
	{
		std::unique_ptr<ExprAST> lhs = ParseConditionalExpression();
		BinaryExprKind arith_op_kind = BinaryExprKind::Assign;
		switch (current_token->GetKind()) 
		{
		case TokenKind::equal: arith_op_kind = BinaryExprKind::Assign; break;
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
		//if (arith_op_kind != BinaryExprKind::Assign)
		//{
		//	std::unique_ptr<BinaryExprAST> parent = std::make_unique<BinaryExprAST>(arith_op_kind);
		//	parent->SetLHS(std::move(lhs));
		//	parent->SetRHS(std::move(rhs));
		//	rhs = std::move(parent);
		//}

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
		switch (current_token->GetKind())
		{
		case TokenKind::left_round: return ParseParenthesizedExpression();
		case TokenKind::identifier: return ParseIdentifier(); break;
		case TokenKind::number: return ParseIntegerLiteral();
		case TokenKind::string_literal: return ParseStringLiteral(); break;
		default:
			Report(diag::unexpected_token);
		}
		LU_ASSERT(false);
		return nullptr;
	}

	std::unique_ptr<IntegerLiteralAST> Parser::ParseIntegerLiteral()
	{
		LU_ASSERT(current_token->Is(TokenKind::number));
		std::string_view string_number = current_token->GetIdentifier();
		int64 value = std::stoll(current_token->GetIdentifier().data(), nullptr, 0);
		++current_token;
		return std::make_unique<IntegerLiteralAST>(value);
	}

	std::unique_ptr<StringLiteralAST> Parser::ParseStringLiteral()
	{
		LU_ASSERT(current_token->Is(TokenKind::string_literal));
		std::string_view str = current_token->GetIdentifier();
		++current_token;
		return std::make_unique<StringLiteralAST>(str);
	}

	std::unique_ptr<IdentifierAST> Parser::ParseIdentifier()
	{
		LU_ASSERT(current_token->Is(TokenKind::identifier));
		std::string_view name = current_token->GetIdentifier();
		++current_token;
		return std::make_unique<IdentifierAST>(name);
	}

	//<declaration - specifier> :: = <storage - class - specifier>
	//							 | <type - specifier>
	//							 | <type - qualifier>
	bool Parser::ParseDeclSpec(DeclSpecInfo& decl_spec, bool forbid_storage_specs /*= false*/)
	{
		decl_spec = DeclSpecInfo{};
		decl_spec.qtype = builtin_types::Int;

		enum TypeFlags
		{
			VOID = 1 << 0,
			BOOL = 1 << 2,
			CHAR = 1 << 4,
			SHORT = 1 << 6,
			INT = 1 << 8,
			LONG = 1 << 10,
			FLOAT = 1 << 12,
			DOUBLE = 1 << 14,
			OTHER = 1 << 16,
			SIGNED = 1 << 17,
			UNSIGNED = 1 << 18,
		};
		uint32 counter = 0;
		while (current_token->IsDeclSpec())
		{
			if (current_token->IsStorageSpecifier())
			{
				if (forbid_storage_specs)
				{
					Report(diag::storage_specifier_forbidden_context);
					return false;
				}

				TokenKind kind = current_token->GetKind();
				++current_token;
				if (decl_spec.storage != Storage::None)
				{
					Report(diag::multiple_storage_specifiers);
					return false;
				}

				switch (kind)
				{
				case TokenKind::KW_static:
					decl_spec.storage = Storage::Static;
					break;
				case TokenKind::KW_register:
					decl_spec.storage = Storage::Register;
					break;
				case TokenKind::KW_typedef:
					decl_spec.storage = Storage::Typedef;
					break;
				case TokenKind::KW__Thread_local:
					decl_spec.storage = Storage::ThreadLocal;
					break;
				case TokenKind::KW_extern:
					decl_spec.storage = Storage::Extern;
					break;
				default:
					LU_UNREACHABLE();
				}
			}

			//ignore for now, later add support: atomic, tls, alignas
			if (Consume(
				TokenKind::KW_auto, TokenKind::KW_register, TokenKind::KW__Atomic,
				TokenKind::KW__Alignas, TokenKind::KW__Thread_local))
				continue;

			if (Consume(TokenKind::KW_const))
			{
				decl_spec.qtype.AddConst();
				continue;
			}
			if (Consume(TokenKind::KW_volatile))
			{
				decl_spec.qtype.AddVolatile();
				continue;
			}

			if (!current_token->IsDeclSpec()) break;

			TokenKind kind = current_token->GetKind();
			//#todo Add support for user-defined types

			//builtin types
			switch (kind)
			{
			case TokenKind::KW_void: counter += VOID; break;
			case TokenKind::KW_bool: counter += BOOL; break;
			case TokenKind::KW_char: counter += CHAR; break;
			case TokenKind::KW_short: counter += SHORT; break;
			case TokenKind::KW_int: counter += INT; break;
			case TokenKind::KW_long: counter += LONG; break;
			case TokenKind::KW_float: counter += FLOAT; break;
			case TokenKind::KW_double: counter += DOUBLE; break;
			case TokenKind::KW_signed: counter += SIGNED; break;
			case TokenKind::KW_unsigned: counter += UNSIGNED; break;
			default: LU_UNREACHABLE();
			}
			++current_token;

			switch (counter)
			{
			case VOID:
				decl_spec.qtype.SetRawType(builtin_types::Void);
				break;
			case BOOL:
				decl_spec.qtype.SetRawType(builtin_types::Bool);
				break;
			case CHAR:
			case SIGNED + CHAR:
				decl_spec.qtype.SetRawType(builtin_types::Char);
				break;
			case UNSIGNED + CHAR:
				decl_spec.qtype.SetRawType(builtin_types::UnsignedChar);
				break;
			case SHORT:
			case SHORT + INT:
			case SIGNED + SHORT:
			case SIGNED + SHORT + INT:
				decl_spec.qtype.SetRawType(builtin_types::Short);
				break;
			case UNSIGNED + SHORT:
			case UNSIGNED + SHORT + INT:
				decl_spec.qtype.SetRawType(builtin_types::UnsignedShort);
				break;
			case INT:
			case SIGNED:
			case SIGNED + INT:
				decl_spec.qtype.SetRawType(builtin_types::Int);
				break;
			case UNSIGNED:
			case UNSIGNED + INT:
				decl_spec.qtype.SetRawType(builtin_types::UnsignedInt);
				break;
			case LONG:
			case LONG + INT:
			case SIGNED + LONG:
			case SIGNED + LONG + INT:
				decl_spec.qtype.SetRawType(builtin_types::Long);
				break;
			case LONG + LONG:
			case LONG + LONG + INT:
			case SIGNED + LONG + LONG:
			case SIGNED + LONG + LONG + INT:
				decl_spec.qtype.SetRawType(builtin_types::LongLong);
				break;
			case UNSIGNED + LONG:
			case UNSIGNED + LONG + INT:
				decl_spec.qtype.SetRawType(builtin_types::UnsignedLong);
				break;
			case UNSIGNED + LONG + LONG:
			case UNSIGNED + LONG + LONG + INT:
				decl_spec.qtype.SetRawType(builtin_types::UnsignedLongLong);
				break;
			case FLOAT:
				decl_spec.qtype.SetRawType(builtin_types::Float);
				break;
			case DOUBLE:
				decl_spec.qtype.SetRawType(builtin_types::Double);
				break;
			case LONG + DOUBLE:
				decl_spec.qtype.SetRawType(builtin_types::LongDouble);
				break;
			default:
				//diag 
				return false;
			}
		}
		return true;
	}

	//<declarator> :: = { <pointer> } ? <direct - declarator>
	//<direct - declarator> :: = <identifier>
	//						 | (<declarator>)
	//						 | <direct - declarator>[{<constant - expression>} ? ]
	//						 | <direct - declarator> (<parameter - type - list>)
	//						 | <direct - declarator> ({ <identifier> }*)
	bool Parser::ParseDeclarator(DeclSpecInfo const& decl_spec, DeclaratorInfo& declarator)
	{
		declarator.qtype = decl_spec.qtype;
		ParsePointers(declarator.qtype);

		if (Consume(TokenKind::left_round))
		{
			//DeclaratorInfo stub{};
			//ParseDeclarator(decl_spec, stub);
			//Expect(TokenKind::right_round);
			//return true;
			return false;
		}

		if (current_token->Is(TokenKind::identifier))
		{
			declarator.name = current_token->GetIdentifier();
			declarator.loc = current_token->GetLocation();
			++current_token;
		}
		return ParseTypeSuffix(declarator.qtype);
	}

	//<pointer> :: = * { <type - qualifier> }* {<pointer>} ?
	//<type - qualifier> :: = const | volatile
	bool Parser::ParsePointers(QualifiedType& type)
	{
		while (Consume(TokenKind::star))
		{
			PointerType ptr_type(type);
			type.SetRawType(ptr_type);
			if (current_token->Is(TokenKind::KW_const))
			{
				type.AddConst();
				++current_token;
			}
			if (current_token->Is(TokenKind::KW_volatile))
			{
				type.AddVolatile();
				++current_token;
			}
		}
		return true;
	}

	// func-params = ("void" | param ("," param)* ("," "...")?)? ")"
	// param = declspec declarator
	// array-dimensions = "["("static" | "restrict")* const-expr? "]" type-suffix
	bool Parser::ParseTypeSuffix(QualifiedType& type)
	{
		if (Consume(TokenKind::left_round))
		{
			if (Consume(TokenKind::KW_void))
			{
				FuncType func_type(type);
				type.SetRawType(func_type);
				if (!Consume(TokenKind::right_round))
				{
					Report(diag::function_params_not_closed);
					return false;
				}
				else return true;
			}
			else
			{
				bool is_variadic = false;
				std::vector<FunctionParameter> param_types{};
				bool first = true;
				while (!Consume(TokenKind::right_round))
				{
					if (!first && !Consume(TokenKind::comma))
					{
						Report(diag::function_params_missing_coma);
						return false;
					}
					first = false;

					if (Consume(TokenKind::ellipsis))
					{
						is_variadic = true;
						if (!Consume(TokenKind::right_round))
						{
							Report(diag::variadic_params_not_last);
							return false;
						}
						else break;
					}

					DeclSpecInfo param_decl_spec{};
					if (!ParseDeclSpec(param_decl_spec)) return false;
					DeclaratorInfo param_declarator{};
					if (!ParseDeclarator(param_decl_spec, param_declarator)) return false;
					QualifiedType& qtype = param_declarator.qtype;
					if (qtype->Is(PrimitiveTypeKind::Void)) return false;
					else if (qtype->Is(PrimitiveTypeKind::Array))
					{
						ArrayType const& array_type = TypeCast<ArrayType const&>(*qtype);
						QualifiedType base_type = array_type.BaseQualifiedType();
						PointerType decayed_param_type(base_type);
						qtype = QualifiedType(decayed_param_type);
					}
					else if (qtype->Is(PrimitiveTypeKind::Function))
					{
						FuncType const& function_type = TypeCast<FuncType const&>(*qtype);
						PointerType decayed_param_type(function_type);
						qtype = QualifiedType(decayed_param_type);
					}
					param_types.emplace_back(param_declarator.name, param_declarator.qtype);
				}
				FuncType func_type(type, param_types, is_variadic);
				type.SetRawType(func_type);
				return true;
			}
		}
		else if (Consume(TokenKind::left_square))
		{
			while (Consume(TokenKind::KW_static, TokenKind::KW_restrict));
			if (Consume(TokenKind::right_square))
			{
				ArrayType arr_type(type);
				type.SetRawType(arr_type);
				return ParseTypeSuffix(type);
			}
			else if (current_token->Is(TokenKind::number))
			{
				size_t array_size = std::stoull(current_token->GetIdentifier().data(), nullptr, 0); //#todo check if the it succeeded

				ArrayType arr_type(type, array_size);
				type.SetRawType(arr_type);
				if (!Consume(TokenKind::right_square))
				{
					Report(diag::function_params_not_closed);
					return false;
				}
				else return ParseTypeSuffix(type);
			}
		}
		return true;
	}

}