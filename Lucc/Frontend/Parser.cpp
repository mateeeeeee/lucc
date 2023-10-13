#include "Parser.h"
#include "AST.h"

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

	struct Parser::DeclarationSpecifier
	{
		uint32 align = 0;
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
		DeclarationInfo(DeclarationSpecifier const& decl_spec, DeclaratorInfo const& declarator)
			: name(declarator.name), qtype(declarator.qtype), loc(declarator.loc),
			align(decl_spec.align), storage(decl_spec.storage), func_spec(decl_spec.func_spec)
		{}
		std::string name = "";
		QualifiedType qtype{};
		SourceLocation loc;
		uint32 align = 0;
		Storage storage = Storage::None;
		FunctionSpecifier func_spec = FunctionSpecifier::None;
	};

	Parser::Parser(std::vector<Token> const& _tokens)
		: tokens(_tokens), current_token(tokens.begin()) {}
	Parser::~Parser() = default;
	void Parser::Parse()
	{
		ast = std::make_unique<AST>();
		ctx.decl_scope_stack = std::make_unique<ScopeStack<DeclSymbol>>();
		ctx.tag_scope_stack = std::make_unique<ScopeStack<TagSymbol>>();
		ParseTranslationUnit();
	}
	bool Parser::Expect(TokenKind k)
	{
		if (!Consume(k))
		{
			Diag(unexpected_token);
			return false;
		}
		return true;
	}
	void Parser::Diag(DiagCode code)
	{
		--current_token;
		g_Diagnostics.Report(current_token->GetLocation(), code);
		++current_token;
	}

	void Parser::ParseTranslationUnit()
	{
		while (current_token->IsNot(TokenKind::eof))
		{
			auto declarations = ParseDeclaration();
			for(auto&& declaration : declarations) ast->translation_unit->AddDecl(std::move(declaration));
		}
	}

	UniqueDeclPtrList Parser::ParseDeclaration()
	{
		while (Consume(TokenKind::semicolon)) Diag(empty_statement);
		bool is_global = ctx.decl_scope_stack->IsGlobal();

		UniqueDeclPtrList decls;

		DeclarationSpecifier decl_spec{};
		ParseDeclarationSpecifier(decl_spec);

		if (decl_spec.storage == Storage::Typedef)
		{
			auto typedef_decls = ParseTypedefDeclaration(decl_spec);
			for (auto&& typedef_decl : typedef_decls) decls.push_back(std::move(typedef_decl));
			return decls;
		}

		if (Consume(TokenKind::semicolon)) return {};

		do
		{
			DeclaratorInfo declarator_info{};
			ParseDeclarator(decl_spec, declarator_info);

			if (decl_spec.align)
			{
				if (decl_spec.align >= declarator_info.qtype->GetAlign()) declarator_info.qtype->SetAlign(decl_spec.align);
				else Diag(alignas_cannot_reduce_default_align);
			}

			LU_ASSERT(declarator_info.qtype.HasRawType());

			DeclarationInfo declaration_info(decl_spec, declarator_info);
			DeclSymbol var_symbol{ declaration_info.name, declaration_info.qtype, declaration_info.storage, is_global };

			bool check_redefinition = false;
			if (DeclSymbol* sym = ctx.decl_scope_stack->LookUpCurrentScope(declaration_info.name))
			{
				if (!IsFunctionType(declaration_info.qtype))
				{
					Diag(redefinition_of_identifier);
					return {};
				}
				if (IsFunctionType(sym->qtype))
				{
					FunctionType& func_type = sym->qtype->As<FunctionType>();
					if (!func_type.IsCompatible(declaration_info.qtype))
					{
						Diag(redefinition_of_identifier);
						return {};
					}
					if (func_type.HasDefinition())
					{
						check_redefinition = true;
					}
				}
				else
				{
					Diag(redefinition_of_identifier);
					return {};
				}
			}
			else
			{
				bool success = ctx.decl_scope_stack->Insert(var_symbol);
				LU_ASSERT(success);
			}

			if (IsFunctionType(declarator_info.qtype))
			{
				if (!is_global) Diag(local_functions_not_allowed);

				UniqueFunctionDeclPtr func_decl = ParseFunctionDeclaration(declaration_info);
				func_decl->SetLocation(current_token->GetLocation());
				func_decl->SetSymbol(&var_symbol);

				DeclSymbol* sym = ctx.decl_scope_stack->LookUp(declaration_info.name);
				sym->decl_ast = func_decl.get();
				if (func_decl->IsDefinition())
				{
					if(check_redefinition) 
					{
						Diag(redefinition_of_identifier);
						return {};
					}
					LU_ASSERT(decls.empty());
					decls.push_back(std::move(func_decl));
					return decls;
				}
				decls.push_back(std::move(func_decl));
			}
			else
			{
				if (IsVoidType(declaration_info.qtype)) Diag(void_not_expected);

				std::string_view name = declarator_info.name;
				UniqueVariableDeclPtr var_decl = MakeUnique<VariableDecl>(name);
				var_decl->SetLocation(current_token->GetLocation());
				var_decl->SetSymbol(&var_symbol);

				DeclSymbol* sym = ctx.decl_scope_stack->LookUp(declaration_info.name);
				sym->decl_ast = var_decl.get();
				if (Consume(TokenKind::equal))
				{
					UniqueExprPtr init_expr = ParseAssignmentExpression();
					if (is_global && !IsFunctionPointerType(declarator_info.qtype) && init_expr->GetExprKind() != ExprKind::IntLiteral) Diag(initializer_element_is_not_constant);
					UniqueExprPtr init_expr_casted = GetAssignExpr(std::move(init_expr), declaration_info.qtype);
					var_decl->SetInitExpr(std::move(init_expr_casted));
				}
				decls.push_back(std::move(var_decl));
			}
		} while (Consume(TokenKind::comma));
		Expect(TokenKind::semicolon);
		return decls;
	}

	UniqueTypedefDeclPtrList Parser::ParseTypedefDeclaration(DeclarationSpecifier const& decl_spec)
	{
		bool first = true;
		UniqueTypedefDeclPtrList typedefs{};
		while (!Consume(TokenKind::semicolon))
		{
			if (!first) Expect(TokenKind::comma);
			first = false;

			DeclaratorInfo typedef_info{};
			ParseDeclarator(decl_spec, typedef_info);

			if (typedef_info.name.empty())
			{
				Diag(typedef_name_empty);
				return {};
			}
			UniqueTypedefDeclPtr typedef_ast = MakeUnique<TypedefDecl>(typedef_info.name);
			typedefs.push_back(std::move(typedef_ast));
			bool success = ctx.decl_scope_stack->Insert(DeclSymbol{ typedef_info.name, typedef_info.qtype, Storage::Typedef });
			if (!success)
			{
				Diag(redefinition_of_identifier);
				return {};
			}
		}
		return typedefs;
	}

	UniqueFunctionDeclPtr Parser::ParseFunctionDeclaration(DeclarationInfo const& decl_info)
	{
		LU_ASSERT(ctx.decl_scope_stack->IsGlobal());
		SCOPE_STACK_GUARD(ctx.decl_scope_stack);

		std::string_view func_name = decl_info.name;
		if (func_name.empty())
		{
			Diag(missing_name);
			return nullptr;
		}

		FunctionType const& func_type = type_cast<FunctionType>(decl_info.qtype);
		UniqueFunctionDeclPtr func_decl = MakeUnique<FunctionDecl>(func_name);
		for (auto&& func_param : func_type.GetParamTypes())
		{
			bool success = ctx.decl_scope_stack->Insert(DeclSymbol{ func_param.name, func_param.qtype, Storage::None });
			if (!success)
			{
				Diag(redefinition_of_identifier);
				return nullptr;
			}
			UniqueVariableDeclPtr param_decl = MakeUnique<VariableDecl>(func_param.name);
			DeclSymbol* sym = ctx.decl_scope_stack->LookUp(func_param.name);
			param_decl->SetSymbol(sym);
			sym->decl_ast = param_decl.get();
			func_decl->AddParamDecl(std::move(param_decl));
		}
		func_type.EncounterPrototype();
		if (current_token->Is(TokenKind::left_brace))
		{
			SCOPE_STACK_GUARD(ctx.decl_scope_stack);
			ctx.current_func_type = &func_type;

			func_type.EncounteredDefinition();

			UniqueCompoundStmtPtr compound_stmt = ParseCompoundStatement();
			func_decl->SetFunctionBody(std::move(compound_stmt));

			if (func_name != "main" && ctx.current_func_type->GetReturnType()->IsNot(TypeKind::Void) && !ctx.return_stmt_encountered)
			{
				Diag(return_not_found);
				return nullptr;
			}

			ctx.current_func_type = nullptr;
		}

		for (std::string_view goto_label : ctx.gotos)
		{
			bool found = false;
			for (std::string_view label_name : ctx.labels)
			{
				if (goto_label == label_name)
				{
					found = true;
					break;
				}
			}
			if (!found) Diag(undeclared_label);
		}
		ctx.gotos.clear();
		ctx.labels.clear();

		return func_decl;
	}

	UniqueStmtPtr Parser::ParseStatement()
	{
		switch (current_token->GetKind())
		{
		case TokenKind::left_brace: return ParseCompoundStatement();
		case TokenKind::KW_if: return ParseIfStatement();
		case TokenKind::KW_while: return ParseWhileStatement();
		case TokenKind::KW_for: return ParseForStatement();
		case TokenKind::KW_do: return ParseDoWhileStatement();
		case TokenKind::KW_continue: return ParseContinueStatement();
		case TokenKind::KW_break: return ParseBreakStatement();
		case TokenKind::KW_return: return ParseReturnStatement();
		case TokenKind::KW_goto: return ParseGotoStatement();
		case TokenKind::KW_switch: return ParseSwitchStatement();
		case TokenKind::KW_case:
		case TokenKind::KW_default: return ParseCaseStatement();
		case TokenKind::identifier:
			if ((current_token + 1)->Is(TokenKind::colon)) return ParseLabelStatement();
			[[fallthrough]];
		default:
			return ParseExpressionStatement();
		}
		return nullptr;
	}

	UniqueExprStmtPtr Parser::ParseExpressionStatement()
	{
		if (Consume(TokenKind::semicolon)) return MakeUnique<NullStmt>();
		UniqueExprPtr expression = ParseExpression();
		Expect(TokenKind::semicolon);
		return MakeUnique<ExprStmt>(std::move(expression));
	}

	UniqueCompoundStmtPtr Parser::ParseCompoundStatement()
	{
		Expect(TokenKind::left_brace);
		SCOPE_STACK_GUARD(ctx.decl_scope_stack);
		UniqueCompoundStmtPtr compound_stmt = MakeUnique<CompoundStmt>();
		while (current_token->IsNot(TokenKind::right_brace))
		{
			if (IsTokenTypename())
			{
				UniqueDeclPtrList decl = ParseDeclaration();
				compound_stmt->AddStmt(MakeUnique<DeclStmt>(std::move(decl)));
			}
			else
			{
				UniqueStmtPtr stmt = ParseStatement();
				compound_stmt->AddStmt(std::move(stmt));
			}
		}
		Expect(TokenKind::right_brace);
		return compound_stmt;
	}

	UniqueIfStmtPtr Parser::ParseIfStatement()
	{
		Expect(TokenKind::KW_if);
		UniqueIfStmtPtr if_stmt = MakeUnique<IfStmt>();
		if_stmt->SetCondition(ParseParenthesizedExpression());
		if_stmt->SetThenStmt(ParseStatement());
		if (Consume(TokenKind::KW_else)) if_stmt->SetElseStmt(ParseStatement());
		return if_stmt;
	}

	//<while - statement> ::= while (<expression>) < statement >
	UniqueWhileStmtPtr Parser::ParseWhileStatement()
	{
		Expect(TokenKind::KW_while);
		UniqueWhileStmtPtr while_stmt = MakeUnique<WhileStmt>();
		ctx.break_callback_stack.push_back([&](BreakStmt* break_stmt) { while_stmt->AddBreakStmt(break_stmt); });
		ctx.continue_callback_stack.push_back([&](ContinueStmt* continue_stmt) { while_stmt->AddContinueStmt(continue_stmt); });
		while_stmt->SetConditionExpr(ParseParenthesizedExpression());
		while_stmt->SetBodyStmt(ParseStatement());
		ctx.continue_callback_stack.pop_back();
		ctx.break_callback_stack.pop_back();
		return while_stmt;
	}
	//<dowhile - statement> ::= do <statement> while ( <expression> ) ;
	UniqueDoWhileStmtPtr Parser::ParseDoWhileStatement()
	{
		Expect(TokenKind::KW_do);
		UniqueDoWhileStmtPtr dowhile_stmt = MakeUnique<DoWhileStmt>();
		ctx.break_callback_stack.push_back([&](BreakStmt* break_stmt) { dowhile_stmt->AddBreakStmt(break_stmt); });
		ctx.continue_callback_stack.push_back([&](ContinueStmt* continue_stmt) { dowhile_stmt->AddContinueStmt(continue_stmt); });
		dowhile_stmt->SetBodyStmt(ParseStatement());
		Expect(TokenKind::KW_while);
		dowhile_stmt->SetConditionExpr(ParseParenthesizedExpression());
		Expect(TokenKind::semicolon);
		ctx.continue_callback_stack.pop_back();
		ctx.break_callback_stack.pop_back();
		return dowhile_stmt;
	}

	//<for - statement> ::= for ( {<init>}? ; {<expression>}? ; {<expression>}? ) <statement>
	UniqueForStmtPtr Parser::ParseForStatement()
	{
		Expect(TokenKind::KW_for);
		Expect(TokenKind::left_round);
		UniqueForStmtPtr for_stmt = MakeUnique<ForStmt>();

		UniqueStmtPtr init = nullptr;
		if (IsTokenTypename())
		{
			UniqueDeclPtrList decl = ParseDeclaration();
			for_stmt->SetInitStmt(MakeUnique<DeclStmt>(std::move(decl)));
		}
		else for_stmt->SetInitStmt(ParseExpressionStatement());

		UniqueExprPtr cond_expr = nullptr;
		if (!Consume(TokenKind::semicolon))
		{
			for_stmt->SetConditionExpr(ParseExpression());
			Expect(TokenKind::semicolon);
		}

		UniqueExprPtr iter_expr = nullptr;
		if (!Consume(TokenKind::right_round))
		{
			for_stmt->SetIterationExpr(ParseExpression());
			Expect(TokenKind::right_round);
		}
		ctx.break_callback_stack.push_back([&](BreakStmt* break_stmt) { for_stmt->AddBreakStmt(break_stmt); });
		ctx.continue_callback_stack.push_back([&](ContinueStmt* continue_stmt) { for_stmt->AddContinueStmt(continue_stmt); });
		for_stmt->SetBodyStmt(ParseStatement());
		ctx.continue_callback_stack.pop_back();
		ctx.break_callback_stack.pop_back();
		return for_stmt;
	}

	//<return - statement> ::= return {<expression>}? ;
	UniqueReturnStmtPtr Parser::ParseReturnStatement()
	{
		LU_ASSERT(ctx.current_func_type != nullptr);
		Expect(TokenKind::KW_return);
		UniqueExprStmtPtr ret_expr_stmt = ParseExpressionStatement();
		Expr* ret_expr = ret_expr_stmt->GetExpr();
		QualifiedType ret_type = ctx.current_func_type->GetReturnType();
		ctx.return_stmt_encountered = true;
		UniqueExprStmtPtr ret_expr_cast = GetAssignExprStmt(std::move(ret_expr_stmt), ret_type);
		return MakeUnique<ReturnStmt>(std::move(ret_expr_cast));
	}

	UniqueBreakStmtPtr Parser::ParseBreakStatement()
	{
		Expect(TokenKind::KW_break);
		Expect(TokenKind::semicolon);
		UniqueBreakStmtPtr break_stmt = MakeUnique<BreakStmt>();
		if (ctx.break_callback_stack.empty()) Diag(stray_break);
		else ctx.break_callback_stack.back()(break_stmt.get());
		return break_stmt;
	}

	UniqueContinueStmtPtr Parser::ParseContinueStatement()
	{
		Expect(TokenKind::KW_continue);
		Expect(TokenKind::semicolon);
		UniqueContinueStmtPtr continue_stmt = MakeUnique<ContinueStmt>();
		if (ctx.continue_callback_stack.empty()) Diag(stray_continue);
		else ctx.continue_callback_stack.back()(continue_stmt.get());
		return continue_stmt;
	}

	UniqueGotoStmtPtr Parser::ParseGotoStatement()
	{
		Expect(TokenKind::KW_goto);
		std::string_view label_name = current_token->GetIdentifier();
		Expect(TokenKind::identifier);
		Expect(TokenKind::semicolon);
		UniqueGotoStmtPtr goto_stmt = MakeUnique<GotoStmt>(label_name);
		ctx.gotos.push_back(label_name.data());
		return goto_stmt;
	}

	UniqueSwitchStmtPtr Parser::ParseSwitchStatement()
	{
		Expect(TokenKind::KW_switch);
		UniqueSwitchStmtPtr switch_stmt = MakeUnique<SwitchStmt>();
		ctx.switch_stack.push_back(switch_stmt.get());
		ctx.break_callback_stack.push_back([&](BreakStmt* break_stmt) { switch_stmt->AddBreakStmt(break_stmt); });
		switch_stmt->SetConditionExpr(ParseParenthesizedExpression());
		switch_stmt->SetBodyStmt(ParseStatement());
		ctx.break_callback_stack.pop_back();
		ctx.switch_stack.pop_back();
		return switch_stmt;
	}

	UniqueCaseStmtPtr Parser::ParseCaseStatement()
	{
		if (ctx.switch_stack.empty()) Diag(stray_case);

		UniqueCaseStmtPtr case_stmt = nullptr;
		if (Consume(TokenKind::KW_default))
		{
			if (ctx.switch_stack.back()->HasDefaultCase()) Diag(multiple_default_cases);
			case_stmt = MakeUnique<CaseStmt>();
		}
		else
		{
			Expect(TokenKind::KW_case);
			UniqueExprPtr case_value = ParseExpression();
			if (!case_value->IsConstexpr()) Diag(case_value_not_constexpr);
			case_stmt = MakeUnique<CaseStmt>(case_value->EvaluateConstexpr());
		}
		Expect(TokenKind::colon);
		ctx.switch_stack.back()->AddCaseStmt(case_stmt.get());
		return case_stmt;
	}

	UniqueLabelStmtPtr Parser::ParseLabelStatement()
	{
		std::string_view label_name = current_token->GetIdentifier();
		Expect(TokenKind::identifier);
		Expect(TokenKind::colon);
		UniqueLabelStmtPtr label_stmt = MakeUnique<LabelStmt>(label_name);
		ctx.labels.push_back(label_name.data());
		return label_stmt;
	}

	//<initializer> ::= <assignment-expression>
	//				  | { <initializer - list> }
	//				  | { <initializer - list>, }
	//<initializer - list> :: = <initializer>
	//						  | <initializer - list>, <initializer>
	UniqueExprPtr Parser::ParseInitializer(bool static_init)
	{
		if (Consume(TokenKind::left_brace))
		{
			//do initializer list here
			Expect(TokenKind::right_brace);
			return nullptr;
		}
		else return ParseExpression();
	}

	template<ExprParseFn ParseFn, TokenKind token_kind, BinaryExprKind op_kind>
	UniqueExprPtr Parser::ParseBinaryExpression()
	{
		UniqueExprPtr lhs = (this->*ParseFn)();
		while (Consume(token_kind))
		{
			SourceLocation loc = current_token->GetLocation();
			UniqueExprPtr rhs = (this->*ParseFn)();
			UniqueBinaryExprPtr parent = MakeUnique<BinaryExpr>(op_kind, loc);
			parent->SetLHS(std::move(lhs));
			parent->SetRHS(std::move(rhs));
			lhs = std::move(parent);
		}
		return lhs;
	}

	//<expression> ::=  <assignment - expression>
	//					| <expression>, <assignment - expression>
	UniqueExprPtr Parser::ParseExpression()
	{
		return ParseBinaryExpression<&Parser::ParseAssignmentExpression, TokenKind::comma, BinaryExprKind::Comma>();
	}

	//<parenthesized - expression> :: = (<expression>)
	UniqueExprPtr Parser::ParseParenthesizedExpression()
	{
		Expect(TokenKind::left_round);
		UniqueExprPtr expr = ParseExpression();
		Expect(TokenKind::right_round);
		return expr;
	}

	//<assignment - expression> ::= <conditional - expression>
	//								| <unary - expression> <assignment - operator> <assignment - expression>
	UniqueExprPtr Parser::ParseAssignmentExpression()
	{
		TokenPtr current_token_copy = current_token;
		UniqueExprPtr lhs = ParseConditionalExpression();
		BinaryExprKind arith_op_kind = BinaryExprKind::Assign;
		SourceLocation loc = current_token->GetLocation();
		switch (current_token->GetKind())
		{
		case TokenKind::equal:					arith_op_kind = BinaryExprKind::Assign; break;
		case TokenKind::star_equal:				arith_op_kind = BinaryExprKind::Multiply; break;
		case TokenKind::slash_equal:			arith_op_kind = BinaryExprKind::Divide; break;
		case TokenKind::modulo_equal:			arith_op_kind = BinaryExprKind::Modulo; break;
		case TokenKind::plus_equal:				arith_op_kind = BinaryExprKind::Add; break;
		case TokenKind::minus_equal:			arith_op_kind = BinaryExprKind::Subtract; break;
		case TokenKind::less_less_equal:		arith_op_kind = BinaryExprKind::ShiftLeft; break;
		case TokenKind::greater_greater_equal:	arith_op_kind = BinaryExprKind::ShiftRight; break;
		case TokenKind::amp_equal:				arith_op_kind = BinaryExprKind::BitAnd; break;
		case TokenKind::pipe_equal:				arith_op_kind = BinaryExprKind::BitOr; break;
		case TokenKind::caret_equal:			arith_op_kind = BinaryExprKind::BitXor; break;
		default:
			return lhs;
		}
		if (!lhs->IsAssignable())
		{
			Diag(lhs_not_assignable);
			return nullptr;
		}
		++current_token;
		UniqueExprPtr rhs = ParseAssignmentExpression();
		if (arith_op_kind != BinaryExprKind::Assign)
		{
			//#todo: remove this hack, add clone to ast nodes
			TokenPtr current_token_copy2 = current_token;
			current_token = current_token_copy;
			UniqueExprPtr lhs_copy = ParseConditionalExpression();
			current_token = current_token_copy2;

			UniqueBinaryExprPtr tmp = MakeUnique<BinaryExpr>(arith_op_kind, loc);
			tmp->SetLHS(std::move(lhs_copy));
			tmp->SetRHS(std::move(rhs));

			UniqueBinaryExprPtr parent = MakeUnique<BinaryExpr>(BinaryExprKind::Assign, loc);
			parent->SetLHS(std::move(lhs));
			parent->SetRHS(std::move(tmp));
			return parent;
		}
		else
		{
			UniqueBinaryExprPtr parent = MakeUnique<BinaryExpr>(arith_op_kind, loc);
			parent->SetLHS(std::move(lhs));
			parent->SetRHS(std::move(rhs));
			return parent;
		}
	}

	//<conditional - expression> ::=  <logical - or -expression>
	//								| <logical - or -expression> ? <expression> : <conditional - expression>
	UniqueExprPtr Parser::ParseConditionalExpression()
	{
		SourceLocation loc = current_token->GetLocation();
		UniqueExprPtr cond = ParseLogicalOrExpression();
		if (Consume(TokenKind::question))
		{
			UniqueTernaryExprPtr ternary_expr = MakeUnique<TernaryExpr>(loc);
			ternary_expr->SetConditionExpr(std::move(cond));
			ternary_expr->SetTrueExpr(ParseExpression());
			Expect(TokenKind::colon);
			ternary_expr->SetFalseExpr(ParseConditionalExpression());
			return ternary_expr;
		}
		return cond;
	}

	//<logical - or -expression> :: = <logical - and -expression>
	//								| <logical - or -expression> || <logical - and -expression>
	UniqueExprPtr Parser::ParseLogicalOrExpression()
	{
		return ParseBinaryExpression<&Parser::ParseLogicalAndExpression, TokenKind::pipe_pipe, BinaryExprKind::LogicalOr>();
	}

	//	<logical - and - expression> :: = <inclusive - or - expression>
	//									| <logical - and - expression> && <inclusive - or - expression>
	UniqueExprPtr Parser::ParseLogicalAndExpression()
	{
		return ParseBinaryExpression<&Parser::ParseInclusiveOrExpression, TokenKind::amp_amp, BinaryExprKind::LogicalAnd>();
	}

	//<inclusive - or - expression> :: = <exclusive - or - expression>
	//								  | <inclusive - or - expression> | <exclusive - or - expression>
	UniqueExprPtr Parser::ParseInclusiveOrExpression()
	{
		return ParseBinaryExpression<&Parser::ParseExclusiveOrExpression, TokenKind::pipe, BinaryExprKind::BitOr>();
	}

	//<exclusive - or - expression> ::= <and - expression>
	//								| <exclusive - or - expression> ^ <and - expression>
	UniqueExprPtr Parser::ParseExclusiveOrExpression()
	{
		return ParseBinaryExpression<&Parser::ParseAndExpression, TokenKind::caret, BinaryExprKind::BitXor>();
	}

	//<and-expression> :: = <equality - expression>
	//					  | <and - expression> & <equality - expression>
	UniqueExprPtr Parser::ParseAndExpression()
	{
		return ParseBinaryExpression<&Parser::ParseEqualityExpression, TokenKind::amp, BinaryExprKind::BitAnd>();
	}

	//<equality - expression> :: = <relational - expression>
	//							 | <equality - expression> == <relational - expression>
	//							 | <equality - expression> != <relational - expression>
	UniqueExprPtr Parser::ParseEqualityExpression()
	{
		UniqueExprPtr lhs = ParseRelationalExpression();
		while (true)
		{
			BinaryExprKind op_kind = BinaryExprKind::Invalid;
			SourceLocation loc = current_token->GetLocation();
			switch (current_token->GetKind())
			{
			case TokenKind::equal_equal: op_kind = BinaryExprKind::Equal; break;
			case TokenKind::not_equal:	 op_kind = BinaryExprKind::NotEqual; break;
			default:
				return lhs;
			}
			++current_token;
			UniqueExprPtr rhs = ParseRelationalExpression();
			UniqueBinaryExprPtr parent = MakeUnique<BinaryExpr>(op_kind, loc);
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
	UniqueExprPtr Parser::ParseRelationalExpression()
	{
		UniqueExprPtr lhs = ParseShiftExpression();
		while (true)
		{
			BinaryExprKind op_kind = BinaryExprKind::Invalid;
			SourceLocation loc = current_token->GetLocation();
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
			UniqueExprPtr rhs = ParseShiftExpression();
			UniqueBinaryExprPtr parent = MakeUnique<BinaryExpr>(op_kind, loc);
			parent->SetLHS(std::move(lhs));
			parent->SetRHS(std::move(rhs));
			lhs = std::move(parent);
		}
	}

	//<shift - expression> :: = <additive - expression>
	//						| <shift - expression> << <additive - expression>
	//						| <shift - expression> >> <additive - expression>
	UniqueExprPtr Parser::ParseShiftExpression()
	{
		UniqueExprPtr lhs = ParseAdditiveExpression();
		while (true)
		{
			BinaryExprKind op_kind = BinaryExprKind::Invalid;
			SourceLocation loc = current_token->GetLocation();
			switch (current_token->GetKind())
			{
			case TokenKind::less_less:			op_kind = BinaryExprKind::ShiftLeft; break;
			case TokenKind::greater_greater:	op_kind = BinaryExprKind::ShiftRight; break;
			default:
				return lhs;
			}
			++current_token;
			UniqueExprPtr rhs = ParseAdditiveExpression();
			UniqueBinaryExprPtr parent = MakeUnique<BinaryExpr>(op_kind, loc);
			parent->SetLHS(std::move(lhs));
			parent->SetRHS(std::move(rhs));
			lhs = std::move(parent);
		}
	}

	//<additive - expression> :: = <multiplicative - expression>
	//							| <additive - expression> +<multiplicative - expression>
	//							| <additive - expression> -<multiplicative - expression>
	UniqueExprPtr Parser::ParseAdditiveExpression()
	{
		UniqueExprPtr lhs = ParseMultiplicativeExpression();
		while (true)
		{
			BinaryExprKind op_kind = BinaryExprKind::Invalid;
			SourceLocation loc = current_token->GetLocation();
			switch (current_token->GetKind())
			{
			case TokenKind::plus:			op_kind = BinaryExprKind::Add; break;
			case TokenKind::minus:			op_kind = BinaryExprKind::Subtract; break;
			default:
				return lhs;
			}
			++current_token;
			UniqueExprPtr rhs = ParseMultiplicativeExpression();
			UniqueBinaryExprPtr parent = MakeUnique<BinaryExpr>(op_kind, loc);
			parent->SetLHS(std::move(lhs));
			parent->SetRHS(std::move(rhs));
			lhs = std::move(parent);
		}
	}

	//<multiplicative - expression>   ::= <cast - expression>
	//									| <multiplicative - expression> *<cast - expression>
	//									| <multiplicative - expression> / <cast - expression>
	//									| <multiplicative - expression> % <cast - expression>
	UniqueExprPtr Parser::ParseMultiplicativeExpression()
	{
		UniqueExprPtr lhs = ParseCastExpression();
		while (true)
		{
			BinaryExprKind op_kind = BinaryExprKind::Invalid;
			SourceLocation loc = current_token->GetLocation();
			switch (current_token->GetKind())
			{
			case TokenKind::star:	op_kind = BinaryExprKind::Multiply; break;
			case TokenKind::slash:	op_kind = BinaryExprKind::Divide; break;
			case TokenKind::modulo: op_kind = BinaryExprKind::Modulo; break;
			default:
				return lhs;
			}
			++current_token;
			UniqueExprPtr rhs = ParseCastExpression();
			UniqueBinaryExprPtr parent = MakeUnique<BinaryExpr>(op_kind, loc);
			parent->SetLHS(std::move(lhs));
			parent->SetRHS(std::move(rhs));
			lhs = std::move(parent);
		}
	}

	//<cast - expression> :: = <unary - expression>
	//					   | (<type - name>) < cast - expression >
	UniqueExprPtr Parser::ParseCastExpression()
	{
		if (current_token->Is(TokenKind::left_round) && IsTokenTypename(1))
		{
			Expect(TokenKind::left_round);
			QualifiedType cast_type{};
			ParseTypename(cast_type);
			Expect(TokenKind::right_round);

			if (!IsVoidType(cast_type) && !IsScalarType(cast_type)) Diag(cast_invalid_type);
			//#todo further check cast type compatibility
			SourceLocation loc = current_token->GetLocation();
			UniqueCastExprPtr cast_expr = MakeUnique<CastExpr>(loc, cast_type);
			cast_expr->SetOperand(ParseCastExpression());
			return cast_expr;
		}
		else return ParseUnaryExpression();
	}

	//<unary - expression>  ::= <postfix - expression>
	//						| ++ <unary - expression>
	//						| -- <unary - expression>
	//						| <unary - operator> <cast - expression>
	//						| sizeof <unary - expression>
	//						| sizeof <type - name>
	UniqueExprPtr Parser::ParseUnaryExpression()
	{
		UniqueUnaryExprPtr unary_expr;
		SourceLocation loc = current_token->GetLocation();
		switch (current_token->GetKind())
		{
		// C11 6.5.3.2p1: The operand of the unary & operator shall be either a function
		// designator, the result of a [] or unary * operator, or an lvalue that
		// designates an object that is not a bit-field and is not declared with the
		// register storage-class specifier.
		case TokenKind::amp:
		{
			unary_expr = MakeUnique<UnaryExpr>(UnaryExprKind::AddressOf, loc);
			++current_token;
			UniqueExprPtr op_expr = ParseUnaryExpression();
			if (!op_expr->IsLValue() && !IsFunctionType(op_expr->GetType()))
			{
				Diag(address_of_rvalue_error);
				return nullptr;
			}
			unary_expr->SetOperand(std::move(op_expr));
			return unary_expr;
		}
		case TokenKind::star:
		{
			unary_expr = MakeUnique<UnaryExpr>(UnaryExprKind::Dereference, loc);
			++current_token;
			UniqueExprPtr op_expr = ParseUnaryExpression();
			if (!IsPointerLikeType(op_expr->GetType()))
			{
				Diag(dereferencing_non_pointer_type);
				return nullptr;
			}
			unary_expr->SetOperand(std::move(op_expr));
			return unary_expr;
		}
		case TokenKind::plus_plus:
			unary_expr = MakeUnique<UnaryExpr>(UnaryExprKind::PreIncrement, loc);
			break;
		case TokenKind::minus_minus:
			unary_expr = MakeUnique<UnaryExpr>(UnaryExprKind::PreDecrement, loc);
			break;
		case TokenKind::plus:
			unary_expr = MakeUnique<UnaryExpr>(UnaryExprKind::Plus, loc);
			break;
		case TokenKind::minus:
			unary_expr = MakeUnique<UnaryExpr>(UnaryExprKind::Minus, loc);
			break;
		case TokenKind::tilde:
			unary_expr = MakeUnique<UnaryExpr>(UnaryExprKind::BitNot, loc);
			break;
		case TokenKind::exclaim:
			unary_expr = MakeUnique<UnaryExpr>(UnaryExprKind::LogicalNot, loc);
			break;
		case TokenKind::KW_sizeof: return ParseSizeofExpression();
		case TokenKind::KW__Alignof: return ParseAlignofExpression();
		case TokenKind::KW__Alignas: return ParseAlignasExpression();
		default:
			return ParsePostFixExpression();
		}
		++current_token;
		unary_expr->SetOperand(ParseUnaryExpression());
		return unary_expr;
	}

	//<postfix - expression>    ::= <primary - expression>
	//							| <postfix - expression>[<expression>]
	//							| <postfix - expression> ({ <assignment - expression> }*)
	//							| <postfix - expression> . <identifier>
	//							| <postfix - expression> -> <identifier>
	//							| <postfix - expression> ++
	//							| <postfix - expression> --
	UniqueExprPtr Parser::ParsePostFixExpression()
	{
		UniqueExprPtr expr = ParsePrimaryExpression();

		SourceLocation loc = current_token->GetLocation();
		switch (current_token->GetKind())
		{
		case TokenKind::left_round:
		{
			QualifiedType const& type = expr->GetType();
			if (!IsFunctionPointerType(type) && !IsFunctionType(type))
			{
				Diag(invalid_function_call);
				return nullptr;
			}

			UniqueFunctionCallExprPtr func_call_expr = MakeUnique<FunctionCallExpr>(std::move(expr), current_token->GetLocation());
			++current_token;

			FunctionType const* func_type = nullptr;
			if (IsFunctionType(type)) func_type = &type->As<FunctionType>();
			else if (IsFunctionPointerType(type))
			{
				PointerType const& pointer_type = type->As<PointerType>();
				func_type = &pointer_type.PointeeType()->As<FunctionType>();
			}
			LU_ASSERT(func_type);

			std::span<FunctionParameter const> func_params = func_type->GetParamTypes();
			uint32 arg_index = 0;
			if (!Consume(TokenKind::right_round))
			{
				bool variadic_args = false;
				while (true)
				{
					UniqueExprPtr arg_expr = ParseAssignmentExpression();
					if (arg_index >= func_params.size())
					{
						if (!func_type->IsVariadic())
						{
							Diag(invalid_function_call);
							return nullptr;
						}
						else variadic_args = true;
					}

					if (!variadic_args) arg_expr = GetAssignExpr(std::move(arg_expr), func_params[arg_index].qtype);
					func_call_expr->AddArg(std::move(arg_expr));
					++arg_index;

					if (Consume(TokenKind::right_round)) break;
					Expect(TokenKind::comma);
				}
			}
			if (!func_type->IsVariadic() && func_params.size() != arg_index)
			{
				Diag(invalid_function_call);
				return nullptr;
			}
			return func_call_expr;
		}
		case TokenKind::plus_plus:
		{
			++current_token;
			UniqueUnaryExprPtr post_inc_expr = MakeUnique<UnaryExpr>(UnaryExprKind::PostIncrement, loc);
			post_inc_expr->SetOperand(std::move(expr));
			return post_inc_expr;
		}
		case TokenKind::minus_minus:
		{
			++current_token;
			UniqueUnaryExprPtr post_dec_expr = MakeUnique<UnaryExpr>(UnaryExprKind::PostDecrement, loc);
			post_dec_expr->SetOperand(std::move(expr));
			return post_dec_expr;
		}
		case TokenKind::left_square:
		{
			++current_token;
			if (!IsArrayType(expr->GetType()) && !IsPointerType(expr->GetType()))
			{
				Diag(dereferencing_non_pointer_type);
				return nullptr;
			}

			UniqueExprPtr bracket_expr = ParseExpression();
			Expect(TokenKind::right_square);

			UniqueUnaryExprPtr dereference_expr = MakeUnique<UnaryExpr>(UnaryExprKind::Dereference, loc);
			UniqueBinaryExprPtr add_expr = MakeUnique<BinaryExpr>(BinaryExprKind::Add, loc);
			add_expr->SetLHS(std::move(expr));
			add_expr->SetRHS(std::move(bracket_expr));
			dereference_expr->SetOperand(std::move(add_expr));
			return dereference_expr;
		}
		case TokenKind::period:
		{
			--current_token; 
			if (current_token->IsNot(TokenKind::identifier))
			{
				Diag(invalid_member_access);
				return nullptr;
			}
			std::string_view struct_name = current_token->GetIdentifier();
			DeclSymbol* var = ctx.decl_scope_stack->LookUp(struct_name);
			if (!var || var->is_enum || !IsStructType(var->qtype))
			{
				Diag(invalid_member_access);
				return nullptr;
			}
			Decl* decl_ast = var->decl_ast;

			current_token += 2;
			if (current_token->IsNot(TokenKind::identifier))
			{
				Diag(invalid_member_access);
				return nullptr;
			}
			std::string_view member_name = current_token->GetIdentifier();
			++current_token;

			StructType const& struct_type = var->qtype->As<StructType>();
			if (!struct_type.HasMember(member_name))
			{
				Diag(invalid_member_access);
				return nullptr;
			}
			UniqueMemberRefExprPtr member_access_expr = MakeUnique<MemberRefExpr>(decl_ast, member_name, loc);
			return member_access_expr;
		}
		case TokenKind::arrow:
		{
		//#todo
		}
		}
		return expr;
	}

	UniqueExprPtr Parser::ParseSizeofExpression()
	{
		Expect(TokenKind::KW_sizeof);
		if (Consume(TokenKind::left_round))
		{
			SourceLocation loc = current_token->GetLocation();
			if (IsTokenTypename())
			{
				QualifiedType type{};
				ParseTypename(type);
				if (IsFunctionType(type) || !type->IsComplete())
				{
					Diag(sizeof_invalid_argument);
				}
				Expect(TokenKind::right_round);
				return MakeUnique<IntLiteral>(type->GetSize(), loc);
			}
			else
			{
				UniqueExprPtr sizeof_expr = ParseExpression();
				Expect(TokenKind::right_round);
				return MakeUnique<IntLiteral>(sizeof_expr->GetType()->GetSize(), loc);
			}
		}
		else
		{
			SourceLocation loc = current_token->GetLocation();
			UniqueExprPtr sizeof_expr = ParseExpression();
			return MakeUnique<IntLiteral>(sizeof_expr->GetType()->GetSize(), loc);
		}
	}

	/*
	Returns the alignment requirement of the type named by type-name.
	If type-name is an array type, the result is the alignment requirement of the array element type.
	The type-name cannot be function type or an incomplete type.
	The result is an integer constant of type size_t.
	The operand is not evaluated (so external identifiers used in the operand do not have to be defined).
	*/
	UniqueIntLiteralPtr Parser::ParseAlignofExpression()
{
		Expect(TokenKind::KW__Alignof);
		Expect(TokenKind::left_round);

		if (!IsTokenTypename()) Diag(alignof_expects_type_argument);
		SourceLocation loc = current_token->GetLocation();
		QualifiedType type{};
		ParseTypename(type);
		if (IsFunctionType(type) || !type->IsComplete()) Diag(alignof_invalid_argument);
		UniqueIntLiteralPtr alignof_expr = MakeUnique<IntLiteral>(type->GetAlign(), loc);
		Expect(TokenKind::right_round);
		return alignof_expr;
	}

	/*
	The _Alignas (until C23)alignas (since C23) specifier can only be used when declaring objects that are not bit-fields,
	and don't have the register storage class.
	It cannot be used in function parameter declarations, and cannot be used in a typedef.

	When used in a declaration, the declared object will have its alignment requirement set to
	1,2) the result of the expression, unless it is zero
	3,4) the alignment requirement of type, that is, to _Alignof(type) (until C23)alignof(type) (since C23)
	except when this would weaken the alignment the type would have had naturally.
	If expression evaluates to zero, this specifier has no effect.
	*/
	UniqueIntLiteralPtr Parser::ParseAlignasExpression()
	{
		Expect(TokenKind::KW__Alignas);
		Expect(TokenKind::left_round);
		SourceLocation loc = current_token->GetLocation();
		UniqueIntLiteralPtr alignas_expr = nullptr;
		if (IsTokenTypename())
		{
			QualifiedType type{};
			ParseTypename(type);
			alignas_expr = MakeUnique<IntLiteral>(type->GetAlign(), loc);
		}
		else
		{
			auto IsPowerOfTwo = [](int64 n) {return n > 0 && ((n & (n - 1)) == 0); };
			UniqueExprPtr expr = ParseExpression();
			if (!expr->IsConstexpr()) Diag(alignas_alignment_not_constexpr);
			int64 alignment = expr->EvaluateConstexpr();
			if (!IsPowerOfTwo(alignment)) Diag(alignas_alignment_not_power_of_two);
			alignas_expr = MakeUnique<IntLiteral>(alignment, loc);
		}

		Expect(TokenKind::right_round);
		return alignas_expr;
	}

	//<primary - expression> :: = <identifier>
	//							| <constant>
	//							| <string>
	//							| (<expression>)
	UniqueExprPtr Parser::ParsePrimaryExpression()
	{
		switch (current_token->GetKind())
		{
		case TokenKind::left_round: return ParseParenthesizedExpression();
		case TokenKind::identifier: return ParseIdentifier(); break;
		case TokenKind::number: return ParseIntegerLiteral();
		case TokenKind::string_literal: return ParseStringLiteral(); break;
		default:
			Diag(unexpected_token);
		}
		LU_ASSERT(false);
		return nullptr;
	}

	UniqueIntLiteralPtr Parser::ParseIntegerLiteral()
	{
		LU_ASSERT(current_token->Is(TokenKind::number));
		std::string_view string_number = current_token->GetIdentifier();
		int64 value = std::stoll(current_token->GetIdentifier().data(), nullptr, 0);
		SourceLocation loc = current_token->GetLocation();
		++current_token;
		return MakeUnique<IntLiteral>(value, loc);
	}

	UniqueStringLiteralPtr Parser::ParseStringLiteral()
	{
		LU_ASSERT(current_token->Is(TokenKind::string_literal));
		std::string_view str = current_token->GetIdentifier();
		SourceLocation loc = current_token->GetLocation();
		++current_token;
		return MakeUnique<StringLiteral>(str, loc);
	}

	UniqueExprPtr Parser::ParseIdentifier()
	{
		LU_ASSERT(current_token->Is(TokenKind::identifier));
		std::string_view name = current_token->GetIdentifier();
		if (DeclSymbol* sym = ctx.decl_scope_stack->LookUp(name))
		{
			if (sym->is_enum)
			{
				SourceLocation loc = current_token->GetLocation();
				++current_token;
				return MakeUnique<IntLiteral>(sym->enum_value, loc);
			}
			else
			{
				SourceLocation loc = current_token->GetLocation();
				++current_token;
				UniqueDeclRefExprPtr decl_ref = MakeUnique<DeclRefExpr>(sym->decl_ast, loc);
				return decl_ref;
			}
		}
		else Diag(variable_not_declared);
		return nullptr;
	}

	void Parser::ParseEnum(DeclarationSpecifier& decl_spec)
	{
		Expect(TokenKind::KW_enum);
		decl_spec.qtype = builtin_types::Enum;

		std::string enum_tag = "";
		if (current_token->Is(TokenKind::identifier))
		{
			enum_tag = current_token->GetIdentifier();
			++current_token;
		}

		if (!enum_tag.empty() && current_token->IsNot(TokenKind::left_brace))
		{
			TagSymbol* sym = ctx.tag_scope_stack->LookUp(enum_tag);
			if (!sym) Diag(unknown_enum);
			else if (!sym->enum_type) Diag(not_enum_type);
			decl_spec.qtype = sym->type;
			return;
		}

		Expect(TokenKind::left_brace);
		int32 val = 0;
		while (true)
		{
			std::string enum_value_name;
			if (current_token->IsNot(TokenKind::identifier)) Diag(enum_value_no_name);
			enum_value_name = current_token->GetIdentifier(); ++current_token;

			if (Consume(TokenKind::equal))
			{
				UniqueExprPtr enum_value_expr = ParseAssignmentExpression();
				if (!enum_value_expr->IsConstexpr()) Diag(enum_value_not_constexpr);
				val = (int32)enum_value_expr->EvaluateConstexpr();
			}

			ctx.decl_scope_stack->Insert(DeclSymbol{ .name = enum_value_name, .is_enum = true, .enum_value = val++ });
			if (Consume(TokenKind::right_brace)) break;
			Expect(TokenKind::comma);
		}

		if (!enum_tag.empty()) ctx.tag_scope_stack->Insert(TagSymbol{ enum_tag, decl_spec.qtype, true });
	}

	void Parser::ParseStruct(DeclarationSpecifier& decl_spec)
	{
		Expect(TokenKind::KW_struct);

		std::string tag_name = "";
		if (current_token->Is(TokenKind::identifier))
		{
			tag_name = current_token->GetIdentifier();
			++current_token;
		}

		if (!tag_name.empty() && !current_token->Is(TokenKind::left_brace))
		{
			TagSymbol* tag = ctx.tag_scope_stack->LookUp(tag_name);
			if (tag)
			{
				decl_spec.qtype = tag->type;
				return;
			}

			decl_spec.qtype = StructType(tag_name);
			ctx.tag_scope_stack->Insert(TagSymbol(tag_name, decl_spec.qtype, false));
			return;
		}

		Expect(TokenKind::left_brace);

		decl_spec.qtype = StructType(tag_name);
		ParseStructMembers(decl_spec);

		if (!tag_name.empty()) 
		{
			TagSymbol* tag = ctx.tag_scope_stack->LookUpCurrentScope(tag_name);
			if (tag)
			{
				tag->type = decl_spec.qtype;
				return;
			}
			ctx.tag_scope_stack->Insert(TagSymbol(tag_name, decl_spec.qtype, false));
		}
	}

	// struct-members ::= (declspec declarator (","  declarator)* ";")*
	void Parser::ParseStructMembers(DeclarationSpecifier& decl_spec)
	{
		LU_ASSERT(IsStructType(decl_spec.qtype));
		StructType& struct_type = decl_spec.qtype->As<StructType>();
		
		while (!Consume(TokenKind::right_brace)) 
		{
			DeclarationSpecifier decl_spec_mem{};
			ParseDeclarationSpecifier(decl_spec_mem, false);
			bool first = true;

			// Anonymous struct member
			if ((IsStructType(decl_spec_mem.qtype) && Consume(TokenKind::semicolon)))
			{
				struct_type.AddMember("", decl_spec_mem.qtype);
				continue;
			}

			// Regular struct members
			while (!Consume(TokenKind::semicolon)) 
			{
				if (!first) Expect(TokenKind::comma);
				first = false;

				DeclaratorInfo declarator_mem{};
				ParseDeclarator(decl_spec_mem, declarator_mem);
				struct_type.AddMember(declarator_mem.name, declarator_mem.qtype);
			}
		}
		bool success = struct_type.Finalize();
		if (!success) Diag(invalid_member_declaration);
	}

	//<declaration - specifier> :: = <storage - class - specifier>
	//							 | <type - specifier>
	//							 | <type - qualifier>
	void Parser::ParseDeclarationSpecifier(DeclarationSpecifier& decl_spec, bool forbid_storage_specs /*= false*/)
	{
		using enum TokenKind;
		decl_spec = DeclarationSpecifier{};
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
		while (IsTokenTypename())
		{
			if (current_token->IsStorageSpecifier())
			{
				if (forbid_storage_specs) Diag(storage_specifier_forbidden_context);

				TokenKind kind = current_token->GetKind();
				++current_token;
				if (decl_spec.storage != Storage::None) Diag(multiple_storage_specifiers);

				switch (kind)
				{
				case KW_static:
					decl_spec.storage = Storage::Static;
					break;
				case KW_register:
					decl_spec.storage = Storage::Register;
					break;
				case KW_typedef:
					decl_spec.storage = Storage::Typedef;
					break;
				case KW__Thread_local:
					decl_spec.storage = Storage::ThreadLocal;
					break;
				case KW_extern:
					decl_spec.storage = Storage::Extern;
					break;
				case KW_auto:
					decl_spec.storage = Storage::Auto;
					break;
				default:
					LU_UNREACHABLE();
				}
			}

			if (Consume(KW__Atomic, KW__Thread_local)) continue;

			if (Consume(KW__Alignas))
			{
				if (forbid_storage_specs) Diag(storage_specifier_forbidden_context);
				--current_token;
				UniqueIntLiteralPtr alignas_expr = ParseAlignasExpression();
				decl_spec.align = (uint32)alignas_expr->GetValue();
				continue;
			}

			if (Consume(KW_const))
			{
				decl_spec.qtype.AddConst();
				continue;
			}
			if (Consume(KW_volatile))
			{
				decl_spec.qtype.AddVolatile();
				continue;
			}

			if (!IsTokenTypename()) break;

			// Handle user-defined types.
			QualifiedType* typedef_type = nullptr;
			if(current_token->Is(identifier))
			{
				DeclSymbol* sym = ctx.decl_scope_stack->LookUp(current_token->GetIdentifier());
				LU_ASSERT(sym);
				typedef_type = &sym->qtype;
			}

			if (current_token->IsOneOf(KW_struct, KW_union, KW_enum) || typedef_type)
			{
				if (counter) break;

				if (current_token->Is(KW_enum))
				{
					ParseEnum(decl_spec);
				}
				else if (current_token->Is(KW_struct))
				{
					ParseStruct(decl_spec);
				}
				else if (typedef_type)
				{
					decl_spec.qtype = *typedef_type;
					++current_token;
				}
				counter += OTHER;
				continue;
			}

			TokenKind kind = current_token->GetKind();

			//builtin types
			switch (kind)
			{
			case KW_void: counter += VOID; break;
			case KW_bool: counter += BOOL; break;
			case KW_char: counter += CHAR; break;
			case KW_short: counter += SHORT; break;
			case KW_int: counter += INT; break;
			case KW_long: counter += LONG; break;
			case KW_float: counter += FLOAT; break;
			case KW_double: counter += DOUBLE; break;
			case KW_signed: counter += SIGNED; break;
			case KW_unsigned: counter += UNSIGNED; break;
			break;
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
				Diag(declarator_specifier_error);
			}
		}
		if (counter == 0) Diag(declarator_specifier_error);
	}

	//<declarator> :: = { <pointer> } ? <direct - declarator>
	//<direct - declarator> :: = <identifier>
	//						 | (<declarator>)
	//						 | <direct - declarator>[{<constant - expression>} ? ]
	//						 | <direct - declarator> (<parameter - type - list>)
	//						 | <direct - declarator> ({ <identifier> }*)
	void Parser::ParseDeclarator(DeclarationSpecifier const& decl_spec, DeclaratorInfo& declarator)
	{
		declarator.qtype = decl_spec.qtype;
		ParsePointers(declarator.qtype);

		if (Consume(TokenKind::left_round))
		{
			if (IsTokenTypename())
			{
				return ParseDeclaratorTailFunction(declarator.qtype);
			}
			else
			{
				TokenPtr start = current_token;
				DeclaratorInfo dummy{};
				ParseDeclarator(decl_spec, dummy);
				Expect(TokenKind::right_round);
				ParseDeclaratorTail(declarator.qtype);
				TokenPtr end = current_token;
				current_token = start;
				DeclarationSpecifier decl_spec2{ decl_spec };
				decl_spec2.qtype = declarator.qtype;
				ParseDeclarator(decl_spec2, declarator);
				current_token = end;
				return;
			}
		}
		if (current_token->Is(TokenKind::identifier))
		{
			declarator.name = current_token->GetIdentifier();
			declarator.loc = current_token->GetLocation();
			++current_token;
		}
		return ParseDeclaratorTail(declarator.qtype);
	}

	//<abstract-declarator> ::= <pointer>
	//                        | <pointer> <direct-abstract-declarator>
	//                        | <direct-abstract-declarator>
	//<direct-abstract-declarator> ::=  ( <abstract-declarator> )
	//                               | {<direct-abstract-declarator>}? [ {<constant-expression>}? ]
	//                               | {<direct-abstract-declarator>}? ( {<parameter-type-list>}? )
	void Parser::ParseAbstractDeclarator(DeclarationSpecifier const& decl_spec, QualifiedType& abstract_declarator)
	{
		abstract_declarator = decl_spec.qtype;
		ParsePointers(abstract_declarator);

		if (Consume(TokenKind::left_round))
		{
			if (IsTokenTypename())
			{
				return ParseDeclaratorTailFunction(abstract_declarator, true);
			}
			else
			{
				TokenPtr start = current_token;
				QualifiedType dummy{};
				ParseAbstractDeclarator(decl_spec, dummy);
				Expect(TokenKind::right_round);
				ParseDeclaratorTail(abstract_declarator, true);
				TokenPtr end = current_token;
				current_token = start;
				DeclarationSpecifier decl_spec2{ decl_spec };
				decl_spec2.qtype = abstract_declarator;
				ParseAbstractDeclarator(decl_spec2, abstract_declarator);
				current_token = end;
				return;
			}
		}
		else return ParseDeclaratorTail(abstract_declarator);
	}

	void Parser::ParseTypename(QualifiedType& type)
	{
		DeclarationSpecifier decl_spec{};
		ParseDeclarationSpecifier(decl_spec, true);
		ParseAbstractDeclarator(decl_spec, type);
	}

	//<pointer> :: = * { <type - qualifier> }* {<pointer>} ?
	//<type - qualifier> :: = const | volatile
	void Parser::ParsePointers(QualifiedType& type)
	{
		while (Consume(TokenKind::star))
		{
			PointerType ptr_type(type);
			type.SetRawType(ptr_type);
			type.RemoveQualifiers();
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
	}

	// func-params = ("void" | param ("," param)* ("," "...")?)? ")"
	// param = declspec declarator
	// array-dimensions = "["("static" | "restrict")* const-expr? "]" type-suffix
	void Parser::ParseDeclaratorTail(QualifiedType& type, bool abstract)
	{
		if (current_token->GetKind() == TokenKind::left_round) ParseDeclaratorTailFunction(type, abstract);
		else if (current_token->GetKind() == TokenKind::left_square) ParseDeclaratorTailArray(type, abstract);
	}

	void Parser::ParseDeclaratorTailFunction(QualifiedType& type, bool abstract)
	{
		Expect(TokenKind::left_round);
		if (Consume(TokenKind::KW_void))
		{
			FunctionType func_type(type);
			type.SetRawType(func_type);
			if (!Consume(TokenKind::right_round)) Diag(function_params_not_closed);
		}
		else
		{
			bool is_variadic = false;
			std::vector<FunctionParameter> param_types{};
			while (!Consume(TokenKind::right_round))
			{
				if (!param_types.empty() && !Consume(TokenKind::comma)) Diag(function_params_missing_coma);

				if (Consume(TokenKind::ellipsis))
				{
					is_variadic = true;
					if (!Consume(TokenKind::right_round)) Diag(variadic_params_not_last);
					else break;
				}

				DeclarationSpecifier param_decl_spec{};
				ParseDeclarationSpecifier(param_decl_spec);
				DeclaratorInfo param_declarator{};
				abstract ? ParseAbstractDeclarator(param_decl_spec, param_declarator.qtype) : ParseDeclarator(param_decl_spec, param_declarator);
				QualifiedType& qtype = param_declarator.qtype;
				if (qtype->Is(TypeKind::Void)) Diag(void_not_first_and_only_parameter);
				else if (qtype->Is(TypeKind::Array))
				{
					ArrayType const& array_type = type_cast<ArrayType>(*qtype);
					QualifiedType base_type = array_type.GetElementType();
					PointerType decayed_param_type(base_type);
					qtype = QualifiedType(decayed_param_type);
				}
				else if (qtype->Is(TypeKind::Function))
				{
					FunctionType const& function_type = type_cast<FunctionType>(*qtype);
					PointerType decayed_param_type(function_type);
					qtype = QualifiedType(decayed_param_type);
				}
				param_types.emplace_back(param_declarator.name, param_declarator.qtype);
			}
			FunctionType func_type(type, param_types, is_variadic);
			type.SetRawType(func_type);
		}
	}

	void Parser::ParseDeclaratorTailArray(QualifiedType& type, bool abstract)
	{
		Expect(TokenKind::left_square);
		while (Consume(TokenKind::KW_static, TokenKind::KW_restrict));
		if (Consume(TokenKind::right_square))
		{
			ArrayType arr_type(type);
			type.SetRawType(arr_type);
			return ParseDeclaratorTail(type, abstract);
		}
		else
		{
			UniqueExprPtr dimensions_expr = ParseExpression();
			if (!dimensions_expr->IsConstexpr()) Diag(array_dimensions_not_constexpr);
			int64 array_size = dimensions_expr->EvaluateConstexpr();
			if (array_size == 0) Diag(zero_size_array_not_allowed);

			ArrayType arr_type(type, (uint32)array_size);
			type.SetRawType(arr_type);
			if (!Consume(TokenKind::right_square)) Diag(array_brackets_not_closed);
			else return ParseDeclaratorTail(type, abstract);
		}
	}

	bool Parser::IsTokenTypename(uint32 offset) const
	{
		if ((current_token + offset)->Is(TokenKind::identifier))
		{
			DeclSymbol* sym = ctx.decl_scope_stack->LookUp((current_token + offset)->GetIdentifier());
			return sym ? sym->storage == Storage::Typedef : false;
		}
		return (current_token + offset)->IsDeclSpec();
	}
}