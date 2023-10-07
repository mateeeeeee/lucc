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
		ctx.identifier_sym_table = std::make_unique<SymbolTable<VarSymbol>>();
		ctx.tag_sym_table = std::make_unique<SymbolTable<TagSymbol>>();
		ParseTranslationUnit();
	}
	bool Parser::Expect(TokenKind k)
	{
		if (!Consume(k))
		{
			Diag(diag::unexpected_token);
			return false;
		}
		return true;
	}
	void Parser::Diag(diag::DiagCode code)
	{
		--current_token;
		diag::Diag(code, current_token->GetLocation());
		++current_token;
	}

	void Parser::ParseTranslationUnit()
	{
		while (current_token->IsNot(TokenKind::eof))
		{
			auto declarations = ParseDeclaration();
			for(auto&& declaration : declarations) ast->translation_unit->AddDeclaration(std::move(declaration));
		}
	}

	std::vector<std::unique_ptr<DeclAST>> Parser::ParseDeclaration()
	{
		while (Consume(TokenKind::semicolon)) Diag(diag::empty_statement);
		bool is_global = ctx.identifier_sym_table->IsGlobal();

		std::vector<std::unique_ptr<DeclAST>> decls;

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
				else Diag(diag::alignas_cannot_reduce_default_align);
			}

			LU_ASSERT(declarator_info.qtype.HasRawType());

			DeclarationInfo declaration_info(decl_spec, declarator_info);
			VarSymbol var_symbol{ declaration_info.name, declaration_info.qtype, declaration_info.storage, is_global };

			bool check_redefinition = false;
			if (VarSymbol* sym = ctx.identifier_sym_table->LookUpCurrentScope(declaration_info.name))
			{
				if (!IsFunctionType(declaration_info.qtype))
				{
					Diag(diag::redefinition_of_identifier);
					return {};
				}
				if (IsFunctionType(sym->qtype))
				{
					FunctionType& func_type = sym->qtype->As<FunctionType>();
					if (!func_type.IsCompatible(declaration_info.qtype))
					{
						Diag(diag::redefinition_of_identifier);
						return {};
					}
					if (func_type.HasDefinition())
					{
						check_redefinition = true;
					}
				}
				else
				{
					Diag(diag::redefinition_of_identifier);
					return {};
				}
			}
			else
			{
				bool success = ctx.identifier_sym_table->Insert(var_symbol);
				LU_ASSERT(success);
			}

			if (IsFunctionType(declarator_info.qtype))
			{
				if (!is_global) Diag(diag::local_functions_not_allowed);

				std::unique_ptr<FunctionDeclAST> func_decl = ParseFunctionDeclaration(declaration_info);
				func_decl->SetLocation(current_token->GetLocation());
				func_decl->SetSymbol(&var_symbol);

				VarSymbol* sym = ctx.identifier_sym_table->LookUp(declaration_info.name);
				sym->decl_ast = func_decl.get();
				if (func_decl->IsDefinition())
				{
					if(check_redefinition) 
					{
						Diag(diag::redefinition_of_identifier);
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
				if (IsVoidType(declaration_info.qtype)) Diag(diag::void_not_expected);

				std::string_view name = declarator_info.name;
				std::unique_ptr<VariableDeclAST> var_decl = std::make_unique<VariableDeclAST>(name);
				var_decl->SetLocation(current_token->GetLocation());
				var_decl->SetSymbol(&var_symbol);

				VarSymbol* sym = ctx.identifier_sym_table->LookUp(declaration_info.name);
				sym->decl_ast = var_decl.get();
				if (Consume(TokenKind::equal))
				{
					std::unique_ptr<ExprAST> init_expr = ParseExpression();
					if (is_global && !IsFunctionPointerType(declarator_info.qtype) && init_expr->GetExprKind() != ExprKind::IntLiteral) Diag(diag::initializer_element_is_not_constant);
					std::unique_ptr<ExprAST> init_expr_casted = GetAssignExpr(std::move(init_expr), declaration_info.qtype);
					var_decl->SetInitExpression(std::move(init_expr_casted));
				}
				decls.push_back(std::move(var_decl));
			}
		} while (Consume(TokenKind::comma));
		Expect(TokenKind::semicolon);
		return decls;
	}

	std::vector<std::unique_ptr<TypedefDeclAST>> Parser::ParseTypedefDeclaration(DeclarationSpecifier const& decl_spec)
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
				Diag(diag::typedef_name_empty);
				return {};
			}
			typedefs.push_back(std::make_unique<TypedefDeclAST>(typedef_info.name));
			bool success = ctx.identifier_sym_table->Insert(VarSymbol{ typedef_info.name, typedef_info.qtype, Storage::Typedef });
			if (!success)
			{
				Diag(diag::redefinition_of_identifier);
				return {};
			}
		}
		return typedefs;
	}

	std::unique_ptr<FunctionDeclAST> Parser::ParseFunctionDeclaration(DeclarationInfo const& decl_info)
	{
		LU_ASSERT(ctx.identifier_sym_table->IsGlobal());
		SCOPED_SYMBOL_TABLE(ctx.identifier_sym_table);

		std::string_view func_name = decl_info.name;
		if (func_name.empty())
		{
			Diag(diag::missing_name);
			return nullptr;
		}

		FunctionType const& func_type = TypeCast<FunctionType>(decl_info.qtype);
		std::unique_ptr<FunctionDeclAST> func_decl = std::make_unique<FunctionDeclAST>(func_name);
		for (auto&& func_param : func_type.GetParamTypes())
		{
			bool success = ctx.identifier_sym_table->Insert(VarSymbol{ func_param.name, func_param.qtype, Storage::None });
			if (!success)
			{
				Diag(diag::redefinition_of_identifier);
				return nullptr;
			}
			std::unique_ptr<VariableDeclAST> param_decl = std::make_unique<VariableDeclAST>(func_param.name);
			VarSymbol* sym = ctx.identifier_sym_table->LookUp(func_param.name);
			param_decl->SetSymbol(sym);
			sym->decl_ast = param_decl.get();
			func_decl->AddParamDeclaration(std::move(param_decl));
		}
		func_type.EncounterPrototype();
		if (current_token->Is(TokenKind::left_brace))
		{
			SCOPED_SYMBOL_TABLE(ctx.identifier_sym_table);
			ctx.current_func_type = &func_type;

			func_type.EncounteredDefinition();

			std::unique_ptr<CompoundStmtAST> compound_stmt = ParseCompoundStatement();
			func_decl->SetFunctionBody(std::move(compound_stmt));

			if (func_name != "main" && ctx.current_func_type->GetReturnType()->IsNot(TypeKind::Void) && !ctx.return_stmt_encountered)
			{
				Diag(diag::return_not_found);
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
			if (!found) Diag(diag::undeclared_label);
		}
		ctx.gotos.clear();
		ctx.labels.clear();

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
		SCOPED_SYMBOL_TABLE(ctx.identifier_sym_table);
		std::unique_ptr<CompoundStmtAST> compound_stmt = std::make_unique<CompoundStmtAST>();
		while (current_token->IsNot(TokenKind::right_brace))
		{
			if (IsTokenTypename())
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
		std::unique_ptr<IfStmtAST> if_stmt = std::make_unique<IfStmtAST>();
		if_stmt->SetCondition(ParseParenthesizedExpression());
		if_stmt->SetThenStatement(ParseStatement());
		if (Consume(TokenKind::KW_else)) if_stmt->SetElseStatement(ParseStatement());
		return if_stmt;
	}

	//<while - statement> ::= while (<expression>) < statement >
	std::unique_ptr<WhileStmtAST> Parser::ParseWhileStatement()
	{
		Expect(TokenKind::KW_while);
		std::unique_ptr<WhileStmtAST> while_stmt = std::make_unique<WhileStmtAST>();
		ctx.break_callback_stack.push_back([&](BreakStmtAST* break_stmt) { while_stmt->AddBreakStmt(break_stmt); });
		ctx.continue_callback_stack.push_back([&](ContinueStmtAST* continue_stmt) { while_stmt->AddContinueStmt(continue_stmt); });
		while_stmt->SetCondition(ParseParenthesizedExpression());
		while_stmt->SetBody(ParseStatement());
		ctx.continue_callback_stack.pop_back();
		ctx.break_callback_stack.pop_back();
		return while_stmt;
	}
	//<dowhile - statement> ::= do <statement> while ( <expression> ) ;
	std::unique_ptr<DoWhileStmtAST> Parser::ParseDoWhileStatement()
	{
		Expect(TokenKind::KW_do);
		std::unique_ptr<DoWhileStmtAST> dowhile_stmt = std::make_unique<DoWhileStmtAST>();
		ctx.break_callback_stack.push_back([&](BreakStmtAST* break_stmt) { dowhile_stmt->AddBreakStmt(break_stmt); });
		ctx.continue_callback_stack.push_back([&](ContinueStmtAST* continue_stmt) { dowhile_stmt->AddContinueStmt(continue_stmt); });
		dowhile_stmt->SetBody(ParseStatement());
		Expect(TokenKind::KW_while);
		dowhile_stmt->SetCondition(ParseParenthesizedExpression());
		Expect(TokenKind::semicolon);
		ctx.continue_callback_stack.pop_back();
		ctx.break_callback_stack.pop_back();
		return dowhile_stmt;
	}

	//<for - statement> ::= for ( {<init>}? ; {<expression>}? ; {<expression>}? ) <statement>
	std::unique_ptr<ForStmtAST> Parser::ParseForStatement()
	{
		Expect(TokenKind::KW_for);
		Expect(TokenKind::left_round);
		std::unique_ptr<ForStmtAST> for_stmt = std::make_unique<ForStmtAST>();

		std::unique_ptr<StmtAST> init = nullptr;
		if (IsTokenTypename())
		{
			std::vector<std::unique_ptr<DeclAST>> decl = ParseDeclaration();
			for_stmt->SetInit(std::make_unique<DeclStmtAST>(std::move(decl)));
		}
		else for_stmt->SetInit(ParseExpressionStatement());

		std::unique_ptr<ExprAST> cond_expr = nullptr;
		if (!Consume(TokenKind::semicolon))
		{
			for_stmt->SetCondition(ParseExpression());
			Expect(TokenKind::semicolon);
		}

		std::unique_ptr<ExprAST> iter_expr = nullptr;
		if (!Consume(TokenKind::right_round))
		{
			for_stmt->SetIterExpression(ParseExpression());
			Expect(TokenKind::right_round);
		}
		ctx.break_callback_stack.push_back([&](BreakStmtAST* break_stmt) { for_stmt->AddBreakStmt(break_stmt); });
		ctx.continue_callback_stack.push_back([&](ContinueStmtAST* continue_stmt) { for_stmt->AddContinueStmt(continue_stmt); });
		for_stmt->SetBody(ParseStatement());
		ctx.continue_callback_stack.pop_back();
		ctx.break_callback_stack.pop_back();
		return for_stmt;
	}

	//<return - statement> ::= return {<expression>}? ;
	std::unique_ptr<ReturnStmtAST> Parser::ParseReturnStatement()
	{
		LU_ASSERT(ctx.current_func_type != nullptr);
		Expect(TokenKind::KW_return);
		std::unique_ptr<ExprStmtAST> ret_expr_stmt = ParseExpressionStatement();
		ExprAST* ret_expr = ret_expr_stmt->GetExpr();
		QualifiedType ret_type = ctx.current_func_type->GetReturnType();
		ctx.return_stmt_encountered = true;
		std::unique_ptr<ExprStmtAST> ret_expr_cast = GetAssignExprStmt(std::move(ret_expr_stmt), ret_type);
		return std::make_unique<ReturnStmtAST>(std::move(ret_expr_cast));
	}

	std::unique_ptr<BreakStmtAST> Parser::ParseBreakStatement()
	{
		Expect(TokenKind::KW_break);
		Expect(TokenKind::semicolon);
		std::unique_ptr<BreakStmtAST> break_stmt = std::make_unique<BreakStmtAST>();
		if (ctx.break_callback_stack.empty()) Diag(diag::stray_break);
		else ctx.break_callback_stack.back()(break_stmt.get());
		return break_stmt;
	}

	std::unique_ptr<ContinueStmtAST> Parser::ParseContinueStatement()
	{
		Expect(TokenKind::KW_continue);
		Expect(TokenKind::semicolon);
		std::unique_ptr<ContinueStmtAST> continue_stmt = std::make_unique<ContinueStmtAST>();
		if (ctx.continue_callback_stack.empty()) Diag(diag::stray_continue);
		else ctx.continue_callback_stack.back()(continue_stmt.get());
		return continue_stmt;
	}

	std::unique_ptr<GotoStmtAST> Parser::ParseGotoStatement()
	{
		Expect(TokenKind::KW_goto);
		std::string_view label_name = current_token->GetIdentifier();
		Expect(TokenKind::identifier);
		Expect(TokenKind::semicolon);
		std::unique_ptr<GotoStmtAST> goto_stmt = std::make_unique<GotoStmtAST>(label_name);
		ctx.gotos.push_back(label_name.data());
		return goto_stmt;
	}

	std::unique_ptr<SwitchStmtAST> Parser::ParseSwitchStatement()
	{
		Expect(TokenKind::KW_switch);
		std::unique_ptr<SwitchStmtAST> switch_stmt = std::make_unique<SwitchStmtAST>();
		ctx.switch_stack.push_back(switch_stmt.get());
		ctx.break_callback_stack.push_back([&](BreakStmtAST* break_stmt) { switch_stmt->AddBreakStmt(break_stmt); });
		switch_stmt->SetCondition(ParseParenthesizedExpression());
		switch_stmt->SetBody(ParseStatement());
		ctx.break_callback_stack.pop_back();
		ctx.switch_stack.pop_back();
		return switch_stmt;
	}

	std::unique_ptr<CaseStmtAST> Parser::ParseCaseStatement()
	{
		if (ctx.switch_stack.empty()) Diag(diag::stray_case);

		std::unique_ptr<CaseStmtAST> case_stmt = nullptr;
		if (Consume(TokenKind::KW_default))
		{
			if (ctx.switch_stack.back()->HasDefaultCase()) Diag(diag::multiple_default_cases);
			case_stmt = std::make_unique<CaseStmtAST>();
		}
		else
		{
			Expect(TokenKind::KW_case);
			std::unique_ptr<ExprAST> case_value = ParseExpression();
			if (!case_value->IsConstexpr()) Diag(diag::case_value_not_constexpr);
			case_stmt = std::make_unique<CaseStmtAST>(case_value->EvaluateConstexpr());
		}
		Expect(TokenKind::colon);
		ctx.switch_stack.back()->AddCaseStatement(case_stmt.get());
		return case_stmt;
	}

	std::unique_ptr<LabelStmtAST> Parser::ParseLabelStatement()
	{
		std::string_view label_name = current_token->GetIdentifier();
		Expect(TokenKind::identifier);
		Expect(TokenKind::colon);
		std::unique_ptr<LabelStmtAST> label_stmt = std::make_unique<LabelStmtAST>(label_name);
		ctx.labels.push_back(label_name.data());
		return label_stmt;
	}

	//<initializer> ::= <assignment-expression>
	//				  | { <initializer - list> }
	//				  | { <initializer - list>, }
	//<initializer - list> :: = <initializer>
	//						  | <initializer - list>, <initializer>
	std::unique_ptr<ExprAST> Parser::ParseInitializer(bool static_init)
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
	std::unique_ptr<ExprAST> Parser::ParseBinaryExpression()
	{
		std::unique_ptr<ExprAST> lhs = (this->*ParseFn)();
		while (Consume(token_kind))
		{
			SourceLocation loc = current_token->GetLocation();
			std::unique_ptr<ExprAST> rhs = (this->*ParseFn)();
			std::unique_ptr<BinaryExprAST> parent = std::make_unique<BinaryExprAST>(op_kind, loc);
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
		return ParseBinaryExpression<&Parser::ParseAssignmentExpression, TokenKind::comma, BinaryExprKind::Comma>();
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
	std::unique_ptr<ExprAST> Parser::ParseAssignmentExpression()
	{
		TokenPtr current_token_copy = current_token;
		std::unique_ptr<ExprAST> lhs = ParseConditionalExpression();
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
			Diag(diag::lhs_not_assignable);
			return nullptr;
		}
		++current_token;
		std::unique_ptr<ExprAST> rhs = ParseAssignmentExpression();
		if (arith_op_kind != BinaryExprKind::Assign)
		{
			//#todo: remove this hack, add clone to ast nodes
			TokenPtr current_token_copy2 = current_token;
			current_token = current_token_copy;
			std::unique_ptr<ExprAST> lhs_copy = ParseConditionalExpression();
			current_token = current_token_copy2;

			std::unique_ptr<BinaryExprAST> tmp = std::make_unique<BinaryExprAST>(arith_op_kind, loc);
			tmp->SetLHS(std::move(lhs_copy));
			tmp->SetRHS(std::move(rhs));

			std::unique_ptr<BinaryExprAST> parent = std::make_unique<BinaryExprAST>(BinaryExprKind::Assign, loc);
			parent->SetLHS(std::move(lhs));
			parent->SetRHS(std::move(tmp));
			return parent;
		}
		else
		{
			std::unique_ptr<BinaryExprAST> parent = std::make_unique<BinaryExprAST>(arith_op_kind, loc);
			parent->SetLHS(std::move(lhs));
			parent->SetRHS(std::move(rhs));
			return parent;
		}
	}

	//<conditional - expression> ::=  <logical - or -expression>
	//								| <logical - or -expression> ? <expression> : <conditional - expression>
	std::unique_ptr<ExprAST> Parser::ParseConditionalExpression()
	{
		SourceLocation loc = current_token->GetLocation();
		std::unique_ptr<ExprAST> cond = ParseLogicalOrExpression();
		if (Consume(TokenKind::question))
		{
			std::unique_ptr<TernaryExprAST> ternary_expr = std::make_unique<TernaryExprAST>(loc);
			ternary_expr->SetCondition(std::move(cond));
			ternary_expr->SetTrueExpr(ParseExpression());
			Expect(TokenKind::colon);
			ternary_expr->SetFalseExpr(ParseConditionalExpression());
			return ternary_expr;
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
			SourceLocation loc = current_token->GetLocation();
			switch (current_token->GetKind())
			{
			case TokenKind::equal_equal: op_kind = BinaryExprKind::Equal; break;
			case TokenKind::not_equal:	 op_kind = BinaryExprKind::NotEqual; break;
			default:
				return lhs;
			}
			++current_token;
			std::unique_ptr<ExprAST> rhs = ParseRelationalExpression();
			std::unique_ptr<BinaryExprAST> parent = std::make_unique<BinaryExprAST>(op_kind, loc);
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
			std::unique_ptr<ExprAST> rhs = ParseShiftExpression();
			std::unique_ptr<BinaryExprAST> parent = std::make_unique<BinaryExprAST>(op_kind, loc);
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
			SourceLocation loc = current_token->GetLocation();
			switch (current_token->GetKind())
			{
			case TokenKind::less_less:			op_kind = BinaryExprKind::ShiftLeft; break;
			case TokenKind::greater_greater:	op_kind = BinaryExprKind::ShiftRight; break;
			default:
				return lhs;
			}
			++current_token;
			std::unique_ptr<ExprAST> rhs = ParseAdditiveExpression();
			std::unique_ptr<BinaryExprAST> parent = std::make_unique<BinaryExprAST>(op_kind, loc);
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
			SourceLocation loc = current_token->GetLocation();
			switch (current_token->GetKind())
			{
			case TokenKind::plus:			op_kind = BinaryExprKind::Add; break;
			case TokenKind::minus:			op_kind = BinaryExprKind::Subtract; break;
			default:
				return lhs;
			}
			++current_token;
			std::unique_ptr<ExprAST> rhs = ParseMultiplicativeExpression();
			std::unique_ptr<BinaryExprAST> parent = std::make_unique<BinaryExprAST>(op_kind, loc);
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
			std::unique_ptr<ExprAST> rhs = ParseCastExpression();
			std::unique_ptr<BinaryExprAST> parent = std::make_unique<BinaryExprAST>(op_kind, loc);
			parent->SetLHS(std::move(lhs));
			parent->SetRHS(std::move(rhs));
			lhs = std::move(parent);
		}
	}

	//<cast - expression> :: = <unary - expression>
	//					   | (<type - name>) < cast - expression >
	std::unique_ptr<ExprAST> Parser::ParseCastExpression()
	{
		if (current_token->Is(TokenKind::left_round) && IsTokenTypename(1))
		{
			Expect(TokenKind::left_round);
			QualifiedType cast_type{};
			ParseTypename(cast_type);
			Expect(TokenKind::right_round);

			if (!IsVoidType(cast_type) && !IsScalarType(cast_type)) Diag(diag::cast_invalid_type);
			//#todo further check cast type compatibility
			SourceLocation loc = current_token->GetLocation();
			std::unique_ptr<CastExprAST> cast_expr = std::make_unique<CastExprAST>(loc, cast_type);
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
	std::unique_ptr<ExprAST> Parser::ParseUnaryExpression()
	{
		std::unique_ptr<UnaryExprAST> unary_expr;
		SourceLocation loc = current_token->GetLocation();
		switch (current_token->GetKind())
		{
		// C11 6.5.3.2p1: The operand of the unary & operator shall be either a function
		// designator, the result of a [] or unary * operator, or an lvalue that
		// designates an object that is not a bit-field and is not declared with the
		// register storage-class specifier.
		case TokenKind::amp:
		{
			unary_expr = std::make_unique<UnaryExprAST>(UnaryExprKind::AddressOf, loc);
			++current_token;
			std::unique_ptr<ExprAST> op_expr = ParseUnaryExpression();
			if (!op_expr->IsLValue() && !IsFunctionType(op_expr->GetType()))
			{
				Diag(diag::address_of_rvalue_error);
				return nullptr;
			}
			unary_expr->SetOperand(std::move(op_expr));
			return unary_expr;
		}
		case TokenKind::star:
		{
			unary_expr = std::make_unique<UnaryExprAST>(UnaryExprKind::Dereference, loc);
			++current_token;
			std::unique_ptr<ExprAST> op_expr = ParseUnaryExpression();
			if (!IsPointerLikeType(op_expr->GetType()))
			{
				Diag(diag::dereferencing_non_pointer_type);
				return nullptr;
			}
			unary_expr->SetOperand(std::move(op_expr));
			return unary_expr;
		}
		case TokenKind::plus_plus:
			unary_expr = std::make_unique<UnaryExprAST>(UnaryExprKind::PreIncrement, loc);
			break;
		case TokenKind::minus_minus:
			unary_expr = std::make_unique<UnaryExprAST>(UnaryExprKind::PreDecrement, loc);
			break;
		case TokenKind::plus:
			unary_expr = std::make_unique<UnaryExprAST>(UnaryExprKind::Plus, loc);
			break;
		case TokenKind::minus:
			unary_expr = std::make_unique<UnaryExprAST>(UnaryExprKind::Minus, loc);
			break;
		case TokenKind::tilde:
			unary_expr = std::make_unique<UnaryExprAST>(UnaryExprKind::BitNot, loc);
			break;
		case TokenKind::exclaim:
			unary_expr = std::make_unique<UnaryExprAST>(UnaryExprKind::LogicalNot, loc);
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
	std::unique_ptr<ExprAST> Parser::ParsePostFixExpression()
	{
		std::unique_ptr<ExprAST> expr = ParsePrimaryExpression();

		SourceLocation loc = current_token->GetLocation();
		switch (current_token->GetKind())
		{
		case TokenKind::left_round:
		{
			QualifiedType const& type = expr->GetType();
			if (!IsFunctionPointerType(type) && !IsFunctionType(type))
			{
				Diag(diag::invalid_function_call);
				return nullptr;
			}

			std::unique_ptr<FunctionCallExprAST> func_call_expr = std::make_unique<FunctionCallExprAST>(std::move(expr), current_token->GetLocation());
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
					std::unique_ptr<ExprAST> arg_expr = ParseAssignmentExpression();
					if (arg_index >= func_params.size())
					{
						if (!func_type->IsVariadic())
						{
							Diag(diag::invalid_function_call);
							return nullptr;
						}
						else variadic_args = true;
					}

					if (!variadic_args) arg_expr = GetAssignExpr(std::move(arg_expr), func_params[arg_index].qtype);
					func_call_expr->AddArgument(std::move(arg_expr));
					++arg_index;

					if (Consume(TokenKind::right_round)) break;
					Expect(TokenKind::comma);
				}
			}
			if (!func_type->IsVariadic() && func_params.size() != arg_index)
			{
				Diag(diag::invalid_function_call);
				return nullptr;
			}
			return func_call_expr;
		}
		case TokenKind::plus_plus:
		{
			++current_token;
			std::unique_ptr<UnaryExprAST> post_inc_expr = std::make_unique<UnaryExprAST>(UnaryExprKind::PostIncrement, loc);
			post_inc_expr->SetOperand(std::move(expr));
			return post_inc_expr;
		}
		case TokenKind::minus_minus:
		{
			++current_token;
			std::unique_ptr<UnaryExprAST> post_dec_expr = std::make_unique<UnaryExprAST>(UnaryExprKind::PostDecrement, loc);
			post_dec_expr->SetOperand(std::move(expr));
			return post_dec_expr;
		}
		case TokenKind::left_square:
		{
			++current_token;
			if (!IsArrayType(expr->GetType()) && !IsPointerType(expr->GetType()))
			{
				Diag(diag::dereferencing_non_pointer_type);
				return nullptr;
			}

			std::unique_ptr<ExprAST> bracket_expr = ParseExpression();
			Expect(TokenKind::right_square);

			std::unique_ptr<UnaryExprAST> dereference_expr = std::make_unique<UnaryExprAST>(UnaryExprKind::Dereference, loc);
			std::unique_ptr<BinaryExprAST> add_expr = std::make_unique<BinaryExprAST>(BinaryExprKind::Add, loc);
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
				Diag(diag::invalid_member_access);
				return nullptr;
			}
			std::string_view struct_name = current_token->GetIdentifier();
			VarSymbol* var = ctx.identifier_sym_table->LookUp(struct_name);
			if (!var || var->is_enum || !IsStructType(var->qtype))
			{
				Diag(diag::invalid_member_access);
				return nullptr;
			}
			DeclAST* decl_ast = var->decl_ast;

			current_token += 2;
			if (current_token->IsNot(TokenKind::identifier))
			{
				Diag(diag::invalid_member_access);
				return nullptr;
			}
			std::string_view member_name = current_token->GetIdentifier();
			++current_token;

			StructType const& struct_type = var->qtype->As<StructType>();
			if (!struct_type.HasMember(member_name))
			{
				Diag(diag::invalid_member_access);
				return nullptr;
			}
			std::unique_ptr<MemberRefExprAST> member_access_expr = std::make_unique<MemberRefExprAST>(decl_ast, member_name, loc);
			return member_access_expr;
		}
		case TokenKind::arrow:
		{
		//#todo
		}
		}
		return expr;
	}

	std::unique_ptr<ExprAST> Parser::ParseSizeofExpression()
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
					Diag(diag::sizeof_invalid_argument);
				}
				Expect(TokenKind::right_round);
				return std::make_unique<IntLiteralAST>(type->GetSize(), loc);
			}
			else
			{
				std::unique_ptr<ExprAST> sizeof_expr = ParseExpression();
				Expect(TokenKind::right_round);
				return std::make_unique<IntLiteralAST>(sizeof_expr->GetType()->GetSize(), loc);
			}
		}
		else
		{
			SourceLocation loc = current_token->GetLocation();
			std::unique_ptr<ExprAST> sizeof_expr = ParseExpression();
			return std::make_unique<IntLiteralAST>(sizeof_expr->GetType()->GetSize(), loc);
		}
	}

	/*
	Returns the alignment requirement of the type named by type-name.
	If type-name is an array type, the result is the alignment requirement of the array element type.
	The type-name cannot be function type or an incomplete type.
	The result is an integer constant of type size_t.
	The operand is not evaluated (so external identifiers used in the operand do not have to be defined).
	*/
	std::unique_ptr<IntLiteralAST> Parser::ParseAlignofExpression()
{
		Expect(TokenKind::KW__Alignof);
		Expect(TokenKind::left_round);

		if (!IsTokenTypename()) Diag(diag::alignof_expects_type_argument);
		SourceLocation loc = current_token->GetLocation();
		QualifiedType type{};
		ParseTypename(type);
		if (IsFunctionType(type) || !type->IsComplete()) Diag(diag::alignof_invalid_argument);
		std::unique_ptr<IntLiteralAST> alignof_expr = std::make_unique<IntLiteralAST>(type->GetAlign(), loc);
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
	std::unique_ptr<IntLiteralAST> Parser::ParseAlignasExpression()
	{
		Expect(TokenKind::KW__Alignas);
		Expect(TokenKind::left_round);
		SourceLocation loc = current_token->GetLocation();
		std::unique_ptr<IntLiteralAST> alignas_expr = nullptr;
		if (IsTokenTypename())
		{
			QualifiedType type{};
			ParseTypename(type);
			alignas_expr = std::make_unique<IntLiteralAST>(type->GetAlign(), loc);
		}
		else
		{
			auto IsPowerOfTwo = [](int64 n) {return n > 0 && ((n & (n - 1)) == 0); };
			std::unique_ptr<ExprAST> expr = ParseExpression();
			if (!expr->IsConstexpr()) Diag(diag::alignas_alignment_not_constexpr);
			int64 alignment = expr->EvaluateConstexpr();
			if (!IsPowerOfTwo(alignment)) Diag(diag::alignas_alignment_not_power_of_two);
			alignas_expr = std::make_unique<IntLiteralAST>(alignment, loc);
		}

		Expect(TokenKind::right_round);
		return alignas_expr;
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
			Diag(diag::unexpected_token);
		}
		LU_ASSERT(false);
		return nullptr;
	}

	std::unique_ptr<IntLiteralAST> Parser::ParseIntegerLiteral()
	{
		LU_ASSERT(current_token->Is(TokenKind::number));
		std::string_view string_number = current_token->GetIdentifier();
		int64 value = std::stoll(current_token->GetIdentifier().data(), nullptr, 0);
		SourceLocation loc = current_token->GetLocation();
		++current_token;
		return std::make_unique<IntLiteralAST>(value, loc);
	}

	std::unique_ptr<StringLiteralAST> Parser::ParseStringLiteral()
	{
		LU_ASSERT(current_token->Is(TokenKind::string_literal));
		std::string_view str = current_token->GetIdentifier();
		SourceLocation loc = current_token->GetLocation();
		++current_token;
		return std::make_unique<StringLiteralAST>(str, loc);
	}

	std::unique_ptr<ExprAST> Parser::ParseIdentifier()
	{
		LU_ASSERT(current_token->Is(TokenKind::identifier));
		std::string_view name = current_token->GetIdentifier();
		if (VarSymbol* sym = ctx.identifier_sym_table->LookUp(name))
		{
			if (sym->is_enum)
			{
				SourceLocation loc = current_token->GetLocation();
				++current_token;
				return std::make_unique<IntLiteralAST>(sym->enum_value, loc);
			}
			else
			{
				SourceLocation loc = current_token->GetLocation();
				++current_token;
				std::unique_ptr<DeclRefExprAST> decl_ref = std::make_unique<DeclRefExprAST>(sym->decl_ast, loc);
				return decl_ref;
			}
		}
		else Diag(diag::variable_not_declared);
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
			TagSymbol* sym = ctx.tag_sym_table->LookUp(enum_tag);
			if (!sym) Diag(diag::unknown_enum);
			else if (!sym->enum_type) Diag(diag::not_enum_type);
			decl_spec.qtype = sym->type;
			return;
		}

		Expect(TokenKind::left_brace);
		int32 val = 0;
		while (true)
		{
			std::string enum_value_name;
			if (current_token->IsNot(TokenKind::identifier)) Diag(diag::enum_value_no_name);
			enum_value_name = current_token->GetIdentifier(); ++current_token;

			if (Consume(TokenKind::equal))
			{
				std::unique_ptr<ExprAST> enum_value_expr = ParseAssignmentExpression();
				if (!enum_value_expr->IsConstexpr()) Diag(diag::enum_value_not_constexpr);
				val = (int32)enum_value_expr->EvaluateConstexpr();
			}

			ctx.identifier_sym_table->Insert(VarSymbol{ .name = enum_value_name, .is_enum = true, .enum_value = val++ });
			if (Consume(TokenKind::right_brace)) break;
			Expect(TokenKind::comma);
		}

		if (!enum_tag.empty()) ctx.tag_sym_table->Insert(TagSymbol{ enum_tag, decl_spec.qtype, true });
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
			TagSymbol* tag = ctx.tag_sym_table->LookUp(tag_name);
			if (tag)
			{
				decl_spec.qtype = tag->type;
				return;
			}

			decl_spec.qtype = StructType(tag_name);
			ctx.tag_sym_table->Insert(TagSymbol(tag_name, decl_spec.qtype, false));
			return;
		}

		Expect(TokenKind::left_brace);

		decl_spec.qtype = StructType(tag_name);
		ParseStructMembers(decl_spec);

		if (!tag_name.empty()) 
		{
			TagSymbol* tag = ctx.tag_sym_table->LookUpCurrentScope(tag_name);
			if (tag)
			{
				tag->type = decl_spec.qtype;
				return;
			}
			ctx.tag_sym_table->Insert(TagSymbol(tag_name, decl_spec.qtype, false));
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
		if (!success) Diag(diag::invalid_member_declaration);
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
				if (forbid_storage_specs) Diag(diag::storage_specifier_forbidden_context);

				TokenKind kind = current_token->GetKind();
				++current_token;
				if (decl_spec.storage != Storage::None) Diag(diag::multiple_storage_specifiers);

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
				if (forbid_storage_specs) Diag(diag::storage_specifier_forbidden_context);
				--current_token;
				std::unique_ptr<IntLiteralAST> alignas_expr = ParseAlignasExpression();
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
				VarSymbol* sym = ctx.identifier_sym_table->LookUp(current_token->GetIdentifier());
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
				Diag(diag::declarator_specifier_error);
			}
		}
		if (counter == 0) Diag(diag::declarator_specifier_error);
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
			if (!Consume(TokenKind::right_round)) Diag(diag::function_params_not_closed);
		}
		else
		{
			bool is_variadic = false;
			std::vector<FunctionParameter> param_types{};
			while (!Consume(TokenKind::right_round))
			{
				if (!param_types.empty() && !Consume(TokenKind::comma)) Diag(diag::function_params_missing_coma);

				if (Consume(TokenKind::ellipsis))
				{
					is_variadic = true;
					if (!Consume(TokenKind::right_round)) Diag(diag::variadic_params_not_last);
					else break;
				}

				DeclarationSpecifier param_decl_spec{};
				ParseDeclarationSpecifier(param_decl_spec);
				DeclaratorInfo param_declarator{};
				abstract ? ParseAbstractDeclarator(param_decl_spec, param_declarator.qtype) : ParseDeclarator(param_decl_spec, param_declarator);
				QualifiedType& qtype = param_declarator.qtype;
				if (qtype->Is(TypeKind::Void)) Diag(diag::void_not_first_and_only_parameter);
				else if (qtype->Is(TypeKind::Array))
				{
					ArrayType const& array_type = TypeCast<ArrayType>(*qtype);
					QualifiedType base_type = array_type.GetElementType();
					PointerType decayed_param_type(base_type);
					qtype = QualifiedType(decayed_param_type);
				}
				else if (qtype->Is(TypeKind::Function))
				{
					FunctionType const& function_type = TypeCast<FunctionType>(*qtype);
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
			std::unique_ptr<ExprAST> dimensions_expr = ParseExpression();
			if (!dimensions_expr->IsConstexpr()) Diag(diag::array_dimensions_not_constexpr);
			int64 array_size = dimensions_expr->EvaluateConstexpr();
			if (array_size == 0) Diag(diag::zero_size_array_not_allowed);

			ArrayType arr_type(type, (uint32)array_size);
			type.SetRawType(arr_type);
			if (!Consume(TokenKind::right_square)) Diag(diag::array_brackets_not_closed);
			else return ParseDeclaratorTail(type, abstract);
		}
	}

	bool Parser::IsTokenTypename(uint32 offset) const
	{
		if ((current_token + offset)->Is(TokenKind::identifier))
		{
			VarSymbol* sym = ctx.identifier_sym_table->LookUp((current_token + offset)->GetIdentifier());
			return sym ? sym->storage == Storage::Typedef : false;
		}
		return (current_token + offset)->IsDeclSpec();
	}
}