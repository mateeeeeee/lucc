#pragma once
#include <functional>
#include "Token.h"
#include "Scope.h"
#include "Diagnostics.h"
#include "ASTTypeAliases.h"

namespace lucc
{
	enum class BinaryExprKind : uint8;

	template<typename>
	class ScopeStack;
	class QualifiedType;
	class FunctionType;
	class Parser;

	using ExprParseFn = std::unique_ptr<Expr>(Parser::*)();

	class Parser
	{
		struct DeclarationSpecifier;
		struct DeclaratorInfo;
		struct DeclarationInfo;
		using TokenPtr = std::vector<Token>::iterator;
		using BreakCallbackFn = std::function<void(BreakStmt*)>;
		using ContinueCallbackFn = std::function<void(ContinueStmt*)>;

		struct Context
		{
			std::unique_ptr<ScopeStack<DeclSymbol>> decl_scope_stack;
			std::unique_ptr<ScopeStack<TagSymbol>> tag_scope_stack;

			FunctionType const* current_func_type = nullptr;
			bool return_stmt_encountered = false;

			std::vector<std::string> gotos;
			std::vector<std::string> labels;

			std::vector<BreakCallbackFn> break_callback_stack;
			std::vector<ContinueCallbackFn> continue_callback_stack;
			SwitchStmtPtrList switch_stack;
		};

	public:

		explicit Parser(std::vector<Token> const& tokens);
		~Parser();
		void Parse();
		AST const* GetAST() const { return ast.get(); }

	private:
		std::vector<Token> tokens;
		TokenPtr current_token;
		std::unique_ptr<AST> ast;
		Context ctx;

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
				g_Diagnostics.Report(unexpected_token);
				return false;
			}
			return true;
		}
		void Diag(DiagCode);

		void ParseTranslationUnit();

		LU_NODISCARD UniqueDeclPtrList ParseDeclaration();
		LU_NODISCARD UniqueTypedefDeclPtrList ParseTypedefDeclaration(DeclarationSpecifier const& decl_spec);
		LU_NODISCARD UniqueFunctionDeclPtr ParseFunctionDeclaration(DeclarationInfo const& decl_spec);

		LU_NODISCARD UniqueStmtPtr ParseStatement();
		LU_NODISCARD UniqueExprStmtPtr ParseExpressionStatement();
		LU_NODISCARD UniqueCompoundStmtPtr ParseCompoundStatement();
		LU_NODISCARD UniqueReturnStmtPtr ParseReturnStatement();
		LU_NODISCARD UniqueIfStmtPtr ParseIfStatement();
		LU_NODISCARD UniqueWhileStmtPtr ParseWhileStatement();
		LU_NODISCARD UniqueDoWhileStmtPtr ParseDoWhileStatement();
		LU_NODISCARD UniqueForStmtPtr ParseForStatement();
		LU_NODISCARD UniqueBreakStmtPtr ParseBreakStatement();
		LU_NODISCARD UniqueContinueStmtPtr ParseContinueStatement();
		LU_NODISCARD UniqueLabelStmtPtr ParseLabelStatement();
		LU_NODISCARD UniqueGotoStmtPtr ParseGotoStatement();
		LU_NODISCARD UniqueSwitchStmtPtr ParseSwitchStatement();
		LU_NODISCARD UniqueCaseStmtPtr ParseCaseStatement();

		LU_NODISCARD UniqueExprPtr ParseInitializer(bool static_init);
		LU_NODISCARD UniqueExprPtr ParseExpression();
		LU_NODISCARD UniqueExprPtr ParseParenthesizedExpression();
		LU_NODISCARD UniqueExprPtr ParseAssignmentExpression();
		LU_NODISCARD UniqueExprPtr ParseConditionalExpression();
		LU_NODISCARD UniqueExprPtr ParseLogicalOrExpression();
		LU_NODISCARD UniqueExprPtr ParseLogicalAndExpression();
		LU_NODISCARD UniqueExprPtr ParseInclusiveOrExpression();
		LU_NODISCARD UniqueExprPtr ParseExclusiveOrExpression();
		LU_NODISCARD UniqueExprPtr ParseAndExpression();
		LU_NODISCARD UniqueExprPtr ParseEqualityExpression();
		LU_NODISCARD UniqueExprPtr ParseRelationalExpression();
		LU_NODISCARD UniqueExprPtr ParseShiftExpression();
		LU_NODISCARD UniqueExprPtr ParseAdditiveExpression();
		LU_NODISCARD UniqueExprPtr ParseMultiplicativeExpression();
		LU_NODISCARD UniqueExprPtr ParseCastExpression();
		LU_NODISCARD UniqueExprPtr ParseUnaryExpression();
		LU_NODISCARD UniqueExprPtr ParsePostFixExpression();
		LU_NODISCARD UniqueExprPtr ParseSizeofExpression();
		LU_NODISCARD UniqueIntLiteralPtr ParseAlignofExpression();
		LU_NODISCARD UniqueIntLiteralPtr ParseAlignasExpression();
		LU_NODISCARD UniqueExprPtr ParsePrimaryExpression();
		LU_NODISCARD UniqueIntLiteralPtr ParseIntegerLiteral();
		LU_NODISCARD UniqueStringLiteralPtr ParseStringLiteral();
		LU_NODISCARD UniqueExprPtr ParseIdentifier();

		template<ExprParseFn ParseFn, TokenKind token_kind, BinaryExprKind op_kind>
		LU_NODISCARD UniqueExprPtr ParseBinaryExpression();

		void ParseEnum(DeclarationSpecifier& decl_spec);
		void ParseStruct(DeclarationSpecifier& decl_spec);
		void ParseStructMembers(DeclarationSpecifier& decl_spec);

		void ParseDeclarationSpecifier(DeclarationSpecifier& decl_spec, bool forbid_storage_specs = false);
		void ParseDeclarator(DeclarationSpecifier const& decl_spec, DeclaratorInfo& declarator);
		void ParseAbstractDeclarator(DeclarationSpecifier const& decl_spec, QualifiedType& abstract_declarator);
		void ParseTypename(QualifiedType& type);

		void ParsePointers(QualifiedType& type);
		void ParseDeclaratorTail(QualifiedType& type, bool abstract = false);
		void ParseDeclaratorTailFunction(QualifiedType& type, bool abstract = false);
		void ParseDeclaratorTailArray(QualifiedType& type, bool abstract = false);

		bool IsTokenTypename(uint32 offset = 0) const;
	};


}