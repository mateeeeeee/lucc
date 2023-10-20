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

		[[nodiscard]] void ParseTranslationUnit();

		[[nodiscard]] UniqueDeclPtrList ParseDeclaration();
		[[nodiscard]] UniqueTypedefDeclPtrList ParseTypedefDeclaration(DeclarationSpecifier const& decl_spec);
		[[nodiscard]] UniqueFunctionDeclPtr ParseFunctionDeclaration(DeclarationInfo const& decl_spec);

		[[nodiscard]] UniqueStmtPtr ParseStatement();
		[[nodiscard]] UniqueExprStmtPtr ParseExpressionStatement();
		[[nodiscard]] UniqueCompoundStmtPtr ParseCompoundStatement();
		[[nodiscard]] UniqueReturnStmtPtr ParseReturnStatement();
		[[nodiscard]] UniqueIfStmtPtr ParseIfStatement();
		[[nodiscard]] UniqueWhileStmtPtr ParseWhileStatement();
		[[nodiscard]] UniqueDoWhileStmtPtr ParseDoWhileStatement();
		[[nodiscard]] UniqueForStmtPtr ParseForStatement();
		[[nodiscard]] UniqueBreakStmtPtr ParseBreakStatement();
		[[nodiscard]] UniqueContinueStmtPtr ParseContinueStatement();
		[[nodiscard]] UniqueLabelStmtPtr ParseLabelStatement();
		[[nodiscard]] UniqueGotoStmtPtr ParseGotoStatement();
		[[nodiscard]] UniqueSwitchStmtPtr ParseSwitchStatement();
		[[nodiscard]] UniqueCaseStmtPtr ParseCaseStatement();

		[[nodiscard]] UniqueExprPtr ParseInitializer(bool static_init);
		[[nodiscard]] UniqueExprPtr ParseExpression();
		[[nodiscard]] UniqueExprPtr ParseParenthesizedExpression();
		[[nodiscard]] UniqueExprPtr ParseAssignmentExpression();
		[[nodiscard]] UniqueExprPtr ParseConditionalExpression();
		[[nodiscard]] UniqueExprPtr ParseLogicalOrExpression();
		[[nodiscard]] UniqueExprPtr ParseLogicalAndExpression();
		[[nodiscard]] UniqueExprPtr ParseInclusiveOrExpression();
		[[nodiscard]] UniqueExprPtr ParseExclusiveOrExpression();
		[[nodiscard]] UniqueExprPtr ParseAndExpression();
		[[nodiscard]] UniqueExprPtr ParseEqualityExpression();
		[[nodiscard]] UniqueExprPtr ParseRelationalExpression();
		[[nodiscard]] UniqueExprPtr ParseShiftExpression();
		[[nodiscard]] UniqueExprPtr ParseAdditiveExpression();
		[[nodiscard]] UniqueExprPtr ParseMultiplicativeExpression();
		[[nodiscard]] UniqueExprPtr ParseCastExpression();
		[[nodiscard]] UniqueExprPtr ParseUnaryExpression();
		[[nodiscard]] UniqueExprPtr ParsePostFixExpression();
		[[nodiscard]] UniqueExprPtr ParseSizeofExpression();
		[[nodiscard]] UniqueIntLiteralPtr ParseAlignofExpression();
		[[nodiscard]] UniqueIntLiteralPtr ParseAlignasExpression();
		[[nodiscard]] UniqueExprPtr ParsePrimaryExpression();
		[[nodiscard]] UniqueIntLiteralPtr ParseIntegerLiteral();
		[[nodiscard]] UniqueStringLiteralPtr ParseStringLiteral();
		[[nodiscard]] UniqueExprPtr ParseIdentifier();

		template<ExprParseFn ParseFn, TokenKind token_kind, BinaryExprKind op_kind>
		[[nodiscard]] UniqueExprPtr ParseBinaryExpression();

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