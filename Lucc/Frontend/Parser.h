#pragma once
#include <vector>
#include <memory>
#include <functional>
#include "Token.h"
#include "Scope.h"
#include "Diagnostics.h"
#include "ForwardAST.h"

namespace lucc
{
	namespace diag
	{
		enum DiagCode : uint32;
	}
	enum class BinaryExprKind : uint8;

	template<typename>
	class ScopeStack;
	class QualifiedType;
	class FunctionType;
	class Parser;
	class Diagnostics;

	using ExprParseFn = std::unique_ptr<ExprAST>(Parser::*)();

	class Parser
	{
		struct DeclarationSpecifier;
		struct DeclaratorInfo;
		struct DeclarationInfo;
		using TokenPtr = std::vector<Token>::iterator;
		using BreakCallbackFn = std::function<void(BreakStmtAST*)>;
		using ContinueCallbackFn = std::function<void(ContinueStmtAST*)>;

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
			std::vector<SwitchStmtAST*> switch_stack;
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

		[[nodiscard]] std::vector<std::unique_ptr<DeclAST>> ParseDeclaration();
		[[nodiscard]] std::vector<std::unique_ptr<TypedefDeclAST>> ParseTypedefDeclaration(DeclarationSpecifier const& decl_spec);
		[[nodiscard]] std::unique_ptr<FunctionDeclAST> ParseFunctionDeclaration(DeclarationInfo const& decl_spec);

		[[nodiscard]] std::unique_ptr<StmtAST> ParseStatement();
		[[nodiscard]] std::unique_ptr<ExprStmtAST> ParseExpressionStatement();
		[[nodiscard]] std::unique_ptr<CompoundStmtAST> ParseCompoundStatement();
		[[nodiscard]] std::unique_ptr<IfStmtAST> ParseIfStatement();
		[[nodiscard]] std::unique_ptr<WhileStmtAST> ParseWhileStatement();
		[[nodiscard]] std::unique_ptr<DoWhileStmtAST> ParseDoWhileStatement();
		[[nodiscard]] std::unique_ptr<ForStmtAST> ParseForStatement();
		[[nodiscard]] std::unique_ptr<ReturnStmtAST> ParseReturnStatement();
		[[nodiscard]] std::unique_ptr<LabelStmtAST> ParseLabelStatement();
		[[nodiscard]] std::unique_ptr<BreakStmtAST> ParseBreakStatement();
		[[nodiscard]] std::unique_ptr<ContinueStmtAST> ParseContinueStatement();
		[[nodiscard]] std::unique_ptr<GotoStmtAST> ParseGotoStatement();
		[[nodiscard]] std::unique_ptr<SwitchStmtAST> ParseSwitchStatement();
		[[nodiscard]] std::unique_ptr<CaseStmtAST> ParseCaseStatement();

		[[nodiscard]] std::unique_ptr<ExprAST> ParseInitializer(bool static_init);
		[[nodiscard]] std::unique_ptr<ExprAST> ParseExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseParenthesizedExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseAssignmentExpression();
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
		[[nodiscard]] std::unique_ptr<IntLiteralAST> ParseAlignofExpression();
		[[nodiscard]] std::unique_ptr<IntLiteralAST> ParseAlignasExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParsePrimaryExpression();
		[[nodiscard]] std::unique_ptr<IntLiteralAST> ParseIntegerLiteral();
		[[nodiscard]] std::unique_ptr<StringLiteralAST> ParseStringLiteral();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseIdentifier();

		template<ExprParseFn ParseFn, TokenKind token_kind, BinaryExprKind op_kind>
		[[nodiscard]] std::unique_ptr<ExprAST> ParseBinaryExpression();

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