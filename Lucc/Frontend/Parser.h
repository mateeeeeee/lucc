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
	class QualifiedType;
	class FunctionType;

	struct AST;
	class DeclAST;
	class TypedefDeclAST;
	class FunctionDeclAST;
	class ExprAST;
	class StmtAST;
	class ExprStmtAST;
	class CompoundStmtAST;
	class IfStmtAST;
	class WhileStmtAST;
	class ForStmtAST;
	class ReturnStmtAST;
	class GotoStmtAST;
	class LabelStmtAST;
	class IntLiteralAST;
	class StringLiteralAST;
	class IdentifierAST;
	class Parser;

	using ExprParseFn = std::unique_ptr<ExprAST>(Parser::*)();

	class Parser
	{
		struct DeclSpecInfo;
		struct DeclaratorInfo;
		struct DeclarationInfo;
		using TokenPtr = std::vector<Token>::iterator;

		struct Context
		{
			std::unique_ptr<SymbolTable> identifier_sym_table;
			FunctionType const* current_func_type = nullptr;
			bool return_stmt_encountered = false;
		};

	public:

		explicit Parser(std::vector<Token> const& tokens);
		~Parser();
		void Parse();
		AST* GetAST() const { return ast.get(); }

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
				Report(diag::unexpected_token);
				return false;
			}
			return true;
		}
		void Report(diag::Code);

		[[nodiscard]] void ParseTranslationUnit();

		[[nodiscard]] std::vector<std::unique_ptr<DeclAST>> ParseDeclaration();
		[[nodiscard]] std::vector<std::unique_ptr<TypedefDeclAST>> ParseTypedefDeclaration(DeclSpecInfo const& decl_spec);
		[[nodiscard]] std::unique_ptr<FunctionDeclAST> ParseFunctionDeclaration(DeclarationInfo const& decl_spec);

		[[nodiscard]] std::unique_ptr<StmtAST> ParseStatement();
		[[nodiscard]] std::unique_ptr<ExprStmtAST> ParseExpressionStatement();
		[[nodiscard]] std::unique_ptr<CompoundStmtAST> ParseCompoundStatement();
		[[nodiscard]] std::unique_ptr<IfStmtAST> ParseIfStatement();
		[[nodiscard]] std::unique_ptr<WhileStmtAST> ParseWhileStatement();
		[[nodiscard]] std::unique_ptr<ForStmtAST> ParseForStatement();
		[[nodiscard]] std::unique_ptr<ReturnStmtAST> ParseReturnStatement();
		[[nodiscard]] std::unique_ptr<LabelStmtAST> ParseLabelStatement();
		[[nodiscard]] std::unique_ptr<GotoStmtAST> ParseGotoStatement();

		[[nodiscard]] std::unique_ptr<ExprAST> ParseExpression();
		[[nodiscard]] std::unique_ptr<ExprAST> ParseParenthesizedExpression();
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
		[[nodiscard]] std::unique_ptr<IntLiteralAST> ParseIntegerLiteral();
		[[nodiscard]] std::unique_ptr<StringLiteralAST> ParseStringLiteral();
		[[nodiscard]] std::unique_ptr<IdentifierAST> ParseIdentifier();

		template<ExprParseFn ParseFn, TokenKind token_kind, BinaryExprKind op_kind>
		std::unique_ptr<ExprAST> ParseBinaryExpression();

		bool ParseDeclSpec(DeclSpecInfo& decl_spec, bool forbid_storage_specs = false);
		bool ParseDeclarator(DeclSpecInfo const& decl_spec, DeclaratorInfo& declarator);

		bool ParsePointers(QualifiedType& type);
		bool ParseTypeSuffix(QualifiedType& type);

		bool IsType(uint32 offset = 0) const;
	};
	

}