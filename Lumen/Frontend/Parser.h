#pragma once
#include <vector>
#include <memory>
#include "Token.h"


namespace lucc
{
	struct AST;
	class DeclAST;
	class TypedefDeclAST;
	class FunctionDeclAST;
	class QualifiedType;
	class ScopeStack;

	class Parser
	{
		struct DeclSpecInfo;
		struct DeclaratorInfo;
		struct DeclarationInfo;

		using TokenPtr = std::vector<Token>::iterator;
	public:

		explicit Parser(std::vector<Token> const& tokens);
		~Parser();
		bool Parse();
		AST* GetAST() const { return ast.get(); }

	private:
		std::vector<Token> tokens;
		TokenPtr current_token;
		std::unique_ptr<AST> ast;
		std::unique_ptr<ScopeStack> scope_stack;

	private:
		[[nodiscard]] bool Consume(TokenKind k)
		{
			if (current_token->Is(k))
			{
				++current_token; return true;
			}
			else return false;
		}
		template<typename... Ts>
		[[nodiscard]] bool Consume(TokenKind k, Ts... ts)
		{
			if (current_token->IsOneOf(k, ts...))
			{
				++current_token; return true;
			}
			else return false;
		}

		[[nodiscard]] bool ParseTranslationUnit();
		[[nodiscard]] bool ParseExternalDeclaration();
		[[nodiscard]] std::vector<std::unique_ptr<TypedefDeclAST>> ParseTypedefDeclaration(DeclSpecInfo const& decl_spec);
		[[nodiscard]] std::unique_ptr<FunctionDeclAST> ParseFunctionDeclaration(DeclarationInfo const& declaration);

		bool ParseDeclSpec(DeclSpecInfo& decl_spec, bool forbid_storage_specs = false);
		bool ParseDeclarator(DeclSpecInfo const& decl_spec, DeclaratorInfo& declarator);

		bool ParsePointers(QualifiedType& type);
		bool ParseTypeSuffix(QualifiedType& type);
	};
}