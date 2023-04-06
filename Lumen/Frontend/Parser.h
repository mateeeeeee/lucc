#pragma once
#include <vector>
#include <memory>
#include "Token.h"


namespace lu
{
	class ScopeStack;
	struct AST;
	class DeclAST;
	class TypedefDeclAST;
	class QualifiedType;

	class Parser
	{
		struct DeclSpecInfo;
		struct DeclaratorInfo;

		using TokenPtr = std::vector<Token>::iterator;
	public:

		explicit Parser(std::vector<Token> const& tokens);
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
		[[nodiscard]] std::vector<std::unique_ptr<TypedefDeclAST>> ParseTypedefDeclaration(DeclSpecInfo& decl_spec);

		bool ParseDeclSpec(DeclSpecInfo& decl_spec);
		bool ParseDeclarator(DeclSpecInfo const& decl_spec, DeclaratorInfo& declarator);

		void ParsePointers(QualifiedType& type);
		void ParseTypeSuffix(QualifiedType& type);
	};
}