#pragma once
#include <vector>
#include <memory>
#include <string>
#include "Type.h"
#include "SourceLocation.h"

namespace lu
{
	class NodeAST;
	class NodeASTVisitor
	{
	public:
		virtual ~NodeASTVisitor() = 0;
	};

	class NodeAST
	{
	public:
		virtual ~NodeAST() = default;

		virtual void Visit(NodeASTVisitor& visitor) const = 0;
	protected:
		NodeAST() = default;
	};
	class DeclAST : public NodeAST
	{
	public:
		DeclAST() = default;

		virtual void Visit(NodeASTVisitor& visitor) const override {}
	};
	class StmtAST : public NodeAST
	{
		StmtAST() = default;

		virtual void Visit(NodeASTVisitor& visitor) const override {}
	};

	class TranslationUnitDeclAST : public DeclAST
	{
	public:
		TranslationUnitDeclAST() = default;

		void AddExternalDeclaration(std::unique_ptr<DeclAST>&& ext_decl)
		{
			external_declarations.push_back(std::move(ext_decl));
		}

		virtual void Visit(NodeASTVisitor& visitor) const override
		{
			for (auto&& decl : external_declarations) decl->Visit(visitor);
		}
	private:
		std::vector<std::unique_ptr<DeclAST>> external_declarations;
	};
	//declarations
	class VarDeclAST : public DeclAST
	{
	public:
		VarDeclAST(QualifiedType const& type, std::string_view id, std::unique_ptr<StmtAST>&& stmt = nullptr)
			: type(type), identifier(id), init_stmt(std::move(stmt))
		{}

	private:
		QualifiedType type;
		std::string identifier;
		std::unique_ptr<StmtAST> init_stmt;
	};

	class TypedefDeclAST : public DeclAST
	{
	public:
		TypedefDeclAST(QualifiedType const& type, std::string_view typealias)
			: type(type), typealias(typealias)
		{}

	private:
		QualifiedType type;
		std::string typealias;
	};

	//statements & expressions
	class ExprAST : public StmtAST
	{
	public:
		ExprAST() = default;

	private:
	};

	template<std::integral T>
	class IntegerLiteralAST : public ExprAST
	{
	public:
		IntegerLiteralAST(T value, QualifiedType const& type, SourceLocation const l) : value(value), type(type), loc(l)
		{}

	private:
		T value;
		QualifiedType type;
		SourceLocation loc;
	};

	struct AST
	{
		std::unique_ptr<TranslationUnitDeclAST> tr_unit;
	};
}