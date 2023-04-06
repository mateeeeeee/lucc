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
	public:
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
	class ParamVarDeclAST : public DeclAST
	{
	public:
		ParamVarDeclAST(QualifiedType const& type, std::string_view id)
			: type(type), identifier(id)
		{}
	private:
		QualifiedType type;
		std::string identifier;
	};
	class FieldDeclAST : public DeclAST
	{
	public:
		FieldDeclAST(QualifiedType const& type, std::string_view id)
			: type(type), identifier(id)
		{}
	private:
		QualifiedType type;
		std::string identifier;
	};
	class CompoundStmtAST : public StmtAST
	{
	public:
		CompoundStmtAST() = default;

		void AddStatement(std::unique_ptr<StmtAST>&& stmt)
		{
			statements.push_back(std::move(stmt));
		}

		virtual void Visit(NodeASTVisitor& visitor) const override
		{
			for (auto&& stmt : statements) stmt->Visit(visitor);
		}
	private:
		std::vector<std::unique_ptr<StmtAST>> statements;
	};
	class RecordDeclAST : public DeclAST
	{
	public:
		RecordDeclAST() = default;

		void AddField(std::unique_ptr<FieldDeclAST>&& field)
		{
			fields.push_back(std::move(field));
		}

		virtual void Visit(NodeASTVisitor& visitor) const override
		{
			for (auto&& field : fields) field->Visit(visitor);
		}
	private:
		std::vector<std::unique_ptr<FieldDeclAST>> fields;
	};
	class FunctionDeclAST : public DeclAST
	{
	public:
		FunctionDeclAST() = default;

		void AddBody(std::unique_ptr<CompoundStmtAST>&& body)
		{
			func_body = std::move(body);
		}

		virtual void Visit(NodeASTVisitor& visitor) const override
		{
			for (auto&& param : param_decls) param->Visit(visitor);
			func_body->Visit(visitor);
		}
	private:
		std::vector<std::unique_ptr<ParamVarDeclAST>> param_decls;
		std::unique_ptr<CompoundStmtAST> func_body;
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
