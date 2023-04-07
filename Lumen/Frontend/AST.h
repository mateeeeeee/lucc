#pragma once
#include <vector>
#include <memory>
#include <string>
#include "Type.h"
#include "SourceLocation.h"

namespace lucc
{
	class NodeAST;
	class TranslationUnitDeclAST;
	class TypedefDeclAST;

	class NodeVisitorAST
	{
	public:
		virtual ~NodeVisitorAST() = default;
		virtual void Visit(TranslationUnitDeclAST const& node, size_t indent) = 0;
		virtual void Visit(TypedefDeclAST const& node, size_t indent) = 0;
		virtual void Visit(NodeAST const& node, size_t indent) = 0;
	};

	class NodeAST
	{
	public:
		virtual ~NodeAST() = default;
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const = 0;

	protected:
		NodeAST() = default;
	};

	class DeclAST : public NodeAST
	{
	public:
		DeclAST() = default;
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override 
		{
			
		}
	};

	class StmtAST : public NodeAST
	{
	public:
		StmtAST() = default;
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override 
		{
			
		}

	};

	class TranslationUnitDeclAST : public DeclAST
	{
	public:
		TranslationUnitDeclAST() = default;
		void AddExternalDeclaration(std::unique_ptr<DeclAST>&& ext_decl)
		{
			external_declarations.push_back(std::move(ext_decl));
		}
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override
		{
			visitor.Visit(*this, indent);
			for (auto&& decl : external_declarations) decl->Accept(visitor, indent + 1);
		}

	private:
		std::vector<std::unique_ptr<DeclAST>> external_declarations;
	};

	class VarDeclAST : public DeclAST
	{
	public:
		VarDeclAST(QualifiedType const& type, std::string_view id, std::unique_ptr<StmtAST>&& stmt = nullptr)
			: type(type), identifier(id), init_stmt(std::move(stmt))
		{}

		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override
		{
			visitor.Visit(*this, indent);
			init_stmt->Accept(visitor, indent + 1);
		}

	private:
		QualifiedType type;
		std::string identifier;
		std::unique_ptr<StmtAST> init_stmt;
	};

	class ParamVarDeclAST : public DeclAST
	{
	public:
		ParamVarDeclAST(FunctionParameter const& param) : param(param)
		{}
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override
		{
			visitor.Visit(*this, indent);
		}

	private:
		FunctionParameter param;
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
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override
		{
			visitor.Visit(*this, indent);
			for (auto&& stmt : statements) stmt->Accept(visitor, indent + 1);
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
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override
		{
			visitor.Visit(*this, indent);
			for (auto&& field : fields) field->Accept(visitor, indent + 1);
		}

	private:
		std::vector<std::unique_ptr<FieldDeclAST>> fields;
	};
	
	class FunctionDeclAST : public DeclAST
	{
	public:
		FunctionDeclAST() = default;
		void AddParamDeclaration(std::unique_ptr<ParamVarDeclAST>&& param)
		{
			param_decls.push_back(std::move(param));
		}
		void AddBody(std::unique_ptr<CompoundStmtAST>&& body)
		{
			func_body = std::move(body);
		}
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override
		{
			visitor.Visit(*this, indent);
			for (auto&& param : param_decls) param->Accept(visitor, indent + 1);
			func_body->Accept(visitor, indent + 1);
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

		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override
		{
			visitor.Visit(*this, indent);
		}
	private:
		QualifiedType type;
		std::string typealias;
	};
	
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
		AST() { tr_unit = std::make_unique<TranslationUnitDeclAST>(); }
		std::unique_ptr<TranslationUnitDeclAST> tr_unit;
	};
}
