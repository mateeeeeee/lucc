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
	class ExprAST;
	class FunctionDeclAST;
	class ParamVarDeclAST;
	class CompoundStmtAST;
	class ReturnStmtAST;

	class NodeVisitorAST
	{
	public:
		virtual ~NodeVisitorAST() = default;
		virtual void Visit(TranslationUnitDeclAST const& node, size_t indent) = 0;
		virtual void Visit(TypedefDeclAST const& node, size_t indent) = 0;
		virtual void Visit(FunctionDeclAST const& node, size_t indent) = 0;
		virtual void Visit(CompoundStmtAST const& node, size_t indent) = 0;
		virtual void Visit(ReturnStmtAST const& node, size_t indent) = 0;
		virtual void Visit(ParamVarDeclAST const& node, size_t indent) = 0;
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
		void AddExternalDeclaration(std::unique_ptr<DeclAST>&& ext_decl);
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;

	private:
		std::vector<std::unique_ptr<DeclAST>> external_declarations;
	};
	class VarDeclAST : public DeclAST
	{
	public:
		VarDeclAST(QualifiedType const& type, std::string_view id, std::unique_ptr<ExprAST>&& expr = nullptr);
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;

	private:
		QualifiedType type;
		std::string identifier;
		std::unique_ptr<ExprAST> init_expr;
	};
	class ParamVarDeclAST : public DeclAST
	{
	public:
		ParamVarDeclAST(FunctionParameter const& param);
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;

	private:
		FunctionParameter param;
	};
	class FieldDeclAST : public DeclAST
	{
	public:
		FieldDeclAST(QualifiedType const& type, std::string_view id);

	private:
		QualifiedType type;
		std::string identifier;
	};
	class RecordDeclAST : public DeclAST
	{
	public:
		RecordDeclAST() = default;
		void AddField(std::unique_ptr<FieldDeclAST>&& field);
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;

	private:
		std::vector<std::unique_ptr<FieldDeclAST>> fields;
	};
	class FunctionDeclAST : public DeclAST
	{
	public:
		FunctionDeclAST() = default;
		void AddParamDeclaration(std::unique_ptr<ParamVarDeclAST>&& param);
		void AddBody(std::unique_ptr<CompoundStmtAST>&& body);
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;

	private:
		std::vector<std::unique_ptr<ParamVarDeclAST>> param_decls;
		std::unique_ptr<CompoundStmtAST> func_body;
	};
	class TypedefDeclAST : public DeclAST
	{
	public:
		TypedefDeclAST(QualifiedType const& type, std::string_view typealias);
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;

	private:
		QualifiedType type;
		std::string typealias;
	};

	class DeclStmtAST : public StmtAST
	{
	public:
		DeclStmtAST(std::unique_ptr<DeclAST>&& _decl);
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;

	private:
		std::unique_ptr<DeclAST> decl;
	};
	class ExprAST : public StmtAST
	{
	public:
		ExprAST() = default;
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override {}

	private:
	};
	class CompoundStmtAST : public StmtAST
	{
	public:
		CompoundStmtAST() = default;
		void AddStatement(std::unique_ptr<StmtAST>&& stmt);
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;
	
	private:
		std::vector<std::unique_ptr<StmtAST>> statements;
	};
	class IfStmtAST : public StmtAST
	{
	public:
		IfStmtAST(std::unique_ptr<ExprAST>&& condition, std::unique_ptr<StmtAST>&& then_stmt,
			std::unique_ptr<StmtAST>&& else_stmt) : condition(std::move(condition)), then_stmt(std::move(then_stmt)),
			else_stmt(std::move(else_stmt))
		{}

	private:
		std::unique_ptr<ExprAST> condition;
		std::unique_ptr<StmtAST> then_stmt;
		std::unique_ptr<StmtAST> else_stmt;
	};
	class WhileStmtAST : public StmtAST
	{
	public:
		WhileStmtAST(std::unique_ptr<ExprAST>&& condition, std::unique_ptr<StmtAST>&& body) 
			: condition(std::move(condition)), body(std::move(body))
		{}

	private:
		std::unique_ptr<ExprAST> condition;
		std::unique_ptr<StmtAST> body;
	};
	class DoWhileStmtAST : public StmtAST
	{
	public:
		DoWhileStmtAST(std::unique_ptr<ExprAST>&& condition, std::unique_ptr<StmtAST>&& body)
			: condition(std::move(condition)), body(std::move(body))
		{}

	private:
		std::unique_ptr<ExprAST> condition;
		std::unique_ptr<StmtAST> body;
	};
	class ReturnStmtAST : public StmtAST
	{
	public:
		ReturnStmtAST() = default;
		explicit ReturnStmtAST(std::unique_ptr<ExprAST>&& expr) : expr(std::move(expr)) {}
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;
	private:
		std::unique_ptr<ExprAST> expr;
	};

	enum class UnaryOpKind : uint8
	{
		PreIncrement, PreDecrement,
		PostIncrement, PostDecrement,
		Plus, Minus, 
		BitNot, LogicalNot,
		Dereference, AddressOf,
		Cast
	};

	class UnaryExprAST : public ExprAST
	{
	public:

	private:
	};

	enum class BinaryOpKind : uint8 
	{
		Assignment,
		Add, Sub, Mul, Div, Mod,
		BitAnd, BitOr, BitXor, BitShl, BitShr,
		LogicalAnd, LogicalOr,
		Equal, NEqual, Less, Greater, LessEq, GreaterEq,
		MemberAccess,
		kComma
	};

	class BinaryExprAST : public ExprAST
	{
	public:

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
