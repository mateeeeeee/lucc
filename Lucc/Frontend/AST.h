#pragma once
#include <vector>
#include <memory>
#include <string>

namespace lucc
{
	class NodeAST;
	class TranslationUnitAST;
	
	class ExprAST;
	class BinaryExprAST;
	class IntegerLiteralAST;
	
	class StmtAST;
	class CompoundStmtAST;
	class DeclStmtAST;
	class ExprStmtAST;
	class NullStmtAST;
	class IfStmtAST;

	class DeclAST;
	class VarDeclAST;
	class FunctionDeclAST;

	class NodeVisitorAST
	{
	public:
		virtual ~NodeVisitorAST() = default;
		virtual void Visit(NodeAST const& node, size_t depth) = 0;
		virtual void Visit(TranslationUnitAST const& node, size_t depth) = 0;
		virtual void Visit(ExprAST const& node, size_t depth) = 0;
		virtual void Visit(BinaryExprAST const& node, size_t depth) = 0;
		virtual void Visit(IntegerLiteralAST const& node, size_t depth) = 0;
		virtual void Visit(StmtAST const& node, size_t depth) = 0;
		virtual void Visit(CompoundStmtAST const& node, size_t depth) = 0;
		virtual void Visit(DeclStmtAST const& node, size_t depth) = 0;
		virtual void Visit(ExprStmtAST const& node, size_t depth) = 0;
		virtual void Visit(NullStmtAST const& node, size_t depth) = 0;
		virtual void Visit(IfStmtAST const& node, size_t depth) = 0;
		virtual void Visit(DeclAST const& node, size_t depth) = 0;
		virtual void Visit(VarDeclAST const& node, size_t depth) = 0;
		virtual void Visit(FunctionDeclAST const& node, size_t depth) = 0;
	};

	class NodeAST
	{
	public:
		virtual ~NodeAST() = default;
		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const = 0;

	protected:
		NodeAST() = default;
	};
	class TranslationUnitAST : public NodeAST
	{
	public:
		TranslationUnitAST() = default;
		void AddDeclarations(std::unique_ptr<DeclAST>&& stmt)
		{
			declarations.push_back(std::move(stmt));
		}
		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;

	private:
		std::vector<std::unique_ptr<DeclAST>> declarations;
	};

	class DeclAST : public NodeAST
	{
	public:
		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;
	
	protected:
		DeclAST() = default;
	};
	class VarDeclAST : public DeclAST
	{
	public:
		VarDeclAST(std::string_view name) : name(name) {}

		void SetInitExpression(std::unique_ptr<ExprAST>&& expr)
		{
			init_expr = std::move(expr);
		}
		std::string_view GetName() const { return name; }

		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;
	
	private:
		std::string name;
		std::unique_ptr<ExprAST> init_expr;
	};
	class FunctionDeclAST : public DeclAST
	{
	public:
		FunctionDeclAST(std::string_view name) : name(name) {}

		std::string_view GetName() const { return name; }
		void SetFunctionBody(std::unique_ptr<CompoundStmtAST>&& _body)
		{
			body = std::move(_body);
		}

		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;

	private:
		std::string name;
		std::unique_ptr<CompoundStmtAST> body;
	};

	class StmtAST : public NodeAST
	{
	public:
		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;

	protected:
		StmtAST() = default;
	};
	class CompoundStmtAST : public StmtAST
	{
	public:
		CompoundStmtAST() = default;
		void AddStatement(std::unique_ptr<StmtAST>&& stmt);
		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;

	private:
		std::vector<std::unique_ptr<StmtAST>> statements;
	};
	class ExprStmtAST : public StmtAST
	{
	public:
		ExprStmtAST(std::unique_ptr<ExprAST>&& expr) : expr(std::move(expr)) {}
		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;

	private:
		std::unique_ptr<ExprAST> expr;
	};
	class DeclStmtAST : public StmtAST
	{
	public:
		DeclStmtAST(std::unique_ptr<DeclAST>&& decl) : decl(std::move(decl)) {}
		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;

	private:
		std::unique_ptr<DeclAST> decl;
	};
	class NullStmtAST final : public ExprStmtAST
	{
	public:
		NullStmtAST() : ExprStmtAST(nullptr) {}
		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;
	};
	class IfStmtAST final : public StmtAST
	{
	public:
		IfStmtAST(std::unique_ptr<ExprAST>&& condition, std::unique_ptr<StmtAST>&& then_stmt) 
			: condition(std::move(condition)), then_stmt(std::move(then_stmt)),
			  else_stmt(std::move(else_stmt))
		{}

		void AddElseStatement(std::unique_ptr<StmtAST>&& _else_stmt)
		{
			else_stmt = std::move(_else_stmt);
		}

		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;

	private:
		std::unique_ptr<ExprAST> condition;
		std::unique_ptr<StmtAST> then_stmt;
		std::unique_ptr<StmtAST> else_stmt;
	};

	enum class BinaryExprKind : uint8
	{
		Add, Subtract, Multiply, Divide,
		Assign,
		Invalid
	};
	class ExprAST : public NodeAST
	{
	public:
		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;

	protected:
		ExprAST() = default;
	};
	class BinaryExprAST : public ExprAST
	{
	public:
		explicit BinaryExprAST(BinaryExprKind op) 
			: op(op) {}
		void SetLHS(std::unique_ptr<ExprAST>&& _lhs) { lhs = std::move(_lhs); }
		void SetRHS(std::unique_ptr<ExprAST>&& _rhs) { rhs = std::move(_rhs); }
		BinaryExprKind GetOp() const { return op; }

		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;

	private:
		std::unique_ptr<ExprAST> lhs, rhs;
		BinaryExprKind op;
	};

	class IntegerLiteralAST final : public ExprAST
	{
	public:
		IntegerLiteralAST(int64 value) : ExprAST(), value(value) {}
		int64 GetValue() const { return value; }

		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;

	private:
		int64 value;
	};
	class StringLiteralAST final : public ExprAST
	{
	public:
		StringLiteralAST(std::string_view value) : ExprAST(), value(value) {}
		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override {}

	private:
		std::string_view value;
	};

	struct AST
	{
		AST() { translation_unit = std::make_unique<TranslationUnitAST>(); }
		std::unique_ptr<TranslationUnitAST> translation_unit;
	};
}