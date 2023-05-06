#pragma once
#include <vector>
#include <memory>
#include <string>
#include "Type.h"

namespace lucc
{
	class NodeAST;
	class TranslationUnitAST;
	
	class ExprAST;
	class UnaryExprAST;
	class BinaryExprAST;
	class TernaryExprAST;
	class IntegerLiteralAST;
	class StringLiteralAST;
	class IdentifierAST;
	
	class StmtAST;
	class CompoundStmtAST;
	class DeclStmtAST;
	class ExprStmtAST;
	class NullStmtAST;
	class IfStmtAST;
	class WhileStmtAST;
	class ForStmtAST;

	class DeclAST;
	class VarDeclAST;
	class FunctionDeclAST;
	class TypedefDeclAST;

	class NodeVisitorAST
	{
	public:
		virtual ~NodeVisitorAST() = default;
		virtual void Visit(NodeAST const& node, size_t depth) = 0;
		virtual void Visit(TranslationUnitAST const& node, size_t depth) = 0;
		virtual void Visit(ExprAST const& node, size_t depth) = 0;
		virtual void Visit(UnaryExprAST const& node, size_t depth) = 0;
		virtual void Visit(BinaryExprAST const& node, size_t depth) = 0;
		virtual void Visit(TernaryExprAST const& node, size_t depth) = 0;
		virtual void Visit(IntegerLiteralAST const& node, size_t depth) = 0;
		virtual void Visit(StringLiteralAST const& node, size_t depth) = 0;
		virtual void Visit(IdentifierAST const& node, size_t depth) = 0;
		virtual void Visit(StmtAST const& node, size_t depth) = 0;
		virtual void Visit(CompoundStmtAST const& node, size_t depth) = 0;
		virtual void Visit(DeclStmtAST const& node, size_t depth) = 0;
		virtual void Visit(ExprStmtAST const& node, size_t depth) = 0;
		virtual void Visit(NullStmtAST const& node, size_t depth) = 0;
		virtual void Visit(IfStmtAST const& node, size_t depth) = 0;
		virtual void Visit(WhileStmtAST const& node, size_t depth) = 0;
		virtual void Visit(ForStmtAST const& node, size_t depth) = 0;
		virtual void Visit(DeclAST const& node, size_t depth) = 0;
		virtual void Visit(VarDeclAST const& node, size_t depth) = 0;
		virtual void Visit(FunctionDeclAST const& node, size_t depth) = 0;
		virtual void Visit(TypedefDeclAST const& node, size_t depth) = 0;
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
	class TypedefDeclAST final : public DeclAST
	{
	public:
		TypedefDeclAST(std::string_view typedef_name) : typedef_name(typedef_name) {}
		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;

		std::string_view GetName() const { return typedef_name; }
	private:
		std::string typedef_name;
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

		operator std::unique_ptr<ExprAST>&&()
		{
			return std::move(expr);
		}
	private:
		std::unique_ptr<ExprAST> expr;
	};
	class DeclStmtAST : public StmtAST
	{
	public:
		DeclStmtAST(std::vector<std::unique_ptr<DeclAST>>&& decls) : decls(std::move(decls)) {}
		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;

	private:
		std::vector<std::unique_ptr<DeclAST>> decls;
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
			: condition(std::move(condition)), then_stmt(std::move(then_stmt))
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
	class WhileStmtAST final : public StmtAST
	{
	public:
		WhileStmtAST(std::unique_ptr<ExprAST>&& condition, std::unique_ptr<StmtAST>&& body_stmt)
			: condition(std::move(condition)), body_stmt(std::move(body_stmt)) {}

		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;

	private:
		std::unique_ptr<ExprAST> condition;
		std::unique_ptr<StmtAST> body_stmt;
	};
	class ForStmtAST final : public StmtAST
	{
	public:
		explicit ForStmtAST(std::unique_ptr<StmtAST>&& body_stmt)
			: body_stmt(std::move(body_stmt)) {}

		void SetInit(std::unique_ptr<StmtAST>&& _init)
		{
			init_stmt = std::move(_init);
		}
		void SetConditionExpression(std::unique_ptr<ExprAST>&& _cond_expr)
		{
			cond_expr = std::move(_cond_expr);
		}
		void SetIterExpression(std::unique_ptr<ExprAST>&& _iter_expr)
		{
			iter_expr = std::move(_iter_expr);
		}

		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;

	private:
		std::unique_ptr<StmtAST> init_stmt;
		std::unique_ptr<ExprAST> cond_expr;
		std::unique_ptr<ExprAST> iter_expr;
		std::unique_ptr<StmtAST> body_stmt;
	};

	enum class UnaryExprKind : uint8
	{
		PreIncrement, PreDecrement,
		PostIncrement, PostDecrement,
		Plus, Minus, BitNot,
		LogicalNot,
		Dereference, AddressOf,
		Cast
	};
	enum class BinaryExprKind : uint8
	{
		Add, Subtract, Multiply, Divide, Modulo,
		ShiftLeft, ShiftRight, BitAnd, BitOr, BitXor,
		Assign,
		Comma,
		LogicalAnd, LogicalOr,
		// Comparison
		Equal, NotEqual,
		Less, Greater,
		LessEqual, GreaterEqual,
		Invalid
	};
	enum class ExprValueCategory : bool
	{
		LValue,
		RValue
	};
	class ExprAST : public NodeAST
	{
	public:
		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;

		void SetLocation(SourceLocation const& _loc) { loc = _loc; }
		void SetValueCategory(ExprValueCategory _value_category) { value_category = _value_category; }
		//void SetQType(const QualType& qtype) { qtype_ = qtype; }

		SourceLocation const& GetLocation() const { return loc; }
		bool IsLValue() const { return value_category == ExprValueCategory::LValue; }

	protected:
		SourceLocation loc;
		ExprValueCategory value_category = ExprValueCategory::LValue;

	protected:
		ExprAST() = default;

	};
	class UnaryExprAST : public ExprAST
	{
	public:
		explicit UnaryExprAST(UnaryExprKind op) : op(op) {}
		void SetOperand(std::unique_ptr<ExprAST>&& _operand) { operand = std::move(_operand); }
		UnaryExprKind GetOp() const { return op; }

		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;

	private:
		std::unique_ptr<ExprAST> operand;
		UnaryExprKind op;
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
	class TernaryExprAST : public ExprAST 
	{
	public:
		TernaryExprAST(std::unique_ptr<ExprAST>&& cond_expr, std::unique_ptr<ExprAST>&& true_expr,
			std::unique_ptr<ExprAST>&& false_expr) :
			cond_expr(std::move(cond_expr)),
			true_expr(std::move(true_expr)),
			false_expr(std::move(false_expr)) {}
		
		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;

	private:
		std::unique_ptr<ExprAST> cond_expr;
		std::unique_ptr<ExprAST> true_expr;
		std::unique_ptr<ExprAST> false_expr;
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
		StringLiteralAST(std::string_view str) : ExprAST(), str(str) {}
		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;

		std::string_view GetString() const { return str; }

	private:
		std::string str;
	};

	class IdentifierAST : public ExprAST
	{
	public:
		explicit IdentifierAST(std::string_view name) : ExprAST(), name(name) {}
		virtual void Accept(NodeVisitorAST& visitor, size_t depth) const override;

		std::string_view GetName() const { return name; }

	private:
		std::string name;
	};

	struct AST
	{
		AST() { translation_unit = std::make_unique<TranslationUnitAST>(); }
		std::unique_ptr<TranslationUnitAST> translation_unit;
	};
}