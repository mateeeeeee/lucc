#pragma once
#include <vector>
#include <optional>
#include <memory>
#include <string>
#include "Type.h"
#include "Backend/ICodeGenerator.h"

namespace lucc
{
	class NodeAST;
	class TranslationUnitAST;
	
	class ExprAST;
	class UnaryExprAST;
	class BinaryExprAST;
	class TernaryExprAST;
	class ImplicitCastExprAST;
	class Int64LiteralAST;
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
	class ReturnStmtAST;
	class GotoStmtAST;
	class LabelStmtAST;

	class DeclAST;
	class VarDeclAST;
	class FunctionDeclAST;
	class TypedefDeclAST;

	class INodeVisitorAST
	{
	public:
		virtual ~INodeVisitorAST() = default;
		virtual void Visit(NodeAST const& node, size_t depth) {}
		virtual void Visit(TranslationUnitAST const& node, size_t depth) {}
		virtual void Visit(ExprAST const& node, size_t depth) {}
		virtual void Visit(UnaryExprAST const& node, size_t depth) {}
		virtual void Visit(BinaryExprAST const& node, size_t depth) {}
		virtual void Visit(TernaryExprAST const& node, size_t depth) {}
		virtual void Visit(ImplicitCastExprAST const& node, size_t depth) {}
		virtual void Visit(Int64LiteralAST const& node, size_t depth) {}
		virtual void Visit(StringLiteralAST const& node, size_t depth) {}
		virtual void Visit(IdentifierAST const& node, size_t depth) {}
		virtual void Visit(StmtAST const& node, size_t depth) {}
		virtual void Visit(CompoundStmtAST const& node, size_t depth) {}
		virtual void Visit(DeclStmtAST const& node, size_t depth) {}
		virtual void Visit(ExprStmtAST const& node, size_t depth) {}
		virtual void Visit(NullStmtAST const& node, size_t depth) {}
		virtual void Visit(IfStmtAST const& node, size_t depth) {}
		virtual void Visit(WhileStmtAST const& node, size_t depth) {}
		virtual void Visit(ForStmtAST const& node, size_t depth) {}
		virtual void Visit(ReturnStmtAST const& node, size_t depth) {}
		virtual void Visit(GotoStmtAST const& node, size_t depth) {}
		virtual void Visit(LabelStmtAST const& node, size_t depth) {}
		virtual void Visit(DeclAST const& node, size_t depth) {}
		virtual void Visit(VarDeclAST const& node, size_t depth) {}
		virtual void Visit(FunctionDeclAST const& node, size_t depth) {}
		virtual void Visit(TypedefDeclAST const& node, size_t depth) {}
	};

	class ICodegenContext;
	class NodeAST
	{
	public:
		virtual ~NodeAST() = default;
		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const = 0;
		virtual void Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg = std::nullopt) const {}

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
		std::vector<std::unique_ptr<DeclAST>> const& GetDeclarations() const { return declarations; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg = std::nullopt) const override;

	private:
		std::vector<std::unique_ptr<DeclAST>> declarations;
	};

	class DeclAST : public NodeAST
	{
	public:

		void SetLocation(SourceLocation const& _loc) { loc = _loc; }
		void SetType(QualifiedType const& _type) { type = _type; }
		SourceLocation const& GetLocation() const { return loc; }
		QualifiedType const& GetType() const { return type; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
	
	protected:
		SourceLocation loc;
		QualifiedType type;

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

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg = std::nullopt) const override;
	private:
		std::string name;
		std::unique_ptr<ExprAST> init_expr;
	};
	class FunctionDeclAST : public DeclAST
	{
	public:
		FunctionDeclAST(std::string_view name) : name(name) {}
		std::string_view GetName() const { return name; }

		void AddParamDeclaration(std::unique_ptr<VarDeclAST>&& param)
		{
			param_decls.push_back(std::move(param));
		}
		void SetFunctionBody(std::unique_ptr<CompoundStmtAST>&& _body)
		{
			body = std::move(_body);
		}
		bool IsDefinition() const { return body != nullptr; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg = std::nullopt) const override;

	private:
		std::string name;
		std::vector<std::unique_ptr<VarDeclAST>> param_decls;
		std::unique_ptr<CompoundStmtAST> body;
	};
	class TypedefDeclAST final : public DeclAST
	{
	public:
		TypedefDeclAST(std::string_view typedef_name) : typedef_name(typedef_name) {}
		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;

		std::string_view GetName() const { return typedef_name; }

	private:
		std::string typedef_name;
	};
	class StmtAST : public NodeAST
	{
	public:
		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;

	protected:
		StmtAST() = default;
	};
	class CompoundStmtAST : public StmtAST
	{
	public:
		CompoundStmtAST() = default;
		void AddStatement(std::unique_ptr<StmtAST>&& stmt);

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg = std::nullopt) const override;

	private:
		std::vector<std::unique_ptr<StmtAST>> statements;
	};
	class ExprStmtAST : public StmtAST
	{
	public:
		ExprStmtAST(std::unique_ptr<ExprAST>&& expr) : expr(std::move(expr)) {}
		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg = std::nullopt) const override;

	private:
		std::unique_ptr<ExprAST> expr;
	};
	class DeclStmtAST : public StmtAST
	{
	public:
		DeclStmtAST(std::vector<std::unique_ptr<DeclAST>>&& decls) : decls(std::move(decls)) {}
		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;

	private:
		std::vector<std::unique_ptr<DeclAST>> decls;
	};
	class NullStmtAST final : public ExprStmtAST
	{
	public:
		NullStmtAST() : ExprStmtAST(nullptr) {}
		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
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

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg = std::nullopt) const override;

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

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;

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

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;

	private:
		std::unique_ptr<StmtAST> init_stmt;
		std::unique_ptr<ExprAST> cond_expr;
		std::unique_ptr<ExprAST> iter_expr;
		std::unique_ptr<StmtAST> body_stmt;
	};
	class ReturnStmtAST final : public StmtAST
	{
	public:
		explicit ReturnStmtAST(std::unique_ptr<ExprStmtAST>&& ret_expr)
			: ret_expr(std::move(ret_expr)) {}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg = std::nullopt) const override;

	private:
		std::unique_ptr <ExprStmtAST> ret_expr;
	};
	class GotoStmtAST final : public StmtAST
	{
	public:
		GotoStmtAST(std::string_view label) : goto_label(label) {}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;

		std::string_view GetName() const { return goto_label; }

	private:
		std::string goto_label;
	};
	class LabelStmtAST final : public StmtAST
	{
	public:
		LabelStmtAST(std::string_view label) : label_name(label) {}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;

		std::string_view GetName() const { return label_name; }

	private:
		std::string label_name;
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
		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;

		void SetLocation(SourceLocation const& _loc) { loc = _loc; }
		void SetType(QualifiedType const& _type) { type = _type; }
		void SetValueCategory(ExprValueCategory _value_category) { value_category = _value_category; }

		SourceLocation const& GetLocation() const { return loc; }
		QualifiedType const& GetType() const { return type; }
		bool IsLValue() const { return value_category == ExprValueCategory::LValue; }

	protected:
		SourceLocation loc;
		ExprValueCategory value_category = ExprValueCategory::LValue;
		QualifiedType type;

	protected:
		ExprAST() = default;

	};
	class UnaryExprAST : public ExprAST
	{
	public:
		explicit UnaryExprAST(UnaryExprKind op) : op(op) {}
		void SetOperand(std::unique_ptr<ExprAST>&& _operand) { operand = std::move(_operand); }
		UnaryExprKind GetOp() const { return op; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;

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

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg = std::nullopt) const override;

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
		
		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;

	private:
		std::unique_ptr<ExprAST> cond_expr;
		std::unique_ptr<ExprAST> true_expr;
		std::unique_ptr<ExprAST> false_expr;
	};

	enum class CastKind 
	{
		ArrayToPointer,
		FunctionToPointer,
		IntegerPromotion
	};
	
	class ImplicitCastExprAST : public ExprAST
	{
	public:
		ImplicitCastExprAST(std::unique_ptr<ExprAST>&& expr, CastKind kind)
		: operand(std::move(expr)), kind(kind) {}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;

		CastKind GetKind() const { return kind; }
	private:
		std::unique_ptr<ExprAST> operand;
		CastKind kind;
	};

	class Float32LiteralAST final : public ExprAST
	{
	public:
		Float32LiteralAST(float value) : ExprAST(), value(value) {}
		float GetValue() const { return value; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;

	private:
		float value;
	};
	class Float64LiteralAST final : public ExprAST
	{
	public:
		Float64LiteralAST(double value) : ExprAST(), value(value) {}
		double GetValue() const { return value; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;

	private:
		double value;
	};
	class Int64LiteralAST final : public ExprAST
	{
	public:
		Int64LiteralAST(int64 value) : ExprAST(), value(value) {}
		int64 GetValue() const { return value; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg = std::nullopt) const override;

	private:
		int64 value;
	};
	class StringLiteralAST final : public ExprAST
	{
	public:
		StringLiteralAST(std::string_view str) : ExprAST(), str(str) {}
		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;

		std::string_view GetString() const { return str; }

	private:
		std::string str;
	};
	class IdentifierAST : public ExprAST
	{
	public:
		explicit IdentifierAST(std::string_view name) : ExprAST(), name(name) {}
		std::string_view GetName() const { return name; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg = std::nullopt) const override;

	private:
		std::string name;
	};

	struct AST
	{
		AST() { translation_unit = std::make_unique<TranslationUnitAST>(); }
		std::unique_ptr<TranslationUnitAST> translation_unit;
	};

	template<typename To, typename From> 
	requires std::is_base_of_v<NodeAST, To> && std::is_base_of_v<NodeAST, From>
	To* AstCast(From* from)
	{
		return dynamic_cast<To*>(from);
	}
}