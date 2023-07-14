#pragma once
#include <vector>
#include <optional>
#include <memory>
#include <string>
#include "Type.h"
#include "Symbol.h"

namespace lucc
{
	class NodeAST;
	class TranslationUnitAST;

	class ExprAST;
	class UnaryExprAST;
	class BinaryExprAST;
	class TernaryExprAST;
	class FunctionCallAST;
	class ImplicitCastExprAST;
	class IntLiteralAST;
	class StringLiteralAST;
	class IdentifierAST;
	class VarDeclRefAST;

	class StmtAST;
	class CompoundStmtAST;
	class DeclStmtAST;
	class ExprStmtAST;
	class NullStmtAST;
	class IfStmtAST;
	class WhileStmtAST;
	class DoWhileStmtAST;
	class ForStmtAST;
	class SwitchStmtAST;
	class CaseStmtAST;
	class ReturnStmtAST;
	class GotoStmtAST;
	class LabelStmtAST;
	class BreakStmtAST;
	class ContinueStmtAST;

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
		virtual void Visit(FunctionCallAST const& node, size_t depth) {}
		virtual void Visit(ImplicitCastExprAST const& node, size_t depth) {}
		virtual void Visit(IntLiteralAST const& node, size_t depth) {}
		virtual void Visit(StringLiteralAST const& node, size_t depth) {}
		virtual void Visit(IdentifierAST const& node, size_t depth) {}
		virtual void Visit(VarDeclRefAST const& node, size_t depth) {}
		virtual void Visit(StmtAST const& node, size_t depth) {}
		virtual void Visit(CompoundStmtAST const& node, size_t depth) {}
		virtual void Visit(DeclStmtAST const& node, size_t depth) {}
		virtual void Visit(ExprStmtAST const& node, size_t depth) {}
		virtual void Visit(NullStmtAST const& node, size_t depth) {}
		virtual void Visit(IfStmtAST const& node, size_t depth) {}
		virtual void Visit(WhileStmtAST const& node, size_t depth) {}
		virtual void Visit(DoWhileStmtAST const& node, size_t depth) {}
		virtual void Visit(ForStmtAST const& node, size_t depth) {}
		virtual void Visit(SwitchStmtAST const& node, size_t depth) {}
		virtual void Visit(CaseStmtAST const& node, size_t depth) {}
		virtual void Visit(ReturnStmtAST const& node, size_t depth) {}
		virtual void Visit(GotoStmtAST const& node, size_t depth) {}
		virtual void Visit(LabelStmtAST const& node, size_t depth) {}
		virtual void Visit(BreakStmtAST const& node, size_t depth) {}
		virtual void Visit(ContinueStmtAST const& node, size_t depth) {}
		virtual void Visit(DeclAST const& node, size_t depth) {}
		virtual void Visit(VarDeclAST const& node, size_t depth) {}
		virtual void Visit(FunctionDeclAST const& node, size_t depth) {}
		virtual void Visit(TypedefDeclAST const& node, size_t depth) {}
	};

	class x86_64Context;
	struct Result;

	class NodeAST
	{
	public:
		virtual ~NodeAST() = default;
		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const = 0;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const {}

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
		auto const& GetDeclarations() const { return declarations; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;

	private:
		std::vector<std::unique_ptr<DeclAST>> declarations;
	};

	enum class DeclKind
	{
		Var,
		Func,
		Typedef
	};
	class DeclAST : public NodeAST
	{
	public:
		void SetLocation(SourceLocation const& _loc) { loc = _loc; }
		void SetSymbol(Symbol* _sym) { sym = *_sym; }

		std::string_view GetName() const { return name; }
		SourceLocation const& GetLocation() const { return loc; }
		Symbol const& GetSymbol() const { return sym; }
		DeclKind GetDeclKind() const { return kind; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;

	protected:
		std::string name;
		SourceLocation loc;
		Symbol sym;
		DeclKind kind;

	protected:
		DeclAST(std::string_view name, DeclKind kind) : name(name), kind(kind) {}
	};
	class VarDeclAST : public DeclAST
	{
	public:
		explicit VarDeclAST(std::string_view name) : DeclAST(name, DeclKind::Var), local_offset(0) {}

		void SetInitExpression(std::unique_ptr<ExprAST>&& expr)
		{
			init_expr = std::move(expr);
		}
		void SetLocalOffset(int32 _local_offset) const { local_offset = _local_offset; }

		bool IsGlobal() const { return GetSymbol().global; }
		int32 GetLocalOffset() const { return local_offset; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;

	private:
		std::unique_ptr<ExprAST> init_expr;
		mutable int32 local_offset;

	};
	class FunctionDeclAST : public DeclAST
	{
	public:
		explicit FunctionDeclAST(std::string_view name) : DeclAST(name, DeclKind::Func) {}

		void AddParamDeclaration(std::unique_ptr<VarDeclAST>&& param)
		{
			param_decls.push_back(std::move(param));
		}
		void AddLocalDeclaration(VarDeclAST const* var_decl)
		{
			local_variables.push_back(var_decl);
		}
		void SetFunctionBody(std::unique_ptr<CompoundStmtAST>&& _body)
		{
			body = std::move(_body);
		}

		bool IsDefinition() const { return body != nullptr; }

		template<typename F> requires std::is_invocable_v<F, DeclAST*>
		void ForAllDeclarations(F&& fn) const
		{
			for (auto const& param : param_decls) fn(param.get());
			for (auto const* local : local_variables) fn(local);
		}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;

	private:
		std::vector<std::unique_ptr<VarDeclAST>> param_decls;
		std::unique_ptr<CompoundStmtAST> body;
		std::vector<VarDeclAST const*> local_variables;
		mutable uint32 stack_size = 0;
	};
	class TypedefDeclAST final : public DeclAST
	{
	public:
		TypedefDeclAST(std::string_view typedef_name) : DeclAST(typedef_name, DeclKind::Typedef) {}
		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
	};

	enum class StmtKind
	{
		Compound,
		Expr,
		Decl,
		Null,
		If,
		While,
		DoWhile,
		Switch,
		Case,
		For,
		Return,
		Goto,
		Label,
		Break,
		Continue
	};
	class StmtAST : public NodeAST
	{
	public:
		StmtKind GetStmtKind() const { return kind; }
		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;

	protected:
		StmtKind kind;

	protected:
		explicit StmtAST(StmtKind kind) : kind(kind) {}
	};

	class CompoundStmtAST : public StmtAST
	{
	public:
		CompoundStmtAST() : StmtAST(StmtKind::Compound) {}

		void AddStatement(std::unique_ptr<StmtAST>&& stmt)
		{
			statements.push_back(std::move(stmt));
		}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;

	private:
		std::vector<std::unique_ptr<StmtAST>> statements;
	};
	class ExprStmtAST : public StmtAST
	{
	public:
		ExprStmtAST(std::unique_ptr<ExprAST>&& expr) : StmtAST(expr ? StmtKind::Expr : StmtKind::Null), expr(std::move(expr)) {}
		ExprAST* GetExpr() const { return expr.get(); }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;

	private:
		std::unique_ptr<ExprAST> expr;
	};
	class DeclStmtAST : public StmtAST
	{
	public:
		DeclStmtAST(std::vector<std::unique_ptr<DeclAST>>&& decls) : StmtAST(StmtKind::Decl), decls(std::move(decls)) {}
		auto const& GetDeclarations() const { return decls; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;

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
		IfStmtAST() : StmtAST(StmtKind::If) {}

		void SetCondition(std::unique_ptr<ExprAST>&& _condition)
		{
			condition = std::move(_condition);
		}
		void SetThenStatement(std::unique_ptr<StmtAST>&& _then_stmt)
		{
			then_stmt = std::move(_then_stmt);
		}
		void SetElseStatement(std::unique_ptr<StmtAST>&& _else_stmt)
		{
			else_stmt = std::move(_else_stmt);
		}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;

	private:
		std::unique_ptr<ExprAST> condition;
		std::unique_ptr<StmtAST> then_stmt;
		std::unique_ptr<StmtAST> else_stmt;
	};
	class CaseStmtAST final : public StmtAST
	{
	public:
		CaseStmtAST() : StmtAST(StmtKind::Case), switch_id(-1), is_default(true), value(0), label_name("L_default") {}
		explicit CaseStmtAST(int64 value) : StmtAST(StmtKind::Case), switch_id(-1), is_default(false), value(value), label_name("L_case" + std::to_string(value)) {}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;

		void SetSwitchId(uint64 _switch_id) { switch_id = _switch_id; }
		bool IsDefault() const { return is_default; }
		int64 GetValue() const { return value; }
		std::string_view GetLabel() const { return label_name; }
	private:
		std::string label_name;
		uint64 switch_id;
		int64 value;
		bool is_default;
	};
	class GotoStmtAST final : public StmtAST
	{
	public:
		explicit GotoStmtAST(std::string_view label) : StmtAST(StmtKind::Goto), goto_label(label) {}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;

		std::string_view GetLabel() const { return goto_label; }

	private:
		std::string goto_label;
	};
	class LabelStmtAST final : public StmtAST
	{
	public:
		LabelStmtAST(std::string_view label) : StmtAST(StmtKind::Label), label_name(label) {}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;

		std::string_view GetLabel() const { return label_name; }

	private:
		std::string label_name;
	};
	class BreakStmtAST final : public StmtAST
	{
	public:
		BreakStmtAST() : StmtAST(StmtKind::Break), label_id(-1) {}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override
		{
			visitor.Visit(*this, depth);
		}
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;

		void SetLabel(char const* _label_name, uint64 _label_id) { label_name = _label_name; label_id = _label_id; }

	private:
		std::string label_name;
		uint64 label_id;
	};
	class ContinueStmtAST final : public StmtAST
	{
	public:
		ContinueStmtAST() : StmtAST(StmtKind::Continue), label_id(-1) {}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override
		{
			visitor.Visit(*this, depth);
		}
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;

		void SetLabel(char const* _label_name, uint64 _label_id) { label_name = _label_name; label_id = _label_id; }

	private:
		std::string label_name;
		uint64 label_id;
	};
	class SwitchStmtAST final : public StmtAST
	{
	public:
		SwitchStmtAST() : StmtAST(StmtKind::Switch) {}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;

		void SetCondition(std::unique_ptr<ExprAST>&& _condition)
		{
			condition = std::move(_condition);
		}
		void SetBody(std::unique_ptr<StmtAST>&& _body_stmt)
		{
			body_stmt = std::move(_body_stmt);
		}

		void AddBreakStmt(BreakStmtAST* break_stmt)
		{
			break_stmts.push_back(break_stmt);
		}
		void AddCaseStatement(CaseStmtAST* case_stmt)
		{
			if (case_stmt->IsDefault())
			{
				LU_ASSERT(!has_default);
				has_default = true;
			}
			case_stmts.push_back(case_stmt);
		}
		bool HasDefaultCase() const
		{
			return has_default;
		}

	private:
		std::unique_ptr<ExprAST> condition;
		std::unique_ptr<StmtAST> body_stmt;
		std::vector<BreakStmtAST*> break_stmts;
		std::vector<CaseStmtAST*> case_stmts;
		bool has_default = false;
	};
	class WhileStmtAST final : public StmtAST
	{
	public:
		WhileStmtAST() : StmtAST(StmtKind::While) {}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;

		void SetCondition(std::unique_ptr<ExprAST>&& _condition)
		{
			condition = std::move(_condition);
		}
		void SetBody(std::unique_ptr<StmtAST>&& _body_stmt)
		{
			body_stmt = std::move(_body_stmt);
		}

		void AddContinueStmt(ContinueStmtAST* continue_stmt)
		{
			continue_stmts.push_back(continue_stmt);
		}
		void AddBreakStmt(BreakStmtAST* break_stmt)
		{
			break_stmts.push_back(break_stmt);
		}

	private:
		std::unique_ptr<ExprAST> condition;
		std::unique_ptr<StmtAST> body_stmt;
		std::vector<ContinueStmtAST*> continue_stmts;
		std::vector<BreakStmtAST*> break_stmts;
	};
	class DoWhileStmtAST final : public StmtAST
	{
	public:
		DoWhileStmtAST() : StmtAST(StmtKind::DoWhile) {}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;

		void SetCondition(std::unique_ptr<ExprAST>&& _condition)
		{
			condition = std::move(_condition);
		}
		void SetBody(std::unique_ptr<StmtAST>&& _body_stmt)
		{
			body_stmt = std::move(_body_stmt);
		}

		void AddContinueStmt(ContinueStmtAST* continue_stmt)
		{
			continue_stmts.push_back(continue_stmt);
		}
		void AddBreakStmt(BreakStmtAST* break_stmt)
		{
			break_stmts.push_back(break_stmt);
		}

	private:
		std::unique_ptr<ExprAST> condition;
		std::unique_ptr<StmtAST> body_stmt;
		std::vector<ContinueStmtAST*> continue_stmts;
		std::vector<BreakStmtAST*> break_stmts;
	};
	class ForStmtAST final : public StmtAST
	{
	public:
		ForStmtAST() : StmtAST(StmtKind::For) {}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;

		void SetInit(std::unique_ptr<StmtAST>&& _init)
		{
			init_stmt = std::move(_init);
		}
		void SetCondition(std::unique_ptr<ExprAST>&& _cond_expr)
		{
			cond_expr = std::move(_cond_expr);
		}
		void SetIterExpression(std::unique_ptr<ExprAST>&& _iter_expr)
		{
			iter_expr = std::move(_iter_expr);
		}
		void SetBody(std::unique_ptr<StmtAST>&& _body_stmt)
		{
			body_stmt = std::move(_body_stmt);
		}

		void AddContinueStmt(ContinueStmtAST* continue_stmt)
		{
			continue_stmts.push_back(continue_stmt);
		}
		void AddBreakStmt(BreakStmtAST* break_stmt)
		{
			break_stmts.push_back(break_stmt);
		}

	private:
		std::unique_ptr<StmtAST> init_stmt;
		std::unique_ptr<ExprAST> cond_expr;
		std::unique_ptr<ExprAST> iter_expr;
		std::unique_ptr<StmtAST> body_stmt;
		std::vector<ContinueStmtAST*> continue_stmts;
		std::vector<BreakStmtAST*> break_stmts;
	};
	class ReturnStmtAST final : public StmtAST
	{
	public:
		explicit ReturnStmtAST(std::unique_ptr<ExprStmtAST>&& ret_expr)
			: StmtAST(StmtKind::Return), ret_expr(std::move(ret_expr)) {}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;

	private:
		std::unique_ptr <ExprStmtAST> ret_expr;
	};

	enum class ExprKind
	{
		InitializerList,
		Unary,
		Binary,
		Ternary,
		FunctionCall,
		IntLiteral,
		StringLiteral,
		DeclRef
	};
	enum class UnaryExprKind : uint8
	{
		PreIncrement, PreDecrement,
		PostIncrement, PostDecrement,
		Plus, Minus, BitNot,
		LogicalNot,
		Dereference, AddressOf, Cast
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

		virtual bool IsConstexpr() const = 0;
		virtual int64 EvaluateConstexpr() const = 0;
		
		SourceLocation const& GetLocation() const { return loc; }
		QualifiedType const& GetType() const { return type; }
		ExprKind GetExprKind() const { return kind; }
		bool IsLValue() const { return value_category == ExprValueCategory::LValue; }
		bool IsAssignable() const
		{
			if (!IsLValue()) return false;
			if (!type->IsComplete() || type.IsConst() || type->Is(PrimitiveTypeKind::Array)) return false;
			return true;
		}

	protected:
		ExprKind kind;
		SourceLocation loc;
		QualifiedType type;
		ExprValueCategory value_category = ExprValueCategory::RValue;

	protected:
		ExprAST(ExprKind kind, SourceLocation const& loc, QualifiedType const& type = builtin_types::Int) : kind(kind), loc(loc), type(type) {}
		void SetValueCategory(ExprValueCategory _value_category) { value_category = _value_category; }
		void SetLocation(SourceLocation const& _loc) { loc = _loc; }
		void SetType(QualifiedType const& _type) { type = _type; }
	};

	class UnaryExprAST : public ExprAST
	{
	public:
		UnaryExprAST(UnaryExprKind op, SourceLocation const& loc) : ExprAST(ExprKind::Unary, loc), op(op), operand(nullptr)
		{}
		void SetOperand(std::unique_ptr<ExprAST>&& _operand)
		{
			operand = std::move(_operand);
			SetExpressionType();
		}
		UnaryExprKind GetUnaryKind() const { return op; }
		ExprAST* GetOperand() const { return operand.get(); }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;
		
		virtual bool IsConstexpr() const override;
		virtual int64 EvaluateConstexpr() const override;

	private:
		UnaryExprKind op;
		std::unique_ptr<ExprAST> operand;

	private:
		void SetExpressionType()
		{
			QualifiedType const& op_type = operand->GetType();
			switch (op)
			{
			case UnaryExprKind::PreIncrement:
			case UnaryExprKind::PreDecrement:
			case UnaryExprKind::PostIncrement:
			case UnaryExprKind::PostDecrement:
				SetType(IncDecOperatorType(op_type));
				break;
			case UnaryExprKind::Plus:
			case UnaryExprKind::Minus:
				SetType(PlusMinusOperatorType(op_type));
				break;
			case UnaryExprKind::BitNot:
				SetType(BitNotOperatorType(op_type));
				break;
			case UnaryExprKind::LogicalNot:
				SetType(LogicalNotOperatorType(op_type));
				break;
			case UnaryExprKind::Dereference:
				SetType(DereferenceOperatorType(op_type));
				if (!IsFunctionType(GetType())) SetValueCategory(ExprValueCategory::LValue);
				break;
			case UnaryExprKind::AddressOf:
				LU_ASSERT(operand->IsLValue() || IsFunctionType(op_type));
				SetType(AddressOfOperatorType(op_type));
				break;
			default:
				LU_ASSERT(false);
			}
		}
	};
	class BinaryExprAST : public ExprAST
	{
	public:
		BinaryExprAST(BinaryExprKind op, SourceLocation const& loc) : ExprAST(ExprKind::Binary, loc), op(op) {}
		void SetLHS(std::unique_ptr<ExprAST>&& _lhs) { lhs = std::move(_lhs); }
		void SetRHS(std::unique_ptr<ExprAST>&& _rhs) {
			rhs = std::move(_rhs); SetExpressionType();
		}
		
		BinaryExprKind GetBinaryKind() const { return op; }
		ExprAST* GetLHS() const { return lhs.get(); }
		ExprAST* GetRHS() const { return rhs.get(); }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;
		virtual bool IsConstexpr() const override;
		virtual int64 EvaluateConstexpr() const override;

	private:
		std::unique_ptr<ExprAST> lhs, rhs;
		BinaryExprKind op;

	private:
		void SetExpressionType()
		{
			switch (op)
			{
			case BinaryExprKind::Assign: 
				SetType(AsIfByAssignment(rhs->GetType(), lhs->GetType())); break;
			case BinaryExprKind::Add:
			case BinaryExprKind::Subtract: 
				SetType(AdditiveOperatorType(lhs->GetType(), rhs->GetType(), op == BinaryExprKind::Subtract));  break;
			case BinaryExprKind::Multiply:
			case BinaryExprKind::Divide:
			case BinaryExprKind::Modulo:
				SetType(MultiplicativeOperatorType(lhs->GetType(), rhs->GetType(), op == BinaryExprKind::Modulo));  break;
			case BinaryExprKind::ShiftLeft:
			case BinaryExprKind::ShiftRight: 
				SetType(ShiftOperatorType(lhs->GetType(), rhs->GetType())); break;
			case BinaryExprKind::LogicalAnd:
			case BinaryExprKind::LogicalOr: 
				SetType(LogicOperatorType(lhs->GetType(), rhs->GetType())); break;
			case BinaryExprKind::BitAnd:
			case BinaryExprKind::BitOr:
			case BinaryExprKind::BitXor:
				SetType(BitLogicOperatorType(lhs->GetType(), rhs->GetType())); break;
			case BinaryExprKind::Equal:
			case BinaryExprKind::NotEqual:
				SetType(EqualityOperatorType(lhs->GetType(), rhs->GetType())); break;
			case BinaryExprKind::Less:
			case BinaryExprKind::Greater:
			case BinaryExprKind::LessEqual:
			case BinaryExprKind::GreaterEqual:
				SetType(RelationOperatorType(lhs->GetType(), rhs->GetType())); break;
			case BinaryExprKind::Comma:
				SetType(ValueTransformation(rhs->GetType())); break;
			default:
				LU_ASSERT(false);
			}
		}
	};
	class TernaryExprAST : public ExprAST
	{
	public:
		explicit TernaryExprAST(SourceLocation const& loc) : ExprAST(ExprKind::Ternary, loc),
			cond_expr(std::move(cond_expr)),
			true_expr(std::move(true_expr)),
			false_expr(std::move(false_expr)) 
		{}

		void SetCondition(std::unique_ptr<ExprAST>&& expr) { cond_expr = std::move(expr); }
		void SetTrueExpr(std::unique_ptr<ExprAST>&& expr) { true_expr = std::move(expr); }
		void SetFalseExpr(std::unique_ptr<ExprAST>&& expr) { false_expr = std::move(expr); }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;
		virtual bool IsConstexpr() const override;
		virtual int64 EvaluateConstexpr() const override;

	private:
		std::unique_ptr<ExprAST> cond_expr;
		std::unique_ptr<ExprAST> true_expr;
		std::unique_ptr<ExprAST> false_expr;
	};

	class FunctionCallAST : public ExprAST
	{
	public:
		FunctionCallAST(std::unique_ptr<ExprAST>&& func, SourceLocation const& loc)
			: ExprAST(ExprKind::FunctionCall, loc), func_expr(std::move(func)) 
		{
			//#todo 
			auto const& type = func_expr->GetType();
			SetType(RemoveQualifiers(TypeCast<FunctionType>(type).GetReturnType()));
		}
		void AddArgument(std::unique_ptr<ExprAST>&& arg)
		{
			func_args.push_back(std::move(arg));
		}

		ExprAST* GetFunction() const { return func_expr.get(); }
		std::vector<std::unique_ptr<ExprAST>> const& GetFunctionArgs() const { return func_args; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;
		virtual bool IsConstexpr() const override;
		virtual int64 EvaluateConstexpr() const override;

	private:
		std::unique_ptr<ExprAST> func_expr;
		std::vector<std::unique_ptr<ExprAST>> func_args;
	};
	class IntLiteralAST final : public ExprAST
	{
	public:
		IntLiteralAST(int64 value, SourceLocation const& loc) : ExprAST(ExprKind::IntLiteral, loc, builtin_types::Int), value(value) {}
		int64 GetValue() const { return value; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;
		virtual bool IsConstexpr() const override;
		virtual int64 EvaluateConstexpr() const override;

	private:
		int64 value;
	};
	class StringLiteralAST final : public ExprAST
	{
	public:
		StringLiteralAST(std::string_view str, SourceLocation const& loc) : ExprAST(ExprKind::StringLiteral, loc, QualifiedType(ArrayType(builtin_types::Char, str.size()))), str(str) {}
		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;

		std::string_view GetString() const { return str; }
		virtual bool IsConstexpr() const override;
		virtual int64 EvaluateConstexpr() const override;

	private:
		std::string str;
	};

	class IdentifierAST : public ExprAST
	{
	public:
		std::string_view GetName() const { return name; }

	protected:
		explicit IdentifierAST(std::string_view name, SourceLocation const& loc, QualifiedType const& type) : ExprAST(ExprKind::DeclRef, loc, type), name(name)
		{
			SetValueCategory(ExprValueCategory::LValue);
		}
		virtual bool IsConstexpr() const override { return false; }
		virtual int64 EvaluateConstexpr() const override { return 0; }

	private:
		std::string name;
	};
	class VarDeclRefAST : public IdentifierAST
	{
	public:
		VarDeclRefAST(Symbol* symbol, SourceLocation const& loc) : IdentifierAST(symbol->name, loc, symbol->qtype),
			symbol(*symbol), local_offset(0) {}

		Symbol const& GetSymbol() const { return symbol; }
		bool IsGlobal() const { return symbol.global; }
		void SetLocalOffset(int32 _local_offset) const { local_offset = _local_offset; }
		int32 GetLocalOffset() const { return local_offset; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Result* result = nullptr) const override;

	private:
		Symbol symbol;
		mutable int32 local_offset;
	};

	struct AST
	{
		AST() { translation_unit = std::make_unique<TranslationUnitAST>(); }
		std::unique_ptr<TranslationUnitAST> translation_unit;
	};

	template<typename To, typename From>
	requires std::is_base_of_v<NodeAST, To> && std::is_base_of_v<NodeAST, From>
	To* DynamicAstCast(From* from)
	{
		return dynamic_cast<To*>(from);
	}
	template<typename To, typename From>
	requires std::is_base_of_v<NodeAST, To> && std::is_base_of_v<NodeAST, From>
	To* AstCast(From* from)
	{
		return static_cast<To*>(from);
	}
	template<typename To, typename From>
	requires std::is_base_of_v<NodeAST, To> && std::is_base_of_v<NodeAST, From>
	To const* AstCast(From const* from)
	{
		return static_cast<To const*>(from);
	}
}