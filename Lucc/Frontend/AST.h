#pragma once
#include <vector>
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
	class CastExprAST;
	class ImplicitCastExprAST;
	class FunctionCallExprAST;
	class IntLiteralAST;
	class StringLiteralAST;
	class IdentifierExprAST;
	class DeclRefExprAST;
	class MemberRefExprAST;

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
		virtual void Visit(FunctionCallExprAST const& node, size_t depth) {}
		virtual void Visit(CastExprAST const& node, size_t depth) {}
		virtual void Visit(IntLiteralAST const& node, size_t depth) {}
		virtual void Visit(StringLiteralAST const& node, size_t depth) {}
		virtual void Visit(IdentifierExprAST const& node, size_t depth) {}
		virtual void Visit(DeclRefExprAST const& node, size_t depth) {}
		virtual void Visit(MemberRefExprAST const& node, size_t depth) {}
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
	enum Register;

	class NodeAST
	{
	public:
		virtual ~NodeAST() = default;
		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const = 0;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const {}

	protected:
		NodeAST() = default;
	};
	class TranslationUnitAST : public NodeAST
	{
	public:
		TranslationUnitAST() = default;
		void AddDeclaration(std::unique_ptr<DeclAST>&& stmt)
		{
			declarations.push_back(std::move(stmt));
		}
		auto const& GetDeclarations() const { return declarations; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

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
		void SetSymbol(VarSymbol* _sym) { sym = *_sym; }

		std::string_view GetName() const { return name; }
		SourceLocation const& GetLocation() const { return loc; }
		QualifiedType const& GetType() const { return sym.qtype;}
		DeclKind GetDeclKind() const { return kind; }
		Storage GetStorage() const { return sym.storage; }
		VarSymbol const& GetSymbol() const { return sym; }

		virtual int32 GetLocalOffset() const { return 0; }
		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;

	protected:
		std::string name;
		SourceLocation loc;
		VarSymbol sym;
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
		virtual int32 GetLocalOffset() const override { return local_offset; }
		ExprAST const* GetInitExpr() const { return init_expr.get(); }
		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

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
		void AddFunctionCall(FunctionCallExprAST const* func_call)
		{
			function_calls.push_back(func_call);
		}
		void SetFunctionBody(std::unique_ptr<CompoundStmtAST>&& _body)
		{
			body = std::move(_body);
			AssignLocalOffsets();
		}

		bool IsDefinition() const { return body != nullptr; }

		template<typename F> requires std::is_invocable_v<F, DeclAST*>
		void ForAllDeclarations(F&& fn) const
		{
			for (auto const& param : param_decls) fn(param.get());
			for (auto const* local : local_variables) fn(local);
		}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		std::vector<std::unique_ptr<VarDeclAST>> param_decls;
		std::unique_ptr<CompoundStmtAST> body;
		std::vector<VarDeclAST const*> local_variables;
		std::vector<FunctionCallExprAST const*> function_calls;
		uint32 stack_size = 0;

	private:
		void AssignLocalOffsets();
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
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		std::vector<std::unique_ptr<StmtAST>> statements;
	};
	class ExprStmtAST : public StmtAST
	{
	public:
		ExprStmtAST(std::unique_ptr<ExprAST>&& expr) : StmtAST(expr ? StmtKind::Expr : StmtKind::Null), expr(std::move(expr)) {}
		ExprAST* GetExpr() const { return expr.get(); }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		std::unique_ptr<ExprAST> expr;
	};
	class DeclStmtAST : public StmtAST
	{
	public:
		DeclStmtAST(std::vector<std::unique_ptr<DeclAST>>&& decls) : StmtAST(StmtKind::Decl), decls(std::move(decls)) {}
		auto const& GetDeclarations() const { return decls; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

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
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

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
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

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
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

		std::string_view GetLabel() const { return goto_label; }

	private:
		std::string goto_label;
	};
	class LabelStmtAST final : public StmtAST
	{
	public:
		LabelStmtAST(std::string_view label) : StmtAST(StmtKind::Label), label_name(label) {}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

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
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

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
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

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
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

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
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

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
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

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
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

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
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

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
		DeclRef,
		Cast,
		MemberAccess
	};
	enum class UnaryExprKind : uint8
	{
		PreIncrement, PreDecrement,
		PostIncrement, PostDecrement,
		Plus, Minus, BitNot,
		LogicalNot,
		Dereference, AddressOf
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

		virtual bool IsConstexpr() const { return false; }
		virtual int64 EvaluateConstexpr() const { return 0; }

		SourceLocation const& GetLocation() const { return loc; }
		QualifiedType const& GetType() const { return type; }
		ExprKind GetExprKind() const { return kind; }
		bool IsLValue() const { return value_category == ExprValueCategory::LValue; }
		bool IsAssignable() const
		{
			if (!IsLValue()) return false;
			if (!type->IsComplete() || type.IsConst() || type->Is(TypeKind::Array)) return false;
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
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

		virtual bool IsConstexpr() const override;
		virtual int64 EvaluateConstexpr() const override;

	private:
		UnaryExprKind op;
		std::unique_ptr<ExprAST> operand;

	private:
		void SetExpressionType();
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
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;
		virtual bool IsConstexpr() const override;
		virtual int64 EvaluateConstexpr() const override;

	private:
		std::unique_ptr<ExprAST> lhs, rhs;
		BinaryExprKind op;

	private:
		void SetExpressionType();
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
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;
		virtual bool IsConstexpr() const override;
		virtual int64 EvaluateConstexpr() const override;

	private:
		std::unique_ptr<ExprAST> cond_expr;
		std::unique_ptr<ExprAST> true_expr;
		std::unique_ptr<ExprAST> false_expr;
	};

	class FunctionCallExprAST : public ExprAST
	{
	public:
		FunctionCallExprAST(std::unique_ptr<ExprAST>&& func, SourceLocation const& loc)
			: ExprAST(ExprKind::FunctionCall, loc), func_expr(std::move(func))
		{
			auto const& type = func_expr->GetType();
			if (FunctionType const* func_type = type->TryAs<FunctionType>())
			{
				SetType(RemoveQualifiers(func_type->GetReturnType()));
			}
			else if (PointerType const* func_ptr_type = type->TryAs<PointerType>())
			{
				if (func_ptr_type->PointeeType()->Is(TypeKind::Function))
				{
					FunctionType const& func_type = func_ptr_type->PointeeType()->As<FunctionType>();
					SetType(RemoveQualifiers(func_type.GetReturnType()));
				}
				else LU_ASSERT(false);
			}
			else LU_ASSERT(false);
		}
		void AddArgument(std::unique_ptr<ExprAST>&& arg)
		{
			func_args.push_back(std::move(arg));
		}

		ExprAST* GetFunction() const { return func_expr.get(); }
		std::vector<std::unique_ptr<ExprAST>> const& GetFunctionArgs() const { return func_args; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

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
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;
		virtual bool IsConstexpr() const override;
		virtual int64 EvaluateConstexpr() const override;

	private:
		int64 value;
	};
	class StringLiteralAST final : public ExprAST
	{
	public:
		StringLiteralAST(std::string_view str, SourceLocation const& loc) : ExprAST(ExprKind::StringLiteral, loc, QualifiedType(ArrayType(builtin_types::Char, str.size()))), str(str) {}
		std::string_view GetString() const { return str; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		std::string str;
	};

	class CastExprAST : public ExprAST
	{
	public:
		CastExprAST(SourceLocation const& loc, QualifiedType const& qtype) 
			: ExprAST(ExprKind::Cast, loc, qtype), operand(nullptr) 
		{
			SetValueCategory(ExprValueCategory::RValue);
		}
		void SetOperand(std::unique_ptr<ExprAST>&& _operand)
		{
			operand = std::move(_operand);
			SetCastType();
		}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		std::unique_ptr<ExprAST> operand;

	private:
		//// C11 6.5.4 Cast operators
		/*
		Otherwise, if type-name is exactly the type of expression, nothing is done
		Otherwise, the value of expression is converted to the type named by type-name, as follows:

		Every implicit conversion as if by assignment is allowed.

		In addition to the implicit conversions, the following conversions are allowed:
		Any integer can be cast to any pointer type. Except for the null pointer constants such as NULL (which doesn't need a cast), the result is implementation-defined, may not be correctly aligned, may not point to an object of the referenced type, and may be a trap representation.
		Any pointer type can be cast to any integer type. The result is implementation-defined, even for null pointer values (they do not necessarily result in the value zero). If the result cannot be represented in the target type, the behavior is undefined (unsigned integers do not implement modulo arithmetic on a cast from pointer)
		Any pointer to function can be cast to a pointer to any other function type. If the resulting pointer is converted back to the original type, it compares equal to the original value. If the converted pointer is used to make a function call, the behavior is undefined (unless the function types are compatible)
		When casting between pointers (either object or function), if the original value is a null pointer value of its type, the result is the correct null pointer value for the target type.
		*/
		void SetCastType();
	};

	class IdentifierExprAST : public ExprAST
	{
	public:
		std::string_view GetName() const { return name; }

	protected:
		explicit IdentifierExprAST(std::string_view name, SourceLocation const& loc, QualifiedType const& type) : ExprAST(ExprKind::DeclRef, loc, type), name(name)
		{
			SetValueCategory(ExprValueCategory::LValue);
		}
		virtual bool IsConstexpr() const override { return false; }
		virtual int64 EvaluateConstexpr() const override { return 0; }

	private:
		std::string name;
	};
	class DeclRefExprAST : public IdentifierExprAST
	{
	public:
		DeclRefExprAST(DeclAST* decl_ast, SourceLocation const& loc) : IdentifierExprAST(decl_ast->GetName(), loc, decl_ast->GetType()),
			decl_ast(decl_ast) {}

		VarSymbol const& GetSymbol() const { return decl_ast->GetSymbol(); }
		bool IsGlobal() const { return GetSymbol().global; }
		virtual int32 GetLocalOffset() const 
		{  
			return decl_ast->GetLocalOffset();
		}

		DeclAST const* GetDeclaration() const { return decl_ast; }

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	protected:
		DeclAST* decl_ast;
	};
	class MemberRefExprAST : public DeclRefExprAST
	{
	public:
		MemberRefExprAST(DeclAST* decl_ast, std::string_view member_name, SourceLocation const& loc)
			: DeclRefExprAST(decl_ast, loc), member_name(member_name)
		{
			SetValueCategory(ExprValueCategory::LValue);
			SetMemberType();
		}

		virtual int32 GetLocalOffset() const
		{
			LU_ASSERT(IsStructType(decl_ast->GetType()));
			StructType const& struct_type = decl_ast->GetType()->As<StructType>();
			int32 member_offset = (int32)struct_type.GetMemberOffset(member_name);
			return decl_ast->GetLocalOffset() + member_offset;
		}

		virtual void Accept(INodeVisitorAST& visitor, size_t depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		std::string member_name;

	private:
		void SetMemberType()
		{
			QualifiedType const& qtype = decl_ast->GetType();
			LU_ASSERT(qtype->Is(TypeKind::Struct));
			StructType const& struct_type = qtype->As<StructType>();

			LU_ASSERT(struct_type.HasMember(member_name));
			auto const& member_type = struct_type.GetMemberType(member_name);
			SetType(member_type);
		}
	};


	struct AST
	{
		AST() { translation_unit = std::make_unique<TranslationUnitAST>(); }
		std::unique_ptr<TranslationUnitAST> translation_unit;
	};

	inline std::unique_ptr<ExprAST> GetAssignExpr(std::unique_ptr<ExprAST>&& init_expr, QualifiedType const& type)
	{
		QualifiedType expr_type = ValueTransformation(init_expr->GetType());
		QualifiedType ret_type = RemoveQualifiers(type);
		if (expr_type->IsCompatible(ret_type)) return init_expr;
		QualifiedType assign_type = AsIfByAssignment(expr_type, ret_type);
		std::unique_ptr<CastExprAST> cast_expr = std::make_unique<CastExprAST>(init_expr->GetLocation(), assign_type);
		cast_expr->SetOperand(std::move(init_expr));
		return cast_expr;
	}
	inline std::unique_ptr<ExprStmtAST> GetAssignExprStmt(std::unique_ptr<ExprStmtAST>&& init_expr, QualifiedType const& type)
	{
		QualifiedType expr_type = ValueTransformation(init_expr->GetExpr()->GetType());
		QualifiedType ret_type = RemoveQualifiers(type);
		if (expr_type->IsCompatible(ret_type)) return init_expr;
		QualifiedType assign_type = AsIfByAssignment(expr_type, ret_type);
		std::unique_ptr<CastExprAST> cast_expr = std::make_unique<CastExprAST>(init_expr->GetExpr()->GetLocation(), assign_type);
		std::unique_ptr<ExprAST> expr(init_expr.release()->GetExpr());
		cast_expr->SetOperand(std::move(expr));
		return std::make_unique<ExprStmtAST>(std::move(cast_expr));
	}
}