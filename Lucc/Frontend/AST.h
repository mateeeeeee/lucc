#pragma once
#include <vector>
#include <memory>
#include "Scope.h"
#include "SourceLocation.h"
#include "ASTVisitor.h"
#include "ASTTypeAliases.h"

namespace lucc
{
	class x86_64Context;
	enum Register;

	class NodeAST
	{
	public:
		virtual ~NodeAST() = default;
		virtual void Accept(ASTVisitor& visitor, uint32 depth) const = 0;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const {}

	protected:
		NodeAST() = default;
	};
	class TranslationUnit final : public NodeAST
	{
	public:
		TranslationUnit() = default;
		void AddDecl(UniqueDeclPtr&& stmt)
		{
			declarations.push_back(std::move(stmt));
		}
		auto const& GetDecls() const { return declarations; }

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		UniqueDeclPtrList declarations;
	};

	enum class DeclKind
	{
		Var,
		Func,
		Typedef
	};
	class Decl : public NodeAST
	{
	public:

		SourceLocation const& GetLocation() const { return loc; }
		void SetLocation(SourceLocation const& _loc) { loc = _loc; }
		std::string_view GetName() const { return name; }
		QualifiedType const& GetType() const { return sym.qtype;}
		DeclKind GetDeclKind() const { return kind; }
		Storage GetStorage() const { return sym.storage; }
		DeclSymbol const& GetSymbol() const { return sym; }
		void SetSymbol(DeclSymbol* _sym) { sym = *_sym; }

		virtual int32 GetLocalOffset() const { return 0; }
		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;

	protected:
		DeclKind const kind;
		std::string name;
		SourceLocation loc;
		DeclSymbol sym;

	protected:
		Decl(DeclKind kind, std::string_view name) 
			: kind(kind), name(name) {}
	};
	class VariableDecl : public Decl
	{
	public:
		explicit VariableDecl(std::string_view name) : Decl(DeclKind::Var, name), local_offset(0) {}

		void SetInitExpr(UniqueExprPtr&& expr)
		{
			init_expr = std::move(expr);
		}
		Expr const* GetInitExpr() const { return init_expr.get(); }

		void SetLocalOffset(int32 _local_offset) const { local_offset = _local_offset; }
		virtual int32 GetLocalOffset() const override { return local_offset; }

		bool IsGlobal() const { return GetSymbol().global; }

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		UniqueExprPtr init_expr;
		mutable int32 local_offset;
		mutable int64 init_value = 0;

	};
	class FunctionDecl : public Decl
	{
	public:
		explicit FunctionDecl(std::string_view name) : Decl(DeclKind::Func, name) {}

		void AddParamDecl(UniqueVariableDeclPtr&& param)
		{
			param_decls.push_back(std::move(param));
		}
		void AddLocalDecl(VariableDecl const* var_decl)
		{
			local_variables.push_back(var_decl);
		}
		void AddFuncCallExpr(FunctionCallExpr const* func_call)
		{
			function_calls.push_back(func_call);
		}
		void SetBodyStmt(UniqueCompoundStmtPtr&& _body)
		{
			body = std::move(_body);
			AssignLocalOffsets();
		}

		bool IsDefinition() const { return body != nullptr; }

		template<typename F> requires std::is_invocable_v<F, Decl*>
		void ForAllDecls(F&& fn) const
		{
			for (auto const& param : param_decls) fn(param.get());
			for (auto const* local : local_variables) fn(local);
		}

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		UniqueVariableDeclPtrList param_decls;
		UniqueCompoundStmtPtr body;
		ConstVariableDeclPtrList local_variables;
		ConstFunctionCallExprPtrList function_calls;
		uint32 stack_size = 0;

	private:
		void AssignLocalOffsets();
	};
	class TypedefDecl final : public Decl
	{
	public:
		TypedefDecl(std::string_view typedef_name) : Decl(DeclKind::Typedef, typedef_name) {}
		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
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
	class Stmt : public NodeAST
	{
	public:
		StmtKind GetStmtKind() const { return kind; }
		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;

	protected:
		StmtKind const kind;

	protected:
		explicit Stmt(StmtKind kind) : kind(kind) {}
	};

	class CompoundStmt : public Stmt
	{
	public:
		CompoundStmt() : Stmt(StmtKind::Compound) {}

		void AddStmt(UniqueStmtPtr&& stmt)
		{
			statements.push_back(std::move(stmt));
		}

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		UniqueStmtPtrList statements;
	};
	class ExprStmt : public Stmt
	{
	public:
		ExprStmt(UniqueExprPtr&& expr) : Stmt(expr ? StmtKind::Expr : StmtKind::Null), expr(std::move(expr)) {}
		Expr const* GetExpr() const { return expr.get(); }
		Expr* GetExpr() { return expr.get(); }

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		UniqueExprPtr expr;
	};
	class DeclStmt : public Stmt
	{
	public:
		DeclStmt(UniqueDeclPtrList&& decls) : Stmt(StmtKind::Decl), decls(std::move(decls)) {}
		auto const& GetDecls() const { return decls; }

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		UniqueDeclPtrList decls;
	};
	class NullStmt final : public ExprStmt
	{
	public:
		NullStmt() : ExprStmt(nullptr) {}
		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
	};
	class ReturnStmt final : public Stmt
	{
	public:
		explicit ReturnStmt(UniqueExprStmtPtr&& ret_expr)
			: Stmt(StmtKind::Return), ret_expr(std::move(ret_expr)) {}

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		UniqueExprStmtPtr ret_expr;
	};
	class IfStmt final : public Stmt
	{
	public:
		IfStmt() : Stmt(StmtKind::If) {}

		void SetCondExpr(UniqueExprPtr&& _cond_expr)
		{
			cond_expr = std::move(_cond_expr);
		}
		void SetThenStmt(UniqueStmtPtr&& _then_stmt)
		{
			then_stmt = std::move(_then_stmt);
		}
		void SetElseStmt(UniqueStmtPtr&& _else_stmt)
		{
			else_stmt = std::move(_else_stmt);
		}

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		UniqueExprPtr cond_expr;
		UniqueStmtPtr then_stmt;
		UniqueStmtPtr else_stmt;
	};
	class BreakStmt final : public Stmt
	{
	public:
		BreakStmt() : Stmt(StmtKind::Break), label_id(-1) {}

		void SetLabel(char const* _label_name, uint64 _label_id) { label_name = _label_name; label_id = _label_id; }

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		std::string label_name;
		uint64 label_id;
	};
	class ContinueStmt final : public Stmt
	{
	public:
		ContinueStmt() : Stmt(StmtKind::Continue), label_id(-1) {}

		void SetLabel(char const* _label_name, uint64 _label_id) { label_name = _label_name; label_id = _label_id; }

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		std::string label_name;
		uint64 label_id;
	};
	class WhileStmt final : public Stmt
	{
	public:
		WhileStmt() : Stmt(StmtKind::While) {}

		void SetCondExpr(UniqueExprPtr&& _cond_expr)
		{
			cond_expr = std::move(_cond_expr);
		}
		void SetBodyStmt(UniqueStmtPtr&& _body_stmt)
		{
			body_stmt = std::move(_body_stmt);
		}

		void AddContinueStmt(ContinueStmt* continue_stmt)
		{
			continue_stmts.push_back(continue_stmt);
		}
		void AddBreakStmt(BreakStmt* break_stmt)
		{
			break_stmts.push_back(break_stmt);
		}

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		UniqueExprPtr cond_expr;
		UniqueStmtPtr body_stmt;
		ContinueStmtPtrList continue_stmts;
		BreakStmtPtrList break_stmts;
	};
	class DoWhileStmt final : public Stmt
	{
	public:
		DoWhileStmt() : Stmt(StmtKind::DoWhile) {}

		void SetCondExpr(UniqueExprPtr&& _cond_expr)
		{
			cond_expr = std::move(_cond_expr);
		}
		void SetBodyStmt(UniqueStmtPtr&& _body_stmt)
		{
			body_stmt = std::move(_body_stmt);
		}

		void AddContinueStmt(ContinueStmt* continue_stmt)
		{
			continue_stmts.push_back(continue_stmt);
		}
		void AddBreakStmt(BreakStmt* break_stmt)
		{
			break_stmts.push_back(break_stmt);
		}

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		UniqueExprPtr cond_expr;
		UniqueStmtPtr body_stmt;
		ContinueStmtPtrList continue_stmts;
		BreakStmtPtrList break_stmts;
	};
	class ForStmt final : public Stmt
	{
	public:
		ForStmt() : Stmt(StmtKind::For) {}

		void SetInitStmt(UniqueStmtPtr&& _init)
		{
			init_stmt = std::move(_init);
		}
		void SetCondExpr(UniqueExprPtr&& _cond_expr)
		{
			cond_expr = std::move(_cond_expr);
		}
		void SetIterExpr(UniqueExprPtr&& _iter_expr)
		{
			iter_expr = std::move(_iter_expr);
		}
		void SetBodyStmt(UniqueStmtPtr&& _body_stmt)
		{
			body_stmt = std::move(_body_stmt);
		}

		void AddContinueStmt(ContinueStmt* continue_stmt)
		{
			continue_stmts.push_back(continue_stmt);
		}
		void AddBreakStmt(BreakStmt* break_stmt)
		{
			break_stmts.push_back(break_stmt);
		}

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		UniqueStmtPtr init_stmt;
		UniqueExprPtr cond_expr;
		UniqueExprPtr iter_expr;
		UniqueStmtPtr body_stmt;
		ContinueStmtPtrList continue_stmts;
		BreakStmtPtrList break_stmts;
	};
	class CaseStmt final : public Stmt
	{
	public:
		CaseStmt() : Stmt(StmtKind::Case), switch_id(-1), is_default(true), value(0), label_name("L_default") {}
		explicit CaseStmt(int64 value) : Stmt(StmtKind::Case), switch_id(-1), is_default(false), value(value), label_name("L_case" + std::to_string(value)) {}

		void SetSwitchId(uint64 _switch_id) { switch_id = _switch_id; }
		bool IsDefault() const { return is_default; }
		int64 GetValue() const { return value; }
		std::string_view GetLabel() const { return label_name; }

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		std::string label_name;
		uint64 switch_id;
		int64 value;
		bool is_default;
	};
	class SwitchStmt final : public Stmt
	{
	public:
		SwitchStmt() : Stmt(StmtKind::Switch) {}

		void SetCondExpr(UniqueExprPtr&& _cond_expr)
		{
			cond_expr = std::move(_cond_expr);
		}
		void SetBodyStmt(UniqueStmtPtr&& _body_stmt)
		{
			body_stmt = std::move(_body_stmt);
		}

		void AddBreakStmt(BreakStmt* break_stmt)
		{
			break_stmts.push_back(break_stmt);
		}
		void AddCaseStmt(CaseStmt* case_stmt)
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

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		UniqueExprPtr cond_expr;
		UniqueStmtPtr body_stmt;
		BreakStmtPtrList break_stmts;
		CaseStmtPtrList case_stmts;
		bool has_default = false;
	};
	class GotoStmt final : public Stmt
	{
	public:
		explicit GotoStmt(std::string_view label) : Stmt(StmtKind::Goto), goto_label(label) {}

		std::string_view GetLabel() const { return goto_label; }

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		std::string goto_label;
	};
	class LabelStmt final : public Stmt
	{
	public:
		LabelStmt(std::string_view label) : Stmt(StmtKind::Label), label_name(label) {}

		std::string_view GetLabel() const { return label_name; }

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		std::string label_name;
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
	class Expr : public NodeAST
	{
	public:

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

		virtual bool IsConstexpr() const { return false; }
		virtual int64 EvaluateConstexpr() const { return 0; }

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;

	protected:
		ExprKind const kind;
		SourceLocation loc;
		QualifiedType type;
		ExprValueCategory value_category = ExprValueCategory::RValue;

	protected:
		Expr(ExprKind kind, SourceLocation const& loc, QualifiedType const& type = builtin_types::Int) : kind(kind), loc(loc), type(type) {}
		void SetValueCategory(ExprValueCategory _value_category) { value_category = _value_category; }
		void SetLocation(SourceLocation const& _loc) { loc = _loc; }
		void SetType(QualifiedType const& _type) { type = _type; }
	};
	class UnaryExpr : public Expr
	{
	public:
		UnaryExpr(UnaryExprKind op, SourceLocation const& loc) : Expr(ExprKind::Unary, loc), op(op), operand(nullptr)
		{}
		void SetOperand(UniqueExprPtr&& _operand)
		{
			operand = std::move(_operand);
			SetExpressionType();
		}
		UnaryExprKind GetUnaryKind() const { return op; }
		Expr const* GetOperand() const { return operand.get(); }

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

		virtual bool IsConstexpr() const override;
		virtual int64 EvaluateConstexpr() const override;

	private:
		UnaryExprKind op;
		UniqueExprPtr operand;

	private:
		void SetExpressionType();
	};
	class BinaryExpr : public Expr
	{
	public:
		BinaryExpr(BinaryExprKind op, SourceLocation const& loc) : Expr(ExprKind::Binary, loc), op(op) {}
		void SetLHS(UniqueExprPtr&& _lhs) { lhs = std::move(_lhs); }
		void SetRHS(UniqueExprPtr&& _rhs) {
			rhs = std::move(_rhs); SetExpressionType();
		}

		BinaryExprKind GetBinaryKind() const { return op; }
		Expr const* GetLHS() const { return lhs.get(); }
		Expr const* GetRHS() const { return rhs.get(); }

		virtual bool IsConstexpr() const override;
		virtual int64 EvaluateConstexpr() const override;

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		UniqueExprPtr lhs, rhs;
		BinaryExprKind op;

	private:
		void SetExpressionType();
	};
	class TernaryExpr : public Expr
	{
	public:
		explicit TernaryExpr(SourceLocation const& loc) : Expr(ExprKind::Ternary, loc),
			cond_expr(std::move(cond_expr)),
			true_expr(std::move(true_expr)),
			false_expr(std::move(false_expr))
		{}

		void SetCondExpr(UniqueExprPtr&& expr) { cond_expr = std::move(expr); }
		void SetTrueExpr(UniqueExprPtr&& expr) { true_expr = std::move(expr); }
		void SetFalseExpr(UniqueExprPtr&& expr) { false_expr = std::move(expr); }

		virtual bool IsConstexpr() const override;
		virtual int64 EvaluateConstexpr() const override;

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		UniqueExprPtr cond_expr;
		UniqueExprPtr true_expr;
		UniqueExprPtr false_expr;
	};

	class FunctionCallExpr final : public Expr
	{
	public:
		FunctionCallExpr(UniqueExprPtr&& func, SourceLocation const& loc)
			: Expr(ExprKind::FunctionCall, loc), func_expr(std::move(func))
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
		void AddArg(UniqueExprPtr&& arg)
		{
			func_args.push_back(std::move(arg));
		}

		Expr const* GetFuncExpr() const { return func_expr.get(); }

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		UniqueExprPtr func_expr;
		UniqueExprPtrList func_args;
	};
	class IntLiteral final : public Expr
	{
	public:
		IntLiteral(int64 value, SourceLocation const& loc) : Expr(ExprKind::IntLiteral, loc, builtin_types::Int), value(value) {}
		int64 GetValue() const { return value; }

		virtual bool IsConstexpr() const override;
		virtual int64 EvaluateConstexpr() const override;

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		int64 value;
	};
	class StringLiteral final : public Expr
	{
	public:
		StringLiteral(std::string_view str, SourceLocation const& loc) : Expr(ExprKind::StringLiteral, loc, QualifiedType(ArrayType(builtin_types::Char, (uint32)str.size()))), str(str) {}
		std::string_view GetString() const { return str; }

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		std::string str;
	};

	class CastExpr : public Expr
	{
	public:
		CastExpr(SourceLocation const& loc, QualifiedType const& qtype) 
			: Expr(ExprKind::Cast, loc, qtype), operand(nullptr) 
		{
			SetValueCategory(ExprValueCategory::RValue);
		}
		void SetOperand(UniqueExprPtr&& _operand)
		{
			operand = std::move(_operand);
			SetCastType();
		}

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		UniqueExprPtr operand;

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
	class IdentifierExpr : public Expr
	{
	public:
		std::string_view GetName() const { return name; }

	protected:
		explicit IdentifierExpr(std::string_view name, SourceLocation const& loc, QualifiedType const& type) : Expr(ExprKind::DeclRef, loc, type), name(name)
		{
			SetValueCategory(ExprValueCategory::LValue);
		}
		virtual bool IsConstexpr() const override { return false; }
		virtual int64 EvaluateConstexpr() const override { return 0; }

	private:
		std::string name;
	};
	class DeclRefExpr : public IdentifierExpr
	{
	public:
		DeclRefExpr(Decl* decl, SourceLocation const& loc) : IdentifierExpr(decl->GetName(), loc, decl->GetType()),
			decl(decl)  {}

		DeclSymbol const& GetSymbol() const { return decl->GetSymbol(); }
		bool IsGlobal() const { return GetSymbol().global; }
		virtual int32 GetLocalOffset() const 
		{  
			return decl->GetLocalOffset();
		}

		Decl const* GetDecl() const { return decl; }

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	protected:
		Decl* decl;
	};
	class MemberRefExpr final : public DeclRefExpr
	{
	public:
		MemberRefExpr(Decl* decl, std::string_view member_name, SourceLocation const& loc)
			: DeclRefExpr(decl, loc), member_name(member_name)
		{
			SetValueCategory(ExprValueCategory::LValue);
			SetMemberType();
		}

		virtual int32 GetLocalOffset() const
		{
			LU_ASSERT(IsStructType(decl->GetType()));
			StructType const& struct_type = decl->GetType()->As<StructType>();
			int32 member_offset = (int32)struct_type.GetMemberOffset(member_name);
			return decl->GetLocalOffset() + member_offset;
		}

		virtual void Accept(ASTVisitor& visitor, uint32 depth) const override;
		virtual void Codegen(x86_64Context& ctx, Register* result = nullptr) const override;

	private:
		std::string member_name;

	private:
		void SetMemberType()
		{
			QualifiedType const& qtype = decl->GetType();
			LU_ASSERT(qtype->Is(TypeKind::Struct));
			StructType const& struct_type = qtype->As<StructType>();

			LU_ASSERT(struct_type.HasMember(member_name));
			auto const& member_type = struct_type.GetMemberType(member_name);
			SetType(member_type);
		}
	};

	struct AST
	{
		AST() { translation_unit = MakeUnique<TranslationUnit>(); }
		UniqueTranslationUnitPtr translation_unit;
	};

	inline UniqueExprPtr GetAssignExpr(UniqueExprPtr&& init_expr, QualifiedType const& type)
	{
		QualifiedType expr_type = ValueTransformation(init_expr->GetType());
		QualifiedType ret_type = RemoveQualifiers(type);
		if (expr_type->IsCompatible(ret_type)) return init_expr;
		QualifiedType assign_type = AsIfByAssignment(expr_type, ret_type);
		UniqueCastExprPtr cast_expr = MakeUnique<CastExpr>(init_expr->GetLocation(), assign_type);
		cast_expr->SetOperand(std::move(init_expr));
		return cast_expr;
	}
	inline UniqueExprStmtPtr GetAssignExprStmt(UniqueExprStmtPtr&& init_expr, QualifiedType const& type)
	{
		QualifiedType expr_type = ValueTransformation(init_expr->GetExpr()->GetType());
		QualifiedType ret_type = RemoveQualifiers(type);
		if (expr_type->IsCompatible(ret_type)) return init_expr;
		QualifiedType assign_type = AsIfByAssignment(expr_type, ret_type);
		UniqueCastExprPtr cast_expr = MakeUnique<CastExpr>(init_expr->GetExpr()->GetLocation(), assign_type);
		ExprStmt* init_expr_stmt = init_expr.release();
		UniqueExprPtr expr(init_expr_stmt->GetExpr());
		cast_expr->SetOperand(std::move(expr));
		return MakeUnique<ExprStmt>(std::move(cast_expr));
	}
}