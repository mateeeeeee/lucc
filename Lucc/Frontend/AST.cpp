#include "AST.h"
#include "Diagnostics.h"
#include "Core/Defines.h"


namespace lucc
{
	inline int32 AlignTo(int32 n, int32 align) { return (n + align - 1) / align * align; }
	
	class VarDeclVisitorAST : public INodeVisitorAST
	{
	public:
		VarDeclVisitorAST(FunctionDeclAST* func_ref) : func_ref(func_ref) {}
		virtual void Visit(VarDeclAST const& node, size_t depth) override
		{
			func_ref->AddLocalDeclaration(&node);
		}

	private:
		FunctionDeclAST* func_ref;
	};

	class DeclRefVisitorAST : public INodeVisitorAST
	{
	public:
		DeclRefVisitorAST(FunctionDeclAST const* func_ref) : func_ref(func_ref) {}
		virtual void Visit(VarDeclRefAST const& node, size_t depth) override
		{
			func_ref->ForAllDeclarations([&](DeclAST const* decl)
			{
				if (decl->GetDeclKind() == DeclKind::Var)
				{
					VarDeclAST const* var_decl = AstCast<VarDeclAST>(decl);
					if (var_decl->GetSymbol() == node.GetSymbol())
					{
						node.SetLocalOffset(var_decl->GetLocalOffset());
					}
				}
			}
			);
		}

	private:
		FunctionDeclAST const* func_ref;
	};

	/// Accept

	void TranslationUnitAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		for (auto&& decl : declarations) decl->Accept(visitor, depth + 1);
	}

	void StmtAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void ExprAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void BinaryExprAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		lhs->Accept(visitor, depth + 1);
		rhs->Accept(visitor, depth + 1);
	}

	void NullStmtAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void ExprStmtAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		if (expr) expr->Accept(visitor, depth + 1);
	}

	void DeclAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void VarDeclAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		if (init_expr) init_expr->Accept(visitor, depth + 1);
	}

	void FunctionDeclAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		for (auto&& param : param_decls) param->Accept(visitor, depth + 1);
		if (body) body->Accept(visitor, depth + 1);
	}

	void TypedefDeclAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void CompoundStmtAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		for (auto&& stmt : statements) stmt->Accept(visitor, depth + 1);
	}

	void DeclStmtAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		for(auto&& decl : decls) decl->Accept(visitor, depth + 1);
	}

	void IfStmtAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		LU_ASSERT(condition && then_stmt);
		visitor.Visit(*this, depth);
		condition->Accept(visitor, depth + 1);
		then_stmt->Accept(visitor, depth + 1);
		if (else_stmt) else_stmt->Accept(visitor, depth + 1);
	}

	void WhileStmtAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		LU_ASSERT(condition && body_stmt);
		visitor.Visit(*this, depth);
		condition->Accept(visitor, depth + 1);
		body_stmt->Accept(visitor, depth + 1);
	}

	void DoWhileStmtAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		LU_ASSERT(condition && body_stmt);
		visitor.Visit(*this, depth);
		condition->Accept(visitor, depth + 1);
		body_stmt->Accept(visitor, depth + 1);
	}

	void SwitchStmtAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		LU_ASSERT(condition && body_stmt);
		visitor.Visit(*this, depth);
		condition->Accept(visitor, depth + 1);
		body_stmt->Accept(visitor, depth + 1);
	}

	void CaseStmtAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void ForStmtAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		LU_ASSERT(body_stmt);
		visitor.Visit(*this, depth);
		if (init_stmt) init_stmt->Accept(visitor, depth + 1);
		if (cond_expr) cond_expr->Accept(visitor, depth + 1);
		if (iter_expr) iter_expr->Accept(visitor, depth + 1);
		body_stmt->Accept(visitor, depth + 1);
	}

	void TernaryExprAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		LU_ASSERT(cond_expr && true_expr && false_expr);
		visitor.Visit(*this, depth);
		cond_expr->Accept(visitor, depth + 1);
		true_expr->Accept(visitor, depth + 1);
		false_expr->Accept(visitor, depth + 1);
	}

	void FunctionCallAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		func_expr->Accept(visitor, depth + 1);
		for (auto const& arg : func_args) arg->Accept(visitor, depth + 1);
	}

	void UnaryExprAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		operand->Accept(visitor, depth + 1);
	}

	void IntLiteralAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void StringLiteralAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	bool StringLiteralAST::IsConstexpr() const
	{
		return false;
	}

	int64 StringLiteralAST::EvaluateConstexpr() const
	{
		return 0;
	}

	void VarDeclRefAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void ReturnStmtAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		if (ret_expr) ret_expr->Accept(visitor, depth + 1);
	}

	void GotoStmtAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void LabelStmtAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	//constexpr 

	bool UnaryExprAST::IsConstexpr() const
	{
		return operand->IsConstexpr();
	}

	int64 UnaryExprAST::EvaluateConstexpr() const
	{
		LU_ASSERT_MSG(IsConstexpr(), "Cannot call EvaluateConstexpr on Expr that isn't constexpr");
		switch (op)
		{
		case UnaryExprKind::Plus:
			return operand->EvaluateConstexpr();
		case UnaryExprKind::Minus:
			return -operand->EvaluateConstexpr();
		case UnaryExprKind::BitNot:
			return ~operand->EvaluateConstexpr();
		case UnaryExprKind::LogicalNot:
			return !operand->EvaluateConstexpr();
		default:
			LU_ASSERT_MSG(false, "Invalid operation for constepxr");
		}
		return 0;
	}

	bool BinaryExprAST::IsConstexpr() const
	{
		return lhs->IsConstexpr() && rhs->IsConstexpr();
	}

	int64 BinaryExprAST::EvaluateConstexpr() const
	{
		LU_ASSERT_MSG(IsConstexpr(), "Cannot call EvaluateConstexpr on Expr that isn't constexpr");

		switch (op)
		{
		case BinaryExprKind::Add:
			return lhs->EvaluateConstexpr() + rhs->EvaluateConstexpr();
		case BinaryExprKind::Subtract:
			return lhs->EvaluateConstexpr() - rhs->EvaluateConstexpr();
		case BinaryExprKind::Multiply:
			return lhs->EvaluateConstexpr() * rhs->EvaluateConstexpr();
		case BinaryExprKind::Divide:
			return lhs->EvaluateConstexpr() / rhs->EvaluateConstexpr();
		case BinaryExprKind::Modulo:
			return lhs->EvaluateConstexpr() % rhs->EvaluateConstexpr();
		case BinaryExprKind::ShiftLeft:
			return lhs->EvaluateConstexpr() << rhs->EvaluateConstexpr();
		case BinaryExprKind::ShiftRight:
			return lhs->EvaluateConstexpr() >> rhs->EvaluateConstexpr();
		case BinaryExprKind::LogicalAnd:
			return lhs->EvaluateConstexpr() && rhs->EvaluateConstexpr();
		case BinaryExprKind::LogicalOr:
			return lhs->EvaluateConstexpr() || rhs->EvaluateConstexpr();
		case BinaryExprKind::BitAnd:
			return lhs->EvaluateConstexpr() & rhs->EvaluateConstexpr();
		case BinaryExprKind::BitOr:
			return lhs->EvaluateConstexpr() | rhs->EvaluateConstexpr();
		case BinaryExprKind::BitXor:
			return lhs->EvaluateConstexpr() ^ rhs->EvaluateConstexpr();
		case BinaryExprKind::Equal:
			return lhs->EvaluateConstexpr() == rhs->EvaluateConstexpr();
		case BinaryExprKind::NotEqual:
			return lhs->EvaluateConstexpr() != rhs->EvaluateConstexpr();
		case BinaryExprKind::Less:
			return lhs->EvaluateConstexpr() < rhs->EvaluateConstexpr();
		case BinaryExprKind::Greater:
			return lhs->EvaluateConstexpr() > rhs->EvaluateConstexpr();
		case BinaryExprKind::LessEqual:
			return lhs->EvaluateConstexpr() <= rhs->EvaluateConstexpr();
		case BinaryExprKind::GreaterEqual:
			return lhs->EvaluateConstexpr() >= rhs->EvaluateConstexpr();
		case BinaryExprKind::Comma:
			return rhs->EvaluateConstexpr();
		default:
			LU_ASSERT_MSG(false, "Invalid operation for constepxr");
		}
		return 0;
	}

	bool TernaryExprAST::IsConstexpr() const
	{
		if (!cond_expr->IsConstexpr()) return false;
		if (cond_expr->EvaluateConstexpr()) return true_expr->IsConstexpr();
		else return false_expr->IsConstexpr();
	}

	int64 TernaryExprAST::EvaluateConstexpr() const
	{
		LU_ASSERT_MSG(IsConstexpr(), "Cannot call EvaluateConstexpr on Expr that isn't constexpr");
		if (cond_expr->EvaluateConstexpr()) return true_expr->EvaluateConstexpr();
		else return false_expr->EvaluateConstexpr();
	}

	bool FunctionCallAST::IsConstexpr() const
	{
		return false;
	}

	int64 FunctionCallAST::EvaluateConstexpr() const
	{
		return 0;
	}

	bool IntLiteralAST::IsConstexpr() const
	{
		return true;
	}

	int64 IntLiteralAST::EvaluateConstexpr() const
	{
		return value;
	}

	/// Codegen

	void TranslationUnitAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		for (auto const& decl : declarations) decl->Codegen(ctx);
	}

	void VarDeclAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		
	}

	void FunctionDeclAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		
	}

	void UnaryExprAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		
	}

	void BinaryExprAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		
	}

	void TernaryExprAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		
	}

	void VarDeclRefAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		
	}

	void IntLiteralAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		
	}

	void ExprStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		if(expr) expr->Codegen(ctx, return_reg);
	}

	void CompoundStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		for (auto& stmt : statements) stmt->Codegen(ctx);
	}

	void IfStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		
	}

	void ReturnStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
	}

	void WhileStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		
	}

	void DoWhileStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		
	}

	void DeclStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		for (auto const& decl : decls) decl->Codegen(ctx);
	}

	void ForStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		
	}

	void FunctionCallAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		
	}

	void BreakStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		
	}

	void ContinueStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		
	}

	void GotoStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		
	}

	void LabelStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		
	}

	void SwitchStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		
	}

	void CaseStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		
	}

}


