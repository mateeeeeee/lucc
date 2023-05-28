#include "AST.h"
#include "Backend/ICodeGenerator.h"
#include "Core/Defines.h"

namespace lucc
{
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
		if (body) body->Accept(visitor, depth + 1);
	}

	void TypedefDeclAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void CompoundStmtAST::AddStatement(std::unique_ptr<StmtAST>&& stmt)
	{
		statements.push_back(std::move(stmt));
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

	void UnaryExprAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		operand->Accept(visitor, depth + 1);
	}

	void Int64LiteralAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void StringLiteralAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void IdentifierAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void ReturnStmtAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		if (ret_expr) ret_expr->Accept(visitor, depth + 1);
	}

	void ImplicitCastExprAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		operand->Accept(visitor, depth + 1);
	}

	void GotoStmtAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void LabelStmtAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void Float32LiteralAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void Float64LiteralAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	/// Codegen
	bool TranslationUnitAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		return true;
	}
	
	bool VarDeclAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		ctx.DeclareStaticVariable(name.c_str()); 
		return true;
	}

	bool FunctionDeclAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		if (!body) return INVALID_REG;
		ctx.DeclareGlobalFunction(name.c_str());
		body->Codegen(ctx);
		ctx.ReturnFromFunction(name.c_str());
		return true;
	}

	bool BinaryExprAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		switch (op)
		{
		case BinaryExprKind::Assign: 
		{
			register_t rhs_reg = ctx.AllocateRegister();
			rhs->Codegen(ctx, rhs_reg);
			if (IdentifierAST* var_decl = AstCast<IdentifierAST>(lhs.get()))
			{
				ctx.Store(var_decl->GetName().data(), rhs_reg);
			}
			ctx.FreeRegister(rhs_reg);

			if (return_reg)
			{
				return_reg = ctx.AllocateRegister();
				//ctx.Load(var_decl->GetName().data(), var_reg);
				
			}
			return false;
		}
		break;
		case BinaryExprKind::Add:
		{
			//size_t rhs_reg = rhs->Codegen(ctx);
			//size_t lhs_reg = lhs->Codegen(ctx);
			//ctx.Add(lhs_reg, rhs_reg);
			//ctx.FreeRegister(rhs_reg);
			//return lhs_reg;
		}
		}
		return true;
	}

	bool IdentifierAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		if (return_reg) ctx.Load(name.c_str(), *return_reg);
		return true;
	}

	bool Int64LiteralAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		if (return_reg) ctx.Mov(value, *return_reg);
		return true;
	}

	bool ExprStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		expr->Codegen(ctx);
		return true;
	}

	bool CompoundStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		for (auto& stmt : statements)
		{
			bool stop = !stmt->Codegen(ctx);
			if (stop) break;
		}
		return true;
	}

}


