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

	bool ExprAST::IsAssignable() const
	{
		if (!IsLValue()) return false;
		if (!type->IsComplete() || type.IsConst() || type->Is(PrimitiveTypeKind::Array)) return false;
		return true;
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

	void TranslationUnitAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		for (auto const& decl : declarations) decl->Codegen(ctx);
	}
	
	void VarDeclAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		if (sym.storage == Storage::Extern) ctx.DeclareExternVariable(name.c_str());
		else ctx.DeclareVariable(name.c_str(), sym.storage == Storage::Static); 
	}

	void FunctionDeclAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		//just a declaration but could be extern 
		if (!body)
		{
			if(sym.storage == Storage::Extern) ctx.DeclareExternFunction(name.c_str());
			return;
		}
		ctx.DeclareFunction(name.c_str(), sym.storage == Storage::Static); //#todo: should be linkage
		body->Codegen(ctx);
		ctx.ReturnFromFunction();
		return;
	}

	void UnaryExprAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg /*= std::nullopt*/) const
	{
		switch (op)
		{
		case UnaryExprKind::PreIncrement:
		case UnaryExprKind::PreDecrement:
		{
			if (IdentifierAST* identifier = AstCast<IdentifierAST>(operand.get()))
			{
				char const* name = identifier->GetName().data();
				if (op == UnaryExprKind::PreIncrement) ctx.Inc(name);
				else ctx.Dec(name);
				if (return_reg) ctx.Move(*return_reg, name);
			}
		}
		return;
		case UnaryExprKind::PostIncrement:
		case UnaryExprKind::PostDecrement:
		{
			if (IdentifierAST* identifier = AstCast<IdentifierAST>(operand.get()))
			{
				char const* name = identifier->GetName().data();
				if (return_reg) ctx.Move(*return_reg, name);
				if (op == UnaryExprKind::PostIncrement) ctx.Inc(name);
				else ctx.Dec(name);
			}
		}
		return;
		case UnaryExprKind::Plus:
		case UnaryExprKind::Minus:
		{
			if (return_reg)
			{
				if (Int64LiteralAST* literal = AstCast<Int64LiteralAST>(operand.get()))
				{
					ctx.Move(*return_reg, literal->GetValue());
					if(op == UnaryExprKind::Minus) ctx.Neg(*return_reg);
				}
				else if (IdentifierAST* identifier = AstCast<IdentifierAST>(operand.get()))
				{
					char const* name = identifier->GetName().data();
					ctx.Move(*return_reg, name);
					if (op == UnaryExprKind::Minus) ctx.Neg(*return_reg);
				}
				else
				{
					operand->Codegen(ctx, *return_reg);
					if (op == UnaryExprKind::Minus) ctx.Neg(*return_reg);
				}
			}
		}
		return;
		case UnaryExprKind::BitNot:
		case UnaryExprKind::LogicalNot:
		case UnaryExprKind::Dereference:
		case UnaryExprKind::AddressOf:
		case UnaryExprKind::Cast:
		default:
			LU_ASSERT_MSG(false, "Not implemented yet");
		}
	}

	void BinaryExprAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		auto CommonArithmeticCodegen = [&](BinaryExprKind kind)
		{
			if (!return_reg) return;

			if (Int64LiteralAST* int_literal = AstCast<Int64LiteralAST>(rhs.get()))
			{
				lhs->Codegen(ctx, *return_reg);
				switch (kind)
				{
				case BinaryExprKind::Add:		ctx.AddImm(*return_reg, int_literal->GetValue()); break;
				case BinaryExprKind::Subtract:  ctx.SubImm(*return_reg, int_literal->GetValue()); break;
				}
			}
			else
			{
				register_t tmp_reg = ctx.AllocateRegister();
				rhs->Codegen(ctx, tmp_reg);
				lhs->Codegen(ctx, *return_reg);
				switch (kind)
				{
				case BinaryExprKind::Add:		ctx.Add(*return_reg, tmp_reg); break;
				case BinaryExprKind::Subtract:  ctx.Sub(*return_reg, tmp_reg); break;
				}
				ctx.FreeRegister(tmp_reg);
			}
		};
		auto CommonComparisonCodegen = [&](BinaryExprKind kind)
		{
			if (!return_reg) return;

			Int64LiteralAST* int_literal = AstCast<Int64LiteralAST>(rhs.get());
			if (int_literal)
			{
				register_t reg = ctx.AllocateRegister();
				lhs->Codegen(ctx, reg);
				ctx.Compare(reg, int_literal->GetValue());
				ctx.FreeRegister(reg);
			}
			else
			{
				register_t reg1 = ctx.AllocateRegister();
				register_t reg2 = ctx.AllocateRegister();
				rhs->Codegen(ctx, reg2);
				lhs->Codegen(ctx, reg1);
				ctx.Compare(reg1, reg2);
				ctx.FreeRegister(reg2);
				ctx.FreeRegister(reg1);
			}
			switch (kind)
			{
			case BinaryExprKind::Less:			ctx.Set(*return_reg, Condition::Less); break;
			case BinaryExprKind::LessEqual:		ctx.Set(*return_reg, Condition::LessEqual); break;
			case BinaryExprKind::Greater:		ctx.Set(*return_reg, Condition::Greater); break;
			case BinaryExprKind::GreaterEqual:	ctx.Set(*return_reg, Condition::GreaterEqual); break;
			case BinaryExprKind::Equal:			ctx.Set(*return_reg, Condition::Equal); break;
			case BinaryExprKind::NotEqual:		ctx.Set(*return_reg, Condition::NotEqual); break;
			}
		};

		switch (op)
		{
		case BinaryExprKind::Assign: 
		{
			if (IdentifierAST* var_decl = AstCast<IdentifierAST>(lhs.get()))
			{
				char const* var_name = var_decl->GetName().data();
				if (Int64LiteralAST* int_literal = AstCast<Int64LiteralAST>(rhs.get()))
				{
					ctx.Move(var_name, int_literal->GetValue());
				}
				else if (FunctionCallAST* func_call = AstCast<FunctionCallAST>(rhs.get()))
				{
					register_t ret_reg = ctx.AllocateRegisterForReturn();
					func_call->Codegen(ctx);
					ctx.Move(var_name, ret_reg);
					ctx.FreeRegister(ret_reg);
				}
				else
				{
					register_t rhs_reg = return_reg ? *return_reg : ctx.AllocateRegister();
					rhs->Codegen(ctx, rhs_reg);
					ctx.Move(var_name, rhs_reg);
					if(!return_reg) ctx.FreeRegister(rhs_reg);
				}
			}
		}
		break;
		case BinaryExprKind::Add:
		case BinaryExprKind::Subtract:
		{
			CommonArithmeticCodegen(op);
		}
		break;
		case BinaryExprKind::Less:
		case BinaryExprKind::LessEqual:
		case BinaryExprKind::Greater:
		case BinaryExprKind::GreaterEqual:
		case BinaryExprKind::Equal:
		case BinaryExprKind::NotEqual:
		{
			CommonComparisonCodegen(op);
		}
		break;
		}
	}

	void IdentifierAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		if (return_reg) ctx.Move(*return_reg, name.c_str());
	}

	void Int64LiteralAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		if (return_reg) ctx.Move(*return_reg, value);
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
		register_t cond_reg = ctx.AllocateRegister();
		ctx.GenerateLabelId();
		condition->Codegen(ctx, cond_reg);
		ctx.Compare(cond_reg);
		ctx.Jump("L_else", Condition::Equal);
		then_stmt->Codegen(ctx);
		ctx.Jump("L_end");
		ctx.Label("L_else");
		if (else_stmt) else_stmt->Codegen(ctx);
		ctx.Label("L_end");
	}

	void ReturnStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		register_t reg = ctx.AllocateRegisterForReturn();
		ret_expr->Codegen(ctx, reg);
		ctx.JumpToFunctionEnd();
		ctx.FreeRegister(reg);
	}

	void WhileStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		register_t cond_reg = ctx.AllocateRegister();
		ctx.GenerateLabelId();
		ctx.Label("L_start");
		condition->Codegen(ctx, cond_reg);
		ctx.Compare(cond_reg);
		ctx.Jump("L_end", Condition::Equal);
		body_stmt->Codegen(ctx);
		ctx.Jump("L_start");
		ctx.Label("L_end");
		ctx.FreeRegister(cond_reg);
	}

	void DeclStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg /*= std::nullopt*/) const
	{
		for (auto const& decl : decls) decl->Codegen(ctx);
	}

	void ForStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		init_stmt->Codegen(ctx);
		register_t cond_reg = ctx.AllocateRegister();
		ctx.GenerateLabelId();
		ctx.Label("L_start");
		cond_expr->Codegen(ctx, cond_reg);
		ctx.Compare(cond_reg);
		ctx.Jump("L_end", Condition::Equal);
		body_stmt->Codegen(ctx);
		iter_expr->Codegen(ctx);
		ctx.Jump("L_start");
		ctx.Label("L_end");
		ctx.FreeRegister(cond_reg);
	}

	void FunctionCallAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg /*= std::nullopt*/) const
	{
		for (size_t i = 0; i < func_args.size(); ++i)
		{
			register_t arg_reg = ctx.AllocateRegisterForFunctionArg(i);
			func_args[i]->Codegen(ctx, arg_reg);
		}
		if (IdentifierAST* func_id = AstCast<IdentifierAST>(func_expr.get()))
		{
			ctx.CallFunction(func_id->GetName().data());
		}
		else LU_ASSERT(false);
	}
}


