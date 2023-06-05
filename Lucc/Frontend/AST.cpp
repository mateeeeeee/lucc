#include "AST.h"
#include "Diagnostics.h"
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

	void IntLiteralAST::Accept(INodeVisitorAST& visitor, size_t depth) const
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

	void FloatLiteralAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void DoubleLiteralAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	/// Codegen
	//mov     rcx, qword ptr[rip + i]  |int k = *i; 
	//mov     eax, dword ptr[rcx]	   |
	//lea     rdx, [rcx + 4]		   |i += 1;
	//mov     qword ptr[rip + i], rdx  |
	
	static BitMode ConvertToBitMode(size_t type_size)
	{
		switch (type_size)
		{
		case 1: return BitMode_8;
		case 2: return BitMode_16;
		case 4: return BitMode_32;
		case 8: return BitMode_64;
		default:
			LU_ASSERT(false);
		}
		return BitMode_Count;
	}

	void TranslationUnitAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		for (auto const& decl : declarations) decl->Codegen(ctx);
	}

	void VarDeclAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		if (sym.storage == Storage::Extern)
		{
			size_t type_size = sym.qtype->GetSize();
			ctx.DeclareExternVariable(name.c_str(), ConvertToBitMode(type_size));
		}
		else
		{
			bool is_static = sym.storage == Storage::Static;
			if (IsArrayType(sym.qtype))
			{
				ArrayType const& array_type = TypeCast<ArrayType>(sym.qtype);
				size_t array_size = array_type.GetArraySize();
				size_t type_size = array_type.GetSize() / array_size;
				ctx.DeclareArray(name.c_str(), array_size, is_static, ConvertToBitMode(type_size));
			}
			else
			{
				size_t type_size = sym.qtype->GetSize();
				ctx.DeclareVariable(name.c_str(), is_static, ConvertToBitMode(type_size));
			}
		}
	}

	void FunctionDeclAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		//just a declaration but could be extern 
		if (!IsDefinition())
		{
			if(sym.storage == Storage::Extern) ctx.DeclareExternFunction(name.c_str());
			return;
		}
		ctx.DeclareFunction(name.c_str(), sym.storage == Storage::Static); 
		body->Codegen(ctx);
		ctx.ReturnFromFunction();
		return;
	}

	void UnaryExprAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg /*= std::nullopt*/) const
	{
		size_t type_size = GetType()->GetSize();
		BitMode bitmode = ConvertToBitMode(type_size);
		switch (op)
		{
		case UnaryExprKind::PreIncrement:
		case UnaryExprKind::PreDecrement:
		{
			LU_ASSERT(operand->IsLValue());
			bool const is_pointer_arithmetic = IsPointerLikeType(operand->GetType());
			int32 type_size = (int32)operand->GetType()->GetSize();
			if (is_pointer_arithmetic)
			{
				if (operand->GetExprKind() == ExprKind::Identifier)
				{
					IdentifierAST* identifier = AstCast<IdentifierAST>(operand.get());
					char const* name = identifier->GetName().data();
					register_t tmp_reg = ctx.AllocateRegister();
					ctx.Mov(tmp_reg, name, BitMode_64, true);
					if (op == UnaryExprKind::PreIncrement)	   ctx.Add(tmp_reg, type_size, BitMode_64);
					else if (op == UnaryExprKind::PreDecrement) ctx.Sub(tmp_reg, type_size, BitMode_64);
					ctx.Mov(name, tmp_reg);
					if (return_reg) ctx.Mov(*return_reg, tmp_reg, BitMode_64);
					ctx.FreeRegister(tmp_reg);
				}
				else LU_ASSERT(false);
			}
			else
			{
				if (operand->GetExprKind() == ExprKind::Identifier)
				{
					IdentifierAST* identifier = AstCast<IdentifierAST>(operand.get());
					char const* name = identifier->GetName().data();
					if (op == UnaryExprKind::PreIncrement) ctx.Inc(name, bitmode);
					else ctx.Dec(name);
					if (return_reg) ctx.Mov(*return_reg, name, bitmode);
				}
				else LU_ASSERT(false);
			}
		}
		return;
		case UnaryExprKind::PostIncrement:
		case UnaryExprKind::PostDecrement:
		{
			LU_ASSERT(operand->IsLValue());
			bool const is_pointer_arithmetic = IsPointerLikeType(operand->GetType());
			int32 type_size = (int32)operand->GetType()->GetSize();
			if (is_pointer_arithmetic)
			{
				if (operand->GetExprKind() == ExprKind::Identifier)
				{
					IdentifierAST* identifier = AstCast<IdentifierAST>(operand.get());
					char const* name = identifier->GetName().data();
					register_t tmp_reg = ctx.AllocateRegister();
					ctx.Mov(tmp_reg, name, BitMode_64, true);
					if (op == UnaryExprKind::PreIncrement)	   ctx.Add(tmp_reg, type_size, BitMode_64);
					else if (op == UnaryExprKind::PreDecrement) ctx.Sub(tmp_reg, type_size, BitMode_64);
					if (return_reg) ctx.Mov(*return_reg, tmp_reg, BitMode_64);
					ctx.Mov(name, tmp_reg);
					ctx.FreeRegister(tmp_reg);
				}
				else LU_ASSERT(false);
			}
			else
			{
				if (operand->GetExprKind() == ExprKind::Identifier)
				{
					IdentifierAST* identifier = AstCast<IdentifierAST>(operand.get());
					char const* name = identifier->GetName().data();
					if (return_reg) ctx.Mov(*return_reg, name, bitmode);
					if (op == UnaryExprKind::PostIncrement) ctx.Inc(name, bitmode);
					else ctx.Dec(name);
				}
				else LU_ASSERT(false);
			}
		}
		return;
		case UnaryExprKind::Plus:
		case UnaryExprKind::Minus:
		{
			if (return_reg)
			{
				LU_ASSERT(!IsPointerType(operand->GetType()));
				if (operand->GetExprKind() == ExprKind::IntLiteral)
				{
					IntLiteralAST* literal = AstCast<IntLiteralAST>(operand.get());
					ctx.Mov(*return_reg, literal->GetValue());
					if(op == UnaryExprKind::Minus) ctx.Neg(*return_reg);
				}
				else if (operand->GetExprKind() == ExprKind::Identifier)
				{
					IdentifierAST* identifier = AstCast<IdentifierAST>(operand.get());
					char const* name = identifier->GetName().data();
					ctx.Mov(*return_reg, name);
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
		case UnaryExprKind::Dereference:
		{
			if (return_reg) 
			{
				LU_ASSERT(IsPointerLikeType(operand->GetType()));
				register_t address_reg = ctx.AllocateRegister();
				operand->Codegen(ctx, address_reg);
				mem_ref_t mem_ref{ .base_reg = address_reg };
				ctx.Mov(*return_reg, mem_ref, ConvertToBitMode(operand->GetType()->GetSize()));
				ctx.FreeRegister(address_reg);
			}
		}
		return;
		case UnaryExprKind::AddressOf:
		{
			if (return_reg)
			{
				if (operand->GetExprKind() == ExprKind::Identifier)
				{
					IdentifierAST* identifier = AstCast<IdentifierAST>(operand.get());
					char const* name = identifier->GetName().data();
					ctx.Lea(*return_reg, name);
				}
				else 
				{
					LU_ASSERT_MSG(false, "Not implemented yet");
				}
			}
		}
		return;
		case UnaryExprKind::BitNot:
		case UnaryExprKind::LogicalNot:
		case UnaryExprKind::Cast:
		default:
			LU_ASSERT_MSG(false, "Not implemented yet");
		}
	}

	void BinaryExprAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		size_t type_size = GetType()->GetSize();
		BitMode bitmode = ConvertToBitMode(type_size);

		auto CommonArithmeticCodegen = [&](BinaryExprKind kind)
		{
			if (!return_reg) return;

			bool const lhs_is_pointer = IsPointerLikeType(lhs->GetType());
			bool const rhs_is_pointer = IsPointerLikeType(rhs->GetType());
			bool const is_pointer_arithmetic = lhs_is_pointer || rhs_is_pointer;

			if (is_pointer_arithmetic)
			{
				if (kind == BinaryExprKind::Add) LU_ASSERT(!lhs_is_pointer || !rhs_is_pointer);

				register_t tmp_reg = ctx.AllocateRegister();
				if (lhs_is_pointer)
				{
					lhs->Codegen(ctx, *return_reg);
					rhs->Codegen(ctx, tmp_reg);
					ctx.Imul(tmp_reg, tmp_reg, (int32)lhs->GetType()->GetSize());
				}
				else
				{
					rhs->Codegen(ctx, *return_reg);
					lhs->Codegen(ctx, tmp_reg);
					ctx.Imul(tmp_reg, tmp_reg, (int32)rhs->GetType()->GetSize());
				}

				switch (kind)
				{
				case BinaryExprKind::Add:		ctx.Add(*return_reg, tmp_reg, bitmode); break;
				case BinaryExprKind::Subtract:  ctx.Sub(*return_reg, tmp_reg, bitmode); break;
				}
			}
			else
			{
				if (rhs->GetExprKind() == ExprKind::IntLiteral)
				{
					IntLiteralAST* int_literal = AstCast<IntLiteralAST>(rhs.get());
					lhs->Codegen(ctx, *return_reg);

					switch (kind)
					{
					case BinaryExprKind::Add:		ctx.Add(*return_reg, (int32)int_literal->GetValue(), bitmode); break;
					case BinaryExprKind::Subtract:  ctx.Sub(*return_reg, (int32)int_literal->GetValue(), bitmode); break;
					}
				}
				else
				{
					register_t tmp_reg = ctx.AllocateRegister();
					rhs->Codegen(ctx, tmp_reg);
					lhs->Codegen(ctx, *return_reg);
					switch (kind)
					{
					case BinaryExprKind::Add:		ctx.Add(*return_reg, tmp_reg, bitmode); break;
					case BinaryExprKind::Subtract:  ctx.Sub(*return_reg, tmp_reg, bitmode); break;
					}
					ctx.FreeRegister(tmp_reg);
				}
			}
		};
		auto CommonComparisonCodegen = [&](BinaryExprKind kind)
		{
			if (!return_reg) return;

			if (rhs->GetExprKind() == ExprKind::IntLiteral)
			{
				IntLiteralAST* int_literal = AstCast<IntLiteralAST>(rhs.get());
				register_t reg = ctx.AllocateRegister();
				lhs->Codegen(ctx, reg);
				ctx.Cmp(reg, int_literal->GetValue(), bitmode);
				ctx.FreeRegister(reg);
			}
			else
			{
				register_t reg1 = ctx.AllocateRegister();
				register_t reg2 = ctx.AllocateRegister();
				rhs->Codegen(ctx, reg2);
				lhs->Codegen(ctx, reg1);
				ctx.Cmp(reg1, reg2, bitmode);
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
			LU_ASSERT_MSG(lhs->IsLValue(), "Cannot assign to rvalue!");
			if (lhs->GetExprKind() == ExprKind::Identifier)
			{
				IdentifierAST* var_decl = AstCast<IdentifierAST>(lhs.get());
				char const* var_name = var_decl->GetName().data();

				if (rhs->GetExprKind() == ExprKind::IntLiteral)
				{
					IntLiteralAST* int_literal = AstCast<IntLiteralAST>(rhs.get());
					ctx.Mov(var_name, (int32)int_literal->GetValue(), bitmode);
				}
				else if (rhs->GetExprKind() == ExprKind::FunctionCall)
				{
					FunctionCallAST* func_call = AstCast<FunctionCallAST>(rhs.get());
					register_t ret_reg = ctx.AllocateRegisterForReturn();
					func_call->Codegen(ctx);
					ctx.Mov(var_name, ret_reg, bitmode);
					ctx.FreeRegister(ret_reg);
				}
				else
				{
					register_t rhs_reg = return_reg ? *return_reg : ctx.AllocateRegister();
					rhs->Codegen(ctx, rhs_reg);
					ctx.Mov(var_name, rhs_reg, bitmode);
					if (!return_reg) ctx.FreeRegister(rhs_reg);
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
		LU_ASSERT(!IsFunctionType(GetType()));
		if (IsPointerLikeType(GetType()))
		{
			if (return_reg) ctx.Mov(*return_reg, name.c_str(), BitMode_64, true);
		}
		else
		{
			size_t type_size = GetType()->GetSize();
			BitMode bitmode = ConvertToBitMode(type_size);
			if (return_reg) ctx.Mov(*return_reg, name.c_str(), bitmode);
		}
	}

	void IntLiteralAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		size_t type_size = GetType()->GetSize();
		BitMode bitmode = ConvertToBitMode(type_size);
		if (return_reg) ctx.Mov(*return_reg, value, bitmode);
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
		ctx.Cmp(cond_reg);
		ctx.Jmp("L_else", Condition::Equal);
		then_stmt->Codegen(ctx);
		ctx.Jmp("L_end");
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
		ctx.Cmp(cond_reg);
		ctx.Jmp("L_end", Condition::Equal);
		body_stmt->Codegen(ctx);
		ctx.Jmp("L_start");
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
		ctx.Cmp(cond_reg);
		ctx.Jmp("L_end", Condition::Equal);
		body_stmt->Codegen(ctx);
		iter_expr->Codegen(ctx);
		ctx.Jmp("L_start");
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
		if (func_expr->GetExprKind() == ExprKind::Identifier)
		{
			IdentifierAST* func_id = AstCast<IdentifierAST>(func_expr.get());
			ctx.CallFunction(func_id->GetName().data());
		}
		else LU_ASSERT(false);
	}
}


