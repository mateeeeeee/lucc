#include "AST.h"
#include "Diagnostics.h"
#include "Core/Defines.h"

namespace lucc
{
	inline int32 AlignTo(int32 n, int32 align) { return (n + align - 1) / align * align; }
	inline BitMode ConvertToBitMode(size_t type_size)
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
		virtual void Visit(DeclRefAST const& node, size_t depth) override
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

	void FunctionDeclAST::SetFunctionBody(std::unique_ptr<CompoundStmtAST>&& _body)
	{
		body = std::move(_body);
		VarDeclVisitorAST var_decl_visitor(this);
		body->Accept(var_decl_visitor, 0);
	}

	void FunctionDeclAST::AssignLocalVariableOffsets(uint64 args_in_registers) const
	{
		int32 top = 16;
		for (uint64 i = args_in_registers; i < param_decls.size(); ++i)
		{
			VarDeclAST* param = param_decls[i].get();
			top = AlignTo(top, 8);
			param->SetLocalOffset(top);
			top += (int32)param->GetSymbol().qtype->GetSize();
		}

		int32 bottom = 0;
		for (uint64 i = 0; i < std::min(args_in_registers, param_decls.size()); ++i)
		{
			VarDeclAST* param = param_decls[i].get();
			int32 alignment = (int32)param->GetSymbol().qtype->GetAlign();
			bottom += (int32)param->GetSymbol().qtype->GetSize();
			bottom = AlignTo(bottom, alignment);
			param->SetLocalOffset(-bottom);
		}

		for (VarDeclAST const* local_var : local_variables)
		{
			int32 alignment = (int32)local_var->GetSymbol().qtype->GetAlign();
			bottom += (int32)local_var->GetSymbol().qtype->GetSize();
			bottom = AlignTo(bottom, alignment);
			local_var->SetLocalOffset(-bottom);
		}
		stack_size = AlignTo(bottom, 16);
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

	void DeclRefAST::Accept(INodeVisitorAST& visitor, size_t depth) const
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

	void FloatLiteralAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void DoubleLiteralAST::Accept(INodeVisitorAST& visitor, size_t depth) const
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
		if (!IsGlobal())
		{
			if (init_expr)
			{
				size_t type_size = sym.qtype->GetSize();
				register_t init_reg = return_reg ? *return_reg : ctx.AllocateRegister();
				init_expr->Codegen(ctx, init_reg);
				LU_ASSERT(local_offset != 0);
				mem_ref_t mem_ref{ .base_reg = ctx.GetStackFrameRegister(), .displacement = local_offset };
				ctx.Mov(mem_ref, init_reg, ConvertToBitMode(type_size));
				if (!return_reg) ctx.FreeRegister(init_reg);
			}
			return;
		}

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
				if (init_expr)
				{
					if (init_expr->GetExprKind() == ExprKind::IntLiteral)
					{
						IntLiteralAST* integer = AstCast<IntLiteralAST>(init_expr.get());
						int64 value = integer->GetValue();
						ctx.DeclareVariable(name.c_str(), is_static, ConvertToBitMode(type_size), &value);
					}
				}
				ctx.DeclareVariable(name.c_str(), is_static, ConvertToBitMode(type_size));
			}
		}
	}

	void FunctionDeclAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		if (!IsDefinition()) 
		{
			if(sym.storage == Storage::Extern) ctx.DeclareExternFunction(name.c_str()); //#todo not quite correct
			return;
		}
		AssignLocalVariableOffsets(ctx.GetFunctionArgsInRegisters());
		DeclRefVisitorAST decl_ref_visitor(this);
		body->Accept(decl_ref_visitor, 0);

		ctx.DeclareFunction(name.c_str(), sym.storage == Storage::Static);
		if (!param_decls.empty() || !local_variables.empty())
		{
			ctx.ReserveStackSpace(stack_size);
		}
		
		for (uint64 i = 0; i < std::min(ctx.GetFunctionArgsInRegisters(), param_decls.size()); ++i)
		{
			VarDeclAST* param_var = param_decls[i].get();
			LU_ASSERT(param_var->GetLocalOffset() < 0);
			BitMode bitmode = ConvertToBitMode(param_var->GetSymbol().qtype->GetSize());
			register_t rbp = ctx.GetStackFrameRegister();
			register_t arg = ctx.GetFunctionArgumentRegister(i);
			mem_ref_t mem_ref{.base_reg = rbp, .displacement = param_var->GetLocalOffset() };
			ctx.Mov(mem_ref, arg, bitmode);
		}

		body->Codegen(ctx);
		ctx.Return();
		return;
	}

	void UnaryExprAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
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
			if (is_pointer_arithmetic)
			{
				int32 type_size = 0;
				if (IsPointerType(operand->GetType()))
				{
					PointerType const& ptr_type = TypeCast<PointerType>(operand->GetType());
					type_size = (int32)ptr_type.PointeeType()->GetSize();
				}
				else if(IsArrayType(operand->GetType()))
				{
					ArrayType const& array_type = TypeCast<ArrayType>(operand->GetType());
					type_size = (int32)array_type.GetElementType()->GetSize();
				}
				LU_ASSERT(type_size);

				if (operand->GetExprKind() == ExprKind::DeclRef)
				{
					DeclRefAST* decl_ref = AstCast<DeclRefAST>(operand.get());
					if (decl_ref->IsGlobal())
					{
						char const* name = decl_ref->GetName().data();
						register_t tmp_reg = ctx.AllocateRegister();
						ctx.Mov(tmp_reg, name, BitMode_64, true);
						if (op == UnaryExprKind::PreIncrement)	   ctx.Add(tmp_reg, type_size, BitMode_64);
						else if (op == UnaryExprKind::PreDecrement) ctx.Sub(tmp_reg, type_size, BitMode_64);
						ctx.Mov(name, tmp_reg, BitMode_64);
						if (return_reg) ctx.Mov(*return_reg, tmp_reg, BitMode_64);
						ctx.FreeRegister(tmp_reg);
					}
					else
					{
						int32 local_offset = decl_ref->GetLocalOffset();
						register_t rbp = ctx.GetStackFrameRegister();
						mem_ref_t mem_ref{ .base_reg = rbp, .displacement = local_offset };

						register_t tmp_reg = ctx.AllocateRegister();
						ctx.Lea(tmp_reg, mem_ref);
						if (op == UnaryExprKind::PreIncrement)	   ctx.Add(tmp_reg, type_size, BitMode_64);
						else if (op == UnaryExprKind::PreDecrement) ctx.Sub(tmp_reg, type_size, BitMode_64);
						ctx.Mov(mem_ref, tmp_reg, BitMode_64);
						if (return_reg) ctx.Mov(*return_reg, tmp_reg, BitMode_64);
						ctx.FreeRegister(tmp_reg);
					}
				}
				else LU_ASSERT(false);
			}
			else
			{
				int32 type_size = (int32)operand->GetType()->GetSize();
				if (operand->GetExprKind() == ExprKind::DeclRef)
				{
					DeclRefAST* decl_ref = AstCast<DeclRefAST>(operand.get());
					if (decl_ref->IsGlobal())
					{
						char const* name = decl_ref->GetName().data();
						if (op == UnaryExprKind::PreIncrement) ctx.Inc(name, bitmode);
						else ctx.Dec(name, bitmode);
						if (return_reg) ctx.Mov(*return_reg, name, bitmode);
					}
					else
					{
						int32 local_offset = decl_ref->GetLocalOffset();
						register_t rbp = ctx.GetStackFrameRegister();
						mem_ref_t mem_ref{ .base_reg = rbp, .displacement = local_offset };
						if (op == UnaryExprKind::PreIncrement) ctx.Inc(mem_ref, bitmode);
						else ctx.Dec(mem_ref, bitmode);
						if (return_reg) ctx.Mov(*return_reg, mem_ref, bitmode);
					}
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
			if (is_pointer_arithmetic)
			{
				int32 type_size = 0;
				if (IsPointerType(operand->GetType()))
				{
					PointerType const& ptr_type = TypeCast<PointerType>(operand->GetType());
					type_size = (int32)ptr_type.PointeeType()->GetSize();
				}
				else if (IsArrayType(operand->GetType()))
				{
					ArrayType const& array_type = TypeCast<ArrayType>(operand->GetType());
					type_size = (int32)array_type.GetElementType()->GetSize();
				}
				LU_ASSERT(type_size);

				if (operand->GetExprKind() == ExprKind::DeclRef)
				{
					DeclRefAST* decl_ref = AstCast<DeclRefAST>(operand.get());
					if (decl_ref->IsGlobal())
					{
						char const* name = decl_ref->GetName().data();
						register_t tmp_reg = ctx.AllocateRegister();
						ctx.Mov(tmp_reg, name, BitMode_64, true);
						if (op == UnaryExprKind::PreIncrement)	   ctx.Add(tmp_reg, type_size, BitMode_64);
						else if (op == UnaryExprKind::PreDecrement) ctx.Sub(tmp_reg, type_size, BitMode_64);
						if (return_reg) ctx.Mov(*return_reg, tmp_reg, BitMode_64);
						ctx.Mov(name, tmp_reg, BitMode_64);
						ctx.FreeRegister(tmp_reg);
					}
					else
					{
						int32 local_offset = decl_ref->GetLocalOffset();
						register_t rbp = ctx.GetStackFrameRegister();
						mem_ref_t mem_ref{ .base_reg = rbp, .displacement = local_offset };
						register_t tmp_reg = ctx.AllocateRegister();
						ctx.Lea(tmp_reg, mem_ref);
						if (op == UnaryExprKind::PreIncrement)	    ctx.Add(tmp_reg, type_size, BitMode_64);
						else if (op == UnaryExprKind::PreDecrement) ctx.Sub(tmp_reg, type_size, BitMode_64);
						if (return_reg) ctx.Mov(*return_reg, tmp_reg, BitMode_64);
						ctx.Mov(mem_ref, tmp_reg, BitMode_64);
						ctx.FreeRegister(tmp_reg);
					}
				}
				else LU_ASSERT(false);
			}
			else
			{
				int32 type_size = (int32)operand->GetType()->GetSize();
				if (operand->GetExprKind() == ExprKind::DeclRef)
				{
					DeclRefAST* decl_ref = AstCast<DeclRefAST>(operand.get());
					if (decl_ref->IsGlobal())
					{
						char const* name = decl_ref->GetName().data();
						if (return_reg) ctx.Mov(*return_reg, name, bitmode);
						if (op == UnaryExprKind::PostIncrement) ctx.Inc(name, bitmode);
						else ctx.Dec(name, bitmode);
					}
					else
					{
						int32 local_offset = decl_ref->GetLocalOffset();
						register_t rbp = ctx.GetStackFrameRegister();
						mem_ref_t mem_ref{ .base_reg = rbp, .displacement = local_offset };
						if (return_reg) ctx.Mov(*return_reg, mem_ref, bitmode);
						if (op == UnaryExprKind::PostIncrement) ctx.Inc(mem_ref, bitmode);
						else ctx.Dec(mem_ref, bitmode);
					}
					
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
					ctx.Mov(*return_reg, literal->GetValue(), bitmode);
					if(op == UnaryExprKind::Minus) ctx.Neg(*return_reg, bitmode);
				}
				else if (operand->GetExprKind() == ExprKind::DeclRef)
				{
					DeclRefAST* decl_ref = AstCast<DeclRefAST>(operand.get());
					if (decl_ref->IsGlobal())
					{
						char const* name = decl_ref->GetName().data();
						ctx.Mov(*return_reg, name, bitmode);
						if (op == UnaryExprKind::Minus) ctx.Neg(*return_reg, bitmode);
					}
					else
					{
						int32 local_offset = decl_ref->GetLocalOffset();
						register_t rbp = ctx.GetStackFrameRegister();
						mem_ref_t mem_ref{ .base_reg = rbp, .displacement = local_offset };
						ctx.Mov(*return_reg, mem_ref, bitmode);
						if (op == UnaryExprKind::Minus) ctx.Neg(*return_reg, bitmode);
					}
					
				}
				else
				{
					operand->Codegen(ctx, *return_reg);
					if (op == UnaryExprKind::Minus) ctx.Neg(*return_reg, bitmode);
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
				if (operand->GetExprKind() == ExprKind::DeclRef)
				{
					DeclRefAST* decl_ref = AstCast<DeclRefAST>(operand.get());
					if (decl_ref->IsGlobal())
					{
						char const* name = decl_ref->GetName().data();
						ctx.Lea(*return_reg, name);
					}
					else
					{
						int32 local_offset = decl_ref->GetLocalOffset();
						register_t rbp = ctx.GetStackFrameRegister();
						mem_ref_t mem_ref{ .base_reg = rbp, .displacement = local_offset };
						ctx.Lea(*return_reg, mem_ref);
					}
				}
				else
				{
					LU_ASSERT_MSG(false, "Not implemented yet");
				}
			}
		}
		return;
		case UnaryExprKind::BitNot:
		{
			if (return_reg)
			{
				LU_ASSERT(!IsPointerLikeType(operand->GetType()));
				if (operand->GetExprKind() == ExprKind::IntLiteral)
				{
					IntLiteralAST* literal = AstCast<IntLiteralAST>(operand.get());
					ctx.Mov(*return_reg, literal->GetValue(), bitmode);
					ctx.Not(*return_reg, bitmode);
				}
				else if (operand->GetExprKind() == ExprKind::DeclRef)
				{
					DeclRefAST* decl_ref = AstCast<DeclRefAST>(operand.get());
					if (decl_ref->IsGlobal())
					{
						char const* name = decl_ref->GetName().data();
						ctx.Mov(*return_reg, name, bitmode);
						ctx.Not(*return_reg, bitmode);
					}
					else
					{
						int32 local_offset = decl_ref->GetLocalOffset();
						register_t rbp = ctx.GetStackFrameRegister();
						mem_ref_t mem_ref{ .base_reg = rbp, .displacement = local_offset };
						ctx.Mov(*return_reg, mem_ref, bitmode);
						ctx.Not(*return_reg, bitmode);
					}
					
				}
				else
				{
					operand->Codegen(ctx, *return_reg);
					ctx.Not(*return_reg, bitmode);
				}
			}
		}
		return;
		case UnaryExprKind::LogicalNot:
		{
			if (return_reg)
			{
				register_t tmp_reg = ctx.AllocateRegister();
				operand->Codegen(ctx, tmp_reg);
				ctx.Cmp(tmp_reg, int64(0), bitmode);
				ctx.Set(*return_reg, Condition::Equal);
				ctx.Movzx(*return_reg, *return_reg, BitMode_64, true);
				ctx.FreeRegister(tmp_reg);
			}
		}
		return;
		default:
			LU_ASSERT(false);
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
				LU_ASSERT(kind == BinaryExprKind::Add || kind == BinaryExprKind::Subtract);
				if (kind == BinaryExprKind::Add) LU_ASSERT(!lhs_is_pointer || !rhs_is_pointer);

				register_t tmp_reg = ctx.AllocateRegister();
				if (lhs_is_pointer)
				{
					auto decayed_type = ValueTransformation(lhs->GetType());
					lhs->Codegen(ctx, *return_reg);
					rhs->Codegen(ctx, tmp_reg);
					ctx.Imul(tmp_reg, tmp_reg, (int32)decayed_type->GetSize(), bitmode);
				}
				else
				{
					auto decayed_type = ValueTransformation(rhs->GetType());
					rhs->Codegen(ctx, *return_reg);
					lhs->Codegen(ctx, tmp_reg);
					ctx.Imul(tmp_reg, tmp_reg, (int32)decayed_type->GetSize(), bitmode);
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
					int32 value = (int32)int_literal->GetValue();
					lhs->Codegen(ctx, *return_reg);

					switch (kind)
					{
					case BinaryExprKind::Add:		ctx.Add(*return_reg, value, bitmode); break;
					case BinaryExprKind::Subtract:  ctx.Sub(*return_reg, value, bitmode); break;
					case BinaryExprKind::Multiply:  ctx.Imul(*return_reg, *return_reg, value, bitmode); break;
					case BinaryExprKind::Divide:	
					{
						register_t tmp_reg = ctx.AllocateRegister();
						ctx.Mov(tmp_reg, value, bitmode);
						ctx.Idiv(*return_reg, tmp_reg, bitmode);
					}
					break;
					case BinaryExprKind::Modulo:	LU_ASSERT(false); break;
					}
				}
				else
				{
					register_t tmp_reg = ctx.AllocateRegister();
					rhs->Codegen(ctx, tmp_reg);
					lhs->Codegen(ctx, *return_reg);
					switch (kind)
					{
					case BinaryExprKind::Add:		ctx.Add(*return_reg, tmp_reg, bitmode);  break;
					case BinaryExprKind::Subtract:  ctx.Sub(*return_reg, tmp_reg, bitmode);  break;
					case BinaryExprKind::Multiply:  ctx.Imul(*return_reg, tmp_reg, bitmode); break;
					case BinaryExprKind::Divide:	ctx.Idiv(*return_reg, tmp_reg, bitmode); break;
					case BinaryExprKind::Modulo:	LU_ASSERT(false); break;
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
			ctx.Movzx(*return_reg, *return_reg, BitMode_64, true);
		};
		auto CommonShiftCodegen		 = [&](BinaryExprKind kind)
		{
			if (!return_reg) return;
			if (rhs->GetExprKind() == ExprKind::IntLiteral)
			{
				IntLiteralAST* int_literal = AstCast<IntLiteralAST>(rhs.get());
				lhs->Codegen(ctx, *return_reg);
				if (kind == BinaryExprKind::ShiftLeft) ctx.Shl(*return_reg, (uint8)int_literal->GetValue(), bitmode);
				else if (kind == BinaryExprKind::ShiftRight) ctx.Sar(*return_reg, (uint8)int_literal->GetValue(), bitmode);
			}
			else
			{
				register_t reg2 = ctx.AllocateRegister();
				rhs->Codegen(ctx, reg2);
				lhs->Codegen(ctx, *return_reg);
				if (kind == BinaryExprKind::ShiftLeft)  ctx.Shl(*return_reg, reg2, bitmode);
				if (kind == BinaryExprKind::ShiftRight) ctx.Sar(*return_reg, reg2, bitmode); //#todo check if unsigned
				ctx.FreeRegister(reg2);
			}
		};
		auto CommonBitCodegen		 = [&](BinaryExprKind kind)
		{
			if (!return_reg) return;

			bool const lhs_is_pointer = IsPointerLikeType(lhs->GetType());
			bool const rhs_is_pointer = IsPointerLikeType(rhs->GetType());
			bool const is_pointer_arithmetic = lhs_is_pointer || rhs_is_pointer;
			LU_ASSERT(!is_pointer_arithmetic);

			if (rhs->GetExprKind() == ExprKind::IntLiteral)
			{
				IntLiteralAST* int_literal = AstCast<IntLiteralAST>(rhs.get());
				int32 value = (int32)int_literal->GetValue();
				lhs->Codegen(ctx, *return_reg);

				switch (kind)
				{
				case BinaryExprKind::BitAnd:		ctx.And(*return_reg, value, bitmode); break;
				case BinaryExprKind::BitOr:			ctx.Or(*return_reg, value, bitmode); break;
				case BinaryExprKind::BitXor:		ctx.Xor(*return_reg, value, bitmode); break;
				}
			}
			else
			{
				register_t tmp_reg = ctx.AllocateRegister();
				rhs->Codegen(ctx, tmp_reg);
				lhs->Codegen(ctx, *return_reg);
				switch (kind)
				{
				case BinaryExprKind::BitAnd:		ctx.And(*return_reg, tmp_reg, bitmode); break;
				case BinaryExprKind::BitOr:			ctx.Or(*return_reg, tmp_reg, bitmode); break;
				case BinaryExprKind::BitXor:		ctx.Xor(*return_reg, tmp_reg, bitmode); break;
				}
				ctx.FreeRegister(tmp_reg);
			}
		};

		switch (op)
		{
		case BinaryExprKind::Assign:
		{
			LU_ASSERT_MSG(lhs->IsLValue(), "Cannot assign to rvalue!");
			if (lhs->GetExprKind() == ExprKind::DeclRef)
			{
				DeclRefAST* decl_ref = AstCast<DeclRefAST>(lhs.get());
				char const* var_name = decl_ref->GetName().data();
				int32 local_offset = decl_ref->GetLocalOffset();
				register_t rbp = ctx.GetStackFrameRegister();
				mem_ref_t mem_ref{ .base_reg = rbp, .displacement = local_offset };
				bool global = decl_ref->IsGlobal();

				if (rhs->GetExprKind() == ExprKind::IntLiteral)
				{
					IntLiteralAST* int_literal = AstCast<IntLiteralAST>(rhs.get());
					if (global) ctx.Mov(var_name, (int32)int_literal->GetValue(), bitmode);
					else ctx.Mov(mem_ref, (int32)int_literal->GetValue(), bitmode);
				}
				else if (rhs->GetExprKind() == ExprKind::FunctionCall)
				{
					FunctionCallAST* func_call = AstCast<FunctionCallAST>(rhs.get());
					register_t ret_reg = ctx.AllocateReturnRegister();
					func_call->Codegen(ctx);
					if (global) ctx.Mov(var_name, ret_reg, bitmode);
					else ctx.Mov(mem_ref, ret_reg, bitmode);
					ctx.FreeRegister(ret_reg);
				}
				else
				{
					register_t rhs_reg = return_reg ? *return_reg : ctx.AllocateRegister();
					rhs->Codegen(ctx, rhs_reg);
					if (global) ctx.Mov(var_name, rhs_reg, bitmode);
					else ctx.Mov(mem_ref, rhs_reg, bitmode);
					if (!return_reg) ctx.FreeRegister(rhs_reg);
				}
			}
			else if (lhs->GetExprKind() == ExprKind::Unary)
			{
				UnaryExprAST* unary_expr = AstCast<UnaryExprAST>(lhs.get());
				if (unary_expr->GetUnaryKind() == UnaryExprKind::Dereference)
				{
					register_t rhs_reg = return_reg ? *return_reg : ctx.AllocateRegister();
					rhs->Codegen(ctx, rhs_reg);

					register_t address_reg = ctx.AllocateRegister();
					unary_expr->GetOperand()->Codegen(ctx, address_reg);
					mem_ref_t mem_ref{ .base_reg = address_reg };
					ctx.Mov(mem_ref, rhs_reg, bitmode);

					ctx.FreeRegister(address_reg);
					if (!return_reg) ctx.FreeRegister(rhs_reg);
				}
			}
		}
		break;
		case BinaryExprKind::Add:
		case BinaryExprKind::Subtract:
		case BinaryExprKind::Multiply:
		case BinaryExprKind::Divide:
		case BinaryExprKind::Modulo:
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
		case BinaryExprKind::Comma:
		{
			lhs->Codegen(ctx, return_reg);
			rhs->Codegen(ctx, return_reg);
		}
		break;
		case BinaryExprKind::LogicalAnd:
		{
			if (return_reg)
			{
				register_t cond_reg = ctx.AllocateRegister();
				ctx.GenerateLabelId();
				lhs->Codegen(ctx, cond_reg);
				size_t lhs_type_size = lhs->GetType()->GetSize();
				ctx.Cmp(cond_reg, int64(0), ConvertToBitMode(lhs_type_size));
				ctx.Jmp("L_false", Condition::Equal);
				rhs->Codegen(ctx, cond_reg);
				size_t rhs_type_size = rhs->GetType()->GetSize();
				ctx.Cmp(cond_reg, int64(0), ConvertToBitMode(rhs_type_size));
				ctx.Jmp("L_false", Condition::Equal);
				ctx.Mov(*return_reg, int64(1), BitMode_64);
				ctx.Jmp("L_end");
				ctx.Label("L_false");
				ctx.Mov(*return_reg, int64(0), BitMode_64);
				ctx.Label("L_end");
			}
		}
		break;
		case BinaryExprKind::LogicalOr:
		{
			if (return_reg)
			{
				register_t cond_reg = ctx.AllocateRegister();
				ctx.GenerateLabelId();
				lhs->Codegen(ctx, cond_reg);
				size_t lhs_type_size = lhs->GetType()->GetSize();
				ctx.Cmp(cond_reg, int64(0), ConvertToBitMode(lhs_type_size));
				ctx.Jmp("L_true", Condition::NotEqual);
				rhs->Codegen(ctx, cond_reg);
				size_t rhs_type_size = rhs->GetType()->GetSize();
				ctx.Cmp(cond_reg, int64(0), ConvertToBitMode(rhs_type_size));
				ctx.Jmp("L_true", Condition::NotEqual);
				ctx.Mov(*return_reg, int64(0), BitMode_64);
				ctx.Jmp("L_end");
				ctx.Label("L_true");
				ctx.Mov(*return_reg, int64(1), BitMode_64);
				ctx.Label("L_end");
			}
		}
		break;
		case BinaryExprKind::ShiftLeft:
		case BinaryExprKind::ShiftRight:
		{
			CommonShiftCodegen(op);
		}
		break;
		case BinaryExprKind::BitAnd:
		case BinaryExprKind::BitOr:
		case BinaryExprKind::BitXor:
		{
			CommonBitCodegen(op);
		}
		break;
		}
	}

	void DeclRefAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		if (!return_reg) return;

		size_t type_size = GetType()->GetSize();
		if (!symbol.global)
		{
			if (IsArrayType(GetType()))
			{
				register_t rbp = ctx.GetStackFrameRegister();
				mem_ref_t mem_ref{ .base_reg = rbp, .displacement = local_offset };
				ctx.Lea(*return_reg, mem_ref);
			}
			else
			{
				BitMode bitmode = ConvertToBitMode(type_size);

				register_t rbp = ctx.GetStackFrameRegister();
				mem_ref_t mem_ref{ .base_reg = rbp, .displacement = local_offset };
				ctx.Mov(*return_reg, mem_ref, bitmode);
			}
			return;
		}
		BitMode bitmode = ConvertToBitMode(type_size);
		if (IsArrayType(GetType())) ctx.Mov(*return_reg, GetName().data(), BitMode_64, true);
		else if (return_reg) ctx.Mov(*return_reg, GetName().data(), bitmode);
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
		ctx.Cmp(cond_reg, int64(0), BitMode_8);
		ctx.Jmp("L_else", Condition::Equal);
		then_stmt->Codegen(ctx);
		ctx.Jmp("L_end");
		ctx.Label("L_else");
		if (else_stmt) else_stmt->Codegen(ctx);
		ctx.Label("L_end");
	}

	void ReturnStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		register_t reg = ctx.AllocateReturnRegister();
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
		ctx.Cmp(cond_reg, int64(0), BitMode_8);
		ctx.Jmp("L_end", Condition::Equal);
		body_stmt->Codegen(ctx);
		ctx.Jmp("L_start");
		ctx.Label("L_end");
		ctx.FreeRegister(cond_reg);
	}

	void DeclStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
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
		ctx.Cmp(cond_reg, int64(0), BitMode_8);
		ctx.Jmp("L_end", Condition::Equal);
		body_stmt->Codegen(ctx);
		iter_expr->Codegen(ctx);
		ctx.Jmp("L_start");
		ctx.Label("L_end");
		ctx.FreeRegister(cond_reg);
	}

	void FunctionCallAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		for (size_t i = 0; i < func_args.size(); ++i)
		{
			register_t arg_reg = ctx.AllocateFunctionArgumentRegister(i);
			func_args[i]->Codegen(ctx, arg_reg);
		}
		if (func_expr->GetExprKind() == ExprKind::DeclRef)
		{
			LU_ASSERT(IsFunctionType(func_expr->GetType()));
			FunctionType const& func_type = TypeCast<FunctionType>(func_expr->GetType());
			QualifiedType const& ret_type = func_type.GetReturnType();
			size_t type_size = ret_type->GetSize();
			IdentifierAST* func_id = AstCast<IdentifierAST>(func_expr.get());
			ctx.CallFunction(func_id->GetName().data());
			if (return_reg)
			{
				register_t func_reg = ctx.AllocateReturnRegister();
				ctx.Mov(*return_reg, func_reg, ConvertToBitMode(type_size));
				ctx.FreeRegister(func_reg);
			}
		}
		else LU_ASSERT(false);
	}
}


