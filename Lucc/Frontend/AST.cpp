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

	//misc

	bool ExprAST::IsAssignable() const
	{
		if (!IsLValue()) return false;
		if (!type->IsComplete() || type.IsConst() || type->Is(PrimitiveTypeKind::Array)) return false;
		return true;
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

		for (auto it = local_variables.rbegin(); it != local_variables.rend(); ++it)
		{
			VarDeclAST const* local_var = *it;
			int32 alignment = (int32)local_var->GetSymbol().qtype->GetAlign();
			bottom += (int32)local_var->GetSymbol().qtype->GetSize();
			bottom = AlignTo(bottom, alignment);
			local_var->SetLocalOffset(-bottom);
		}
		stack_size = AlignTo(bottom, 16);
	}

	void CompoundStmtAST::AddStatement(std::unique_ptr<StmtAST>&& stmt)
	{
		statements.push_back(std::move(stmt));
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
		if (!IsGlobal())
		{
			if (init_expr)
			{
				LU_ASSERT(local_offset != 0);
				size_t type_size = sym.qtype->GetSize();
				mem_ref_t mem_ref{ .base_reg = ctx.GetStackFrameRegister(), .displacement = local_offset };
				if (init_expr->GetExprKind() == ExprKind::IntLiteral)
				{
					IntLiteralAST* int_literal = AstCast<IntLiteralAST>(init_expr.get());
					ctx.Mov(mem_ref, (int32)int_literal->GetValue(), ConvertToBitMode(type_size));
				}
				else
				{
					register_t init_reg = return_reg ? *return_reg : ctx.AllocateRegister();
					init_expr->Codegen(ctx, init_reg);
					ctx.Mov(mem_ref, init_reg, ConvertToBitMode(type_size));
					if (!return_reg) ctx.FreeRegister(init_reg);
				}
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
				else ctx.DeclareVariable(name.c_str(), is_static, ConvertToBitMode(type_size));
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
		
		for (uint16 i = 0; i < std::min(ctx.GetFunctionArgsInRegisters(), param_decls.size()); ++i)
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
				else if (operand->GetExprKind() == ExprKind::Unary) //#todo fix this
				{
					UnaryExprAST* unary_expr = AstCast<UnaryExprAST>(operand.get());
					LU_ASSERT(unary_expr->GetUnaryKind() == UnaryExprKind::Dereference);
					ExprAST* dereference_operand = unary_expr->GetOperand();

					register_t address_reg = ctx.AllocateRegister();
					dereference_operand->Codegen(ctx, address_reg);
					mem_ref_t mem_ref{ .base_reg = address_reg };
					
					if (op == UnaryExprKind::PreIncrement) ctx.Inc(mem_ref, bitmode);
					else ctx.Dec(mem_ref, bitmode);
					if (return_reg) ctx.Mov(*return_reg, mem_ref, bitmode);
					ctx.FreeRegister(address_reg);
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
				else if (operand->GetExprKind() == ExprKind::Unary)
				{
					UnaryExprAST* unary_expr = AstCast<UnaryExprAST>(operand.get());
					LU_ASSERT(unary_expr->GetUnaryKind() == UnaryExprKind::Dereference);
					ExprAST* dereference_operand = unary_expr->GetOperand();

					register_t address_reg = ctx.AllocateRegister();
					dereference_operand->Codegen(ctx, address_reg);
					mem_ref_t mem_ref{ .base_reg = address_reg };
					if (return_reg) ctx.Mov(*return_reg, mem_ref, bitmode);
					if (op == UnaryExprKind::PreIncrement) ctx.Inc(mem_ref, bitmode);
					else ctx.Dec(mem_ref, bitmode);
					ctx.FreeRegister(address_reg);
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
				
				register_t tmp_reg = ctx.AllocateRegister();
				if (lhs_is_pointer && rhs_is_pointer)
				{
					LU_ASSERT(kind == BinaryExprKind::Subtract);
					lhs->Codegen(ctx, *return_reg);
					rhs->Codegen(ctx, tmp_reg);
				}
				else if (lhs_is_pointer)
				{
					QualifiedType const& decayed_type = ValueTransformation(lhs->GetType());
					LU_ASSERT(IsPointerType(decayed_type));
					PointerType const& pointer_type = TypeCast<PointerType>(decayed_type);
					lhs->Codegen(ctx, *return_reg);
					rhs->Codegen(ctx, tmp_reg);
					ctx.Imul(tmp_reg, tmp_reg, (int32)pointer_type.PointeeType()->GetSize(), bitmode);
				}
				else
				{
					QualifiedType const& decayed_type = ValueTransformation(rhs->GetType());
					LU_ASSERT(IsPointerType(decayed_type));
					PointerType const& pointer_type = TypeCast<PointerType>(decayed_type);
					rhs->Codegen(ctx, *return_reg);
					lhs->Codegen(ctx, tmp_reg);
					ctx.Imul(tmp_reg, tmp_reg, (int32)pointer_type.PointeeType()->GetSize(), bitmode);
				}

				switch (kind)
				{
				case BinaryExprKind::Add:		ctx.Add(*return_reg, tmp_reg, bitmode); break;
				case BinaryExprKind::Subtract:  ctx.Sub(*return_reg, tmp_reg, bitmode); break;
				}
				ctx.FreeRegister(tmp_reg);
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
				uint64 label_id = ctx.GenerateLabelId();

				register_t cond_reg = ctx.AllocateRegister();
				lhs->Codegen(ctx, cond_reg);
				size_t lhs_type_size = lhs->GetType()->GetSize();
				ctx.Cmp(cond_reg, int64(0), ConvertToBitMode(lhs_type_size));
				ctx.Jmp("L_false", label_id, Condition::Equal);
				rhs->Codegen(ctx, cond_reg);
				size_t rhs_type_size = rhs->GetType()->GetSize();
				ctx.Cmp(cond_reg, int64(0), ConvertToBitMode(rhs_type_size));
				ctx.Jmp("L_false", label_id, Condition::Equal);
				ctx.Mov(*return_reg, int64(1), BitMode_64);
				ctx.Jmp("L_end", label_id);
				ctx.Label("L_false", label_id);
				ctx.Mov(*return_reg, int64(0), BitMode_64);
				ctx.Label("L_end", label_id);
			}
		}
		break;
		case BinaryExprKind::LogicalOr:
		{
			if (return_reg)
			{
				uint64 label_id = ctx.GenerateLabelId();

				register_t cond_reg = ctx.AllocateRegister();
				lhs->Codegen(ctx, cond_reg);
				size_t lhs_type_size = lhs->GetType()->GetSize();
				ctx.Cmp(cond_reg, int64(0), ConvertToBitMode(lhs_type_size));
				ctx.Jmp("L_true", label_id, Condition::NotEqual);
				rhs->Codegen(ctx, cond_reg);
				size_t rhs_type_size = rhs->GetType()->GetSize();
				ctx.Cmp(cond_reg, int64(0), ConvertToBitMode(rhs_type_size));
				ctx.Jmp("L_true", label_id, Condition::NotEqual);
				ctx.Mov(*return_reg, int64(0), BitMode_64);
				ctx.Jmp("L_end", label_id);
				ctx.Label("L_true", label_id);
				ctx.Mov(*return_reg, int64(1), BitMode_64);
				ctx.Label("L_end", label_id);
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

	void TernaryExprAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		static char const* false_label = "L_false";
		static char const* end_label = "L_end";
		uint64 label_id = ctx.GenerateLabelId();

		register_t cond_reg = ctx.AllocateRegister();
		cond_expr->Codegen(ctx, cond_reg);
		ctx.Cmp(cond_reg, int64(0), BitMode_8);
		ctx.Jmp(false_label, label_id, Condition::Equal);
		true_expr->Codegen(ctx, return_reg);
		ctx.Jmp(end_label, label_id);
		ctx.Label(false_label, label_id);
		false_expr->Codegen(ctx, return_reg);
		ctx.Label(end_label, label_id);
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
		static char const* else_label = "L_else";
		static char const* end_label = "L_end";
		uint64 label_id = ctx.GenerateLabelId();

		register_t cond_reg = ctx.AllocateRegister();
		condition->Codegen(ctx, cond_reg);
		ctx.Cmp(cond_reg, int64(0), BitMode_8);
		ctx.Jmp(else_label, label_id, Condition::Equal);
		then_stmt->Codegen(ctx);
		ctx.Jmp(end_label, label_id);
		ctx.Label(else_label, label_id);
		if (else_stmt) else_stmt->Codegen(ctx);
		ctx.Label(end_label, label_id);
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
		static char const* start_label = "L_start";
		static char const* end_label = "L_end";
		uint64 label_id = ctx.GenerateLabelId();
		for (ContinueStmtAST* continue_stmt : continue_stmts) continue_stmt->SetLabel(start_label, label_id);
		for (BreakStmtAST* break_stmt : break_stmts) break_stmt->SetLabel(end_label, label_id);

		ctx.Label(start_label, label_id);
		register_t cond_reg = ctx.AllocateRegister();
		condition->Codegen(ctx, cond_reg);
		ctx.Cmp(cond_reg, int64(0), BitMode_8);
		ctx.Jmp(end_label, label_id, Condition::Equal);
		body_stmt->Codegen(ctx);
		ctx.Jmp(start_label, label_id);
		ctx.Label(end_label, label_id);
		ctx.FreeRegister(cond_reg);
	}

	void DoWhileStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		static char const* start_label = "L_start";
		static char const* end_label = "L_end";
		uint64 label_id = ctx.GenerateLabelId();
		for (ContinueStmtAST* continue_stmt : continue_stmts) continue_stmt->SetLabel(start_label, label_id);
		for (BreakStmtAST* break_stmt : break_stmts) break_stmt->SetLabel(end_label, label_id);

		ctx.Label(start_label, label_id);
		body_stmt->Codegen(ctx);
		register_t cond_reg = ctx.AllocateRegister();
		condition->Codegen(ctx, cond_reg);
		ctx.Cmp(cond_reg, int64(0), BitMode_8);
		ctx.Jmp(end_label, label_id, Condition::Equal);
		ctx.Jmp(start_label, label_id);
		ctx.Label(end_label, label_id);
		ctx.FreeRegister(cond_reg);
	}

	void DeclStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		for (auto const& decl : decls) decl->Codegen(ctx);
	}

	void ForStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		static char const* start_label = "L_start";
		static char const* end_label = "L_end";
		static char const* iter_label = "L_iter";
		uint64 label_id = ctx.GenerateLabelId();
		for (ContinueStmtAST* continue_stmt : continue_stmts) continue_stmt->SetLabel(iter_label, label_id);
		for (BreakStmtAST* break_stmt : break_stmts) break_stmt->SetLabel(end_label, label_id);

		if (init_stmt) init_stmt->Codegen(ctx);
		ctx.Label(start_label, label_id);
		register_t cond_reg = ctx.AllocateRegister();
		if (cond_expr) cond_expr->Codegen(ctx, cond_reg);
		ctx.Cmp(cond_reg, int64(0), BitMode_8);
		ctx.Jmp(end_label, label_id, Condition::Equal);
		if(body_stmt) body_stmt->Codegen(ctx);
		ctx.Label(iter_label, label_id);
		if(iter_expr) iter_expr->Codegen(ctx);
		ctx.Jmp(start_label, label_id);
		ctx.Label(end_label, label_id);
		ctx.FreeRegister(cond_reg);
	}

	void FunctionCallAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		for (uint16 i = 0; i < func_args.size(); ++i)
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

	void BreakStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		ctx.Jmp(label_name.c_str(), label_id);
	}

	void ContinueStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		ctx.Jmp(label_name.c_str(), label_id);
	}

	void GotoStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		ctx.Jmp(goto_label.c_str());
	}

	void LabelStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		ctx.Label(label_name.c_str());
	}

	void SwitchStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		static char const* end_label = "L_end";
		uint64 label_id = ctx.GenerateLabelId();
		for (BreakStmtAST* break_stmt : break_stmts) break_stmt->SetLabel(end_label, label_id);
		for (CaseStmtAST* case_stmt : case_stmts) case_stmt->SetSwitchId(label_id);
		
		register_t cond_reg = ctx.AllocateRegister();
		condition->Codegen(ctx, cond_reg);

		BitMode bitmode = ConvertToBitMode(condition->GetType()->GetSize());

		CaseStmtAST* default_case = nullptr;
		for (CaseStmtAST* case_stmt : case_stmts)
		{
			if (!case_stmt->IsDefault())
			{
				ctx.Cmp(cond_reg, case_stmt->GetValue(), bitmode);
				ctx.Jmp(case_stmt->GetLabel().data(), label_id, Condition::Equal);
			}
			else
			{
				LU_ASSERT(!default_case);
				default_case = case_stmt;
			}
		}
		if(default_case) ctx.Jmp(default_case->GetLabel().data(), label_id);
		ctx.Jmp(end_label, label_id);
		body_stmt->Codegen(ctx);
		ctx.Label(end_label, label_id);
		ctx.FreeRegister(cond_reg);
	}

	void CaseStmtAST::Codegen(ICodegenContext& ctx, std::optional<register_t> return_reg) const
	{
		ctx.Label(label_name.c_str(), switch_id);
	}

}


