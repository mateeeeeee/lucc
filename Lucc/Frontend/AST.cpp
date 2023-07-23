#include "AST.h"
#include "Diagnostics/Diagnostics.h"
#include "Core/Defines.h"
#include "Backend/x86_64Context.h"


namespace lucc
{
	namespace
	{
		template<typename T>
		inline T AlignTo(T n, T align) { return (n + align - 1) / align * align; }

		namespace cast
		{
			enum CastTableIdx
			{
				i8, i16, i32, i64, u8, u16, u32, u64, CastTypeCount
			};
			CastTableIdx _GetCastTableIdx(QualifiedType const& type)
			{
				LU_ASSERT(IsScalarType(type));
				if (IsArithmeticType(type))
				{
					ArithmeticType const& arith_type = type->As<ArithmeticType>();
					auto flags = arith_type.GetFlags();
					switch (flags)
					{
					case ArithmeticType::Bool: 
					case ArithmeticType::Char: return arith_type.IsUnsigned() ? CastTableIdx::u8 : CastTableIdx::i8;
					case ArithmeticType::Short: return arith_type.IsUnsigned() ? CastTableIdx::u16 : CastTableIdx::i16;
					case ArithmeticType::Int:  
					case ArithmeticType::Long: return arith_type.IsUnsigned() ? CastTableIdx::u32 : CastTableIdx::i32;
					case ArithmeticType::LongLong: return arith_type.IsUnsigned() ? CastTableIdx::u64 : CastTableIdx::i64;
					}
				}
				return CastTableIdx::u64;
			}

			enum MovType
			{
				NoMov,
				Mov,
				Movzx,
				Movsx,
				Movsxd
			};

			constexpr MovType cast_table[CastTypeCount][CastTypeCount] =
			{
				// i8   i16     i32     i64     u8     u16     u32     u64
				{NoMov, NoMov, NoMov, Movsxd, Movzx, Movzx, NoMov, Movsxd },  // i8
				{Movsx, NoMov, NoMov, Movsxd, Movzx, Movzx, NoMov, Movsxd },  // i16
				{Movsx, Movsx, NoMov, Movsxd, Movzx, Movzx, NoMov, Movsxd },  // i32
				{Movsx, Movsx, NoMov, NoMov,  Movzx, Movzx, NoMov, NoMov  },  // i64

				{Movsx, NoMov, NoMov, Movsxd, NoMov, NoMov, NoMov, Movsxd },  // u8
				{Movsx, Movsx, NoMov, Movsxd, Movzx, NoMov, NoMov, Movsxd },  // u16
				{Movsx, Movsx, NoMov, Mov,	  Movzx, Movzx, NoMov, Mov	 },	  // u32
				{Movsx, Movsx, NoMov, NoMov,  Movzx, Movzx, NoMov, NoMov  },  // u64
			};

			MovType GetCastMovType(QualifiedType const& from, QualifiedType const& to)
			{
				CastTableIdx from_type = _GetCastTableIdx(from);
				CastTableIdx to_type = _GetCastTableIdx(to);
				return cast_table[to_type][from_type];
			}
		}
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

	class FunctionCallVisitorAST : public INodeVisitorAST
	{
	public:
		FunctionCallVisitorAST(FunctionDeclAST* func_ref) : func_ref(func_ref) {}
		virtual void Visit(FunctionCallAST const& node, size_t depth) override
		{
			func_ref->AddFunctionCall(&node);
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

	void CastExprAST::Accept(INodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		operand->Accept(visitor, depth + 1);
	}

	/// Constexpr

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

	bool IntLiteralAST::IsConstexpr() const
	{
		return true;
	}

	int64 IntLiteralAST::EvaluateConstexpr() const
	{
		return value;
	}

	/// Expression types

	void UnaryExprAST::SetExpressionType()
	{
		diag::SetLocation(loc);
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

	void BinaryExprAST::SetExpressionType()
	{
		diag::SetLocation(loc);
		switch (op)
		{
		case BinaryExprKind::Assign:
		{
			rhs = GetAssignExpr(std::move(rhs), lhs->GetType());
			SetType(rhs->GetType()); break;
		}
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

	void CastExprAST::SetCastType()
	{
		QualifiedType operand_type = ValueTransformation(operand->GetType());
		if (IsVoidType(GetType()))
		{
			// If the target type is void, then expression is evaluated for its
			// side-effects and its returned value is discarded.
		}
		else if (!IsScalarType(GetType()))
		{
			Report(diag::cast_invalid_type);
		}
		else if (!IsScalarType(operand_type))
		{
			Report(diag::cast_invalid_type);
		}
		else if (IsPointerType(GetType()) && IsFloatingType(operand_type))
		{
			Report(diag::cast_invalid_type);
		}
		else if (IsPointerType(operand_type) && IsFloatingType(GetType()))
		{
			Report(diag::cast_invalid_type);
		}
		else
		{
			if ((IsObjectPointerType(GetType()) && IsFunctionPointerType(operand_type)) ||
				(IsObjectPointerType(operand_type) && IsFunctionPointerType(GetType())))
			{

			}
			SetType(RemoveQualifiers(GetType()));
		}
	}

	/// Misc
	void FunctionDeclAST::AssignLocalOffsets()
	{
		FunctionCallVisitorAST func_call_visitor(this);
		body->Accept(func_call_visitor, 0);
		VarDeclVisitorAST var_decl_visitor(this);
		body->Accept(var_decl_visitor, 0);

		int32 top = 16;
		for (uint64 i = ARGUMENTS_PASSED_BY_REGISTERS; i < param_decls.size(); ++i)
		{
			VarDeclAST* param = param_decls[i].get();
			top = AlignTo(top, 8);
			param->SetLocalOffset(top);
			top += (int32)param->GetSymbol().qtype->GetSize();
		}

		int32 bottom = 0;
		for (uint64 i = 0; i < std::min(ARGUMENTS_PASSED_BY_REGISTERS, param_decls.size()); ++i)
		{
			VarDeclAST* param = param_decls[i].get();
			int32 alignment = (int32)param->GetSymbol().qtype->GetAlign();
			bottom += (int32)param->GetSymbol().qtype->GetSize();
			bottom = AlignTo(bottom, alignment);
			param->SetLocalOffset(-bottom);
		}

		int32 local_stack_space = 0;
		for (auto it = local_variables.rbegin(); it != local_variables.rend(); ++it)
		{
			VarDeclAST const* local_var = *it;
			int32 local_var_align = (int32)local_var->GetSymbol().qtype->GetAlign();
			int32 local_var_size = (int32)local_var->GetSymbol().qtype->GetSize();
			bottom += local_var_size;
			local_stack_space += local_var_size;
			bottom = AlignTo(bottom, local_var_align);
			local_var->SetLocalOffset(-bottom);
		}
		stack_size = AlignTo(local_stack_space, 16);

		DeclRefVisitorAST decl_ref_visitor(this);
		body->Accept(decl_ref_visitor, 0);
	}

	/// Codegen

	void TranslationUnitAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		for (auto const& decl : declarations) decl->Codegen(ctx);
	}

	void VarDeclAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		if (!IsGlobal())
		{
			if (init_expr)
			{
				LU_ASSERT(local_offset != 0);
				size_t type_size = sym.qtype->GetSize();
				BitCount bitcount = GetBitCount(type_size);
				Result var_res(Register::RBP, local_offset);
				if (init_expr->GetExprKind() == ExprKind::IntLiteral)
				{
					IntLiteralAST* int_literal = AstCast<IntLiteralAST>(init_expr.get());
					ctx.Mov(var_res, int_literal->GetValue(), bitcount);
					if (result) ctx.Mov(*result, int_literal->GetValue(), bitcount);
				}
				else
				{
					if (result)
					{
						init_expr->Codegen(ctx, result);
						ctx.Mov(var_res, *result, bitcount);
					}
					else
					{
						Register reg = ctx.AllocateRegister();
						init_expr->Codegen(ctx, &reg);
						ctx.Mov(var_res, reg, bitcount);
						ctx.FreeRegister(reg);
					}
				}
			}
		}
		else
		{
			VarSymbol const& symbol = GetSymbol();
			if (IsArrayType(symbol.qtype))
			{
				ArrayDeclCG array_decl{};
				array_decl.name = name.c_str();
				array_decl.is_static = symbol.storage == Storage::Static;
				array_decl.is_extern = symbol.storage == Storage::Extern;
				array_decl.is_const = symbol.qtype.IsConst();

				ArrayType const& arr_type = TypeCast<ArrayType>(symbol.qtype);
				array_decl.array_size = arr_type.GetArraySize();
				array_decl.align = arr_type.GetElementType()->GetAlign();
				array_decl.bits = GetBitCount(arr_type.GetElementType()->GetSize());

				if (init_expr); //#todo array initialization
				else ctx.DeclareArray(array_decl);
			}
			else
			{
				VarDeclCG var_decl{};
				var_decl.name = name.c_str();
				var_decl.is_static = symbol.storage == Storage::Static;
				var_decl.is_extern = symbol.storage == Storage::Extern;
				var_decl.bits = GetBitCount(symbol.qtype->GetSize());
				var_decl.is_const = symbol.qtype.IsConst();
				var_decl.align = symbol.qtype->GetAlign();
				if (init_expr)
				{
					if (init_expr->IsConstexpr())
					{
						int64 init_value = init_expr->EvaluateConstexpr();
						var_decl.init_value = &init_value;
					}
					else {} //#todo address of another global variable, dereference of another pointer global variable
				}
				ctx.DeclareVariable(var_decl);
			}
		}

	}

	void FunctionDeclAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		FunctionDeclCG func_decl
		{
			.name = name.c_str(),
			.is_static = sym.storage == Storage::Static,
			.is_extern = sym.storage == Storage::Extern
		};
		if (!IsDefinition()) return;
		ctx.DeclareFunction(func_decl);

		ctx.SaveFrameRegister();

		for (uint16 i = 0; i < std::min(ARGUMENTS_PASSED_BY_REGISTERS, param_decls.size()); ++i)
		{
			VarDeclAST* param_var = param_decls[i].get();
			LU_ASSERT(param_var->GetLocalOffset() < 0);
			BitCount bitcount = GetBitCount(param_var->GetSymbol().qtype->GetSize());
			Register arg_reg = ctx.GetCallRegister(i);
			ctx.Mov(Result(RBP, param_var->GetLocalOffset()), arg_reg, bitcount);
			ctx.FreeRegister(arg_reg);
		}

		ctx.AllocateStack(stack_size);

		body->Codegen(ctx);
		ctx.Return();
		return;
	}

	void UnaryExprAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		size_t type_size = GetType()->GetSize();
		BitCount bitcount = GetBitCount(type_size);

		switch (op)
		{
		case UnaryExprKind::PreIncrement:
		case UnaryExprKind::PreDecrement:
		{
			LU_ASSERT(operand->IsLValue());
			bool const is_pointer_arithmetic = IsPointerLikeType(operand->GetType());
			if (is_pointer_arithmetic)
			{
				size_t type_size = 0;
				if (IsPointerType(operand->GetType()))
				{
					PointerType const& ptr_type = TypeCast<PointerType>(operand->GetType());
					type_size = ptr_type.PointeeType()->GetSize();
				}
				else if (IsArrayType(operand->GetType()))
				{
					ArrayType const& array_type = TypeCast<ArrayType>(operand->GetType());
					type_size = array_type.GetElementType()->GetSize();
				}
				LU_ASSERT(type_size);

				if (operand->GetExprKind() == ExprKind::DeclRef)
				{
					DeclRefAST* decl_ref = AstCast<DeclRefAST>(operand.get());
					if (decl_ref->IsGlobal())
					{
						char const* name = decl_ref->GetName().data();
						Result var_res(name);
						Register tmp_reg = ctx.AllocateRegister();
						ctx.MovOffset(tmp_reg, name);
						if (op == UnaryExprKind::PreIncrement)	    ctx.Add(tmp_reg, type_size, BitCount_64);
						else if (op == UnaryExprKind::PreDecrement) ctx.Sub(tmp_reg, type_size, BitCount_64);
						ctx.Mov(name, tmp_reg, BitCount_64);
						if (result) ctx.Mov(*result, tmp_reg, BitCount_64);
						ctx.FreeRegister(tmp_reg);
					}
					else
					{
						int32 local_offset = decl_ref->GetLocalOffset();
						Result var_res(Register::RBP, local_offset);

						Register tmp_reg = ctx.AllocateRegister();
						ctx.Lea(tmp_reg, var_res);
						if (op == UnaryExprKind::PreIncrement)	    ctx.Add(tmp_reg, type_size, BitCount_64);
						else if (op == UnaryExprKind::PreDecrement) ctx.Sub(tmp_reg, type_size, BitCount_64);
						ctx.Mov(var_res, tmp_reg, BitCount_64);
						if (result) ctx.Mov(*result, tmp_reg, BitCount_64);
						ctx.FreeRegister(tmp_reg);
					}
				}
				else LU_ASSERT(false);
			}
			else
			{
				if (operand->GetExprKind() == ExprKind::DeclRef)
				{
					DeclRefAST* decl_ref = AstCast<DeclRefAST>(operand.get());
					if (decl_ref->IsGlobal())
					{
						char const* name = decl_ref->GetName().data();
						if (op == UnaryExprKind::PreIncrement) ctx.Inc(name, bitcount);
						else ctx.Dec(name, bitcount);
						if (result) ctx.Mov(*result, name, bitcount);
					}
					else
					{
						int32 local_offset = decl_ref->GetLocalOffset();
						Result res(Register::RBP, local_offset);
						if (op == UnaryExprKind::PreIncrement) ctx.Inc(res, bitcount);
						else ctx.Dec(res, bitcount);
						if (result) ctx.Mov(*result, res, bitcount);
					}
				}
				else if (operand->GetExprKind() == ExprKind::Unary) //#todo fix this
				{
					UnaryExprAST* unary_expr = AstCast<UnaryExprAST>(operand.get());
					LU_ASSERT(unary_expr->GetUnaryKind() == UnaryExprKind::Dereference);
					ExprAST* dereference_operand = unary_expr->GetOperand();

					Register address_reg = ctx.AllocateRegister();
					dereference_operand->Codegen(ctx, &address_reg);

					Result mem_ref(address_reg, 0);
					if (op == UnaryExprKind::PreIncrement) ctx.Inc(mem_ref, bitcount);
					else ctx.Dec(mem_ref, bitcount);
					if (result) ctx.Mov(*result, mem_ref, bitcount);
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
				size_t type_size = 0;
				if (IsPointerType(operand->GetType()))
				{
					PointerType const& ptr_type = TypeCast<PointerType>(operand->GetType());
					type_size = ptr_type.PointeeType()->GetSize();
				}
				else if (IsArrayType(operand->GetType()))
				{
					ArrayType const& array_type = TypeCast<ArrayType>(operand->GetType());
					type_size = array_type.GetElementType()->GetSize();
				}
				LU_ASSERT(type_size);

				if (operand->GetExprKind() == ExprKind::DeclRef)
				{
					DeclRefAST* decl_ref = AstCast<DeclRefAST>(operand.get());
					if (decl_ref->IsGlobal())
					{
						char const* name = decl_ref->GetName().data();
						Register tmp_reg = ctx.AllocateRegister();
						ctx.MovOffset(tmp_reg, name);
						if (op == UnaryExprKind::PreIncrement)	   ctx.Add(tmp_reg, type_size, BitCount_64);
						else if (op == UnaryExprKind::PreDecrement) ctx.Sub(tmp_reg, type_size, BitCount_64);
						if (result) ctx.Mov(*result, tmp_reg, BitCount_64);
						ctx.Mov(name, tmp_reg, BitCount_64);
						ctx.FreeRegister(tmp_reg);
					}
					else
					{
						int32 local_offset = decl_ref->GetLocalOffset();
						Result res(Register::RBP, local_offset);
						Register tmp_reg = ctx.AllocateRegister();
						ctx.Lea(tmp_reg, res);
						if (op == UnaryExprKind::PreIncrement)	    ctx.Add(tmp_reg, type_size, BitCount_64);
						else if (op == UnaryExprKind::PreDecrement) ctx.Sub(tmp_reg, type_size, BitCount_64);
						if (result) ctx.Mov(*result, tmp_reg, BitCount_64);
						ctx.Mov(res, tmp_reg, BitCount_64);
						ctx.FreeRegister(tmp_reg);
					}
				}
				else LU_ASSERT(false);
			}
			else
			{
				size_t type_size = operand->GetType()->GetSize();
				BitCount bitcount = GetBitCount(type_size);
				if (operand->GetExprKind() == ExprKind::DeclRef)
				{
					DeclRefAST* decl_ref = AstCast<DeclRefAST>(operand.get());
					if (decl_ref->IsGlobal())
					{
						char const* name = decl_ref->GetName().data();
						if (result) ctx.Mov(*result, name, bitcount);
						if (op == UnaryExprKind::PostIncrement) ctx.Inc(name, bitcount);
						else ctx.Dec(name, bitcount);
					}
					else
					{
						int32 local_offset = decl_ref->GetLocalOffset();
						Result res(Register::RBP, local_offset);
						if (result) ctx.Mov(*result, res, bitcount);
						if (op == UnaryExprKind::PostIncrement) ctx.Inc(res, bitcount);
						else ctx.Dec(res, bitcount);
					}

				}
				else if (operand->GetExprKind() == ExprKind::Unary)
				{
					UnaryExprAST* unary_expr = AstCast<UnaryExprAST>(operand.get());
					LU_ASSERT(unary_expr->GetUnaryKind() == UnaryExprKind::Dereference);
					ExprAST* dereference_operand = unary_expr->GetOperand();

					Register address_reg(ctx.AllocateRegister());
					dereference_operand->Codegen(ctx, &address_reg);
					Result mem_ref(address_reg, 0);
					if (result) ctx.Mov(*result, mem_ref, bitcount);
					if (op == UnaryExprKind::PreIncrement) ctx.Inc(mem_ref, bitcount);
					else ctx.Dec(mem_ref, bitcount);
					ctx.FreeRegister(address_reg);
				}
				else LU_ASSERT(false);
			}
		}
		return;
		case UnaryExprKind::Plus:
		case UnaryExprKind::Minus:
		{
			if (result)
			{
				LU_ASSERT(!IsPointerType(operand->GetType()));
				if (operand->GetExprKind() == ExprKind::IntLiteral)
				{
					IntLiteralAST* literal = AstCast<IntLiteralAST>(operand.get());
					ctx.Mov(*result, literal->GetValue(), bitcount);
					if (op == UnaryExprKind::Minus) ctx.Neg(*result, bitcount);
				}
				else if (operand->GetExprKind() == ExprKind::DeclRef)
				{
					DeclRefAST* decl_ref = AstCast<DeclRefAST>(operand.get());
					if (decl_ref->IsGlobal())
					{
						char const* name = decl_ref->GetName().data();
						ctx.Mov(*result, name, bitcount);
						if (op == UnaryExprKind::Minus) ctx.Neg(*result, bitcount);
					}
					else
					{
						int32 local_offset = decl_ref->GetLocalOffset();
						Result res(RBP, local_offset);
						ctx.Mov(*result, res, bitcount);
						if (op == UnaryExprKind::Minus) ctx.Neg(*result, bitcount);
					}

				}
				else
				{
					operand->Codegen(ctx, result);
					if (op == UnaryExprKind::Minus) ctx.Neg(*result, bitcount);
				}
			}
		}
		return;
		case UnaryExprKind::Dereference:
		{
			if (result)
			{
				LU_ASSERT(IsPointerLikeType(operand->GetType()));
				Register address_reg = ctx.AllocateRegister();
				operand->Codegen(ctx, &address_reg);
				Result mem_ref(address_reg, 0);
				ctx.Mov(*result, mem_ref, GetBitCount(operand->GetType()->GetSize()));
				ctx.FreeRegister(address_reg);
			}
		}
		return;
		case UnaryExprKind::AddressOf:
		{
			if (result)
			{
				if (operand->GetExprKind() == ExprKind::DeclRef)
				{
					DeclRefAST* decl_ref = AstCast<DeclRefAST>(operand.get());
					if (decl_ref->IsGlobal())
					{
						char const* name = decl_ref->GetName().data();
						ctx.Lea(*result, name);
					}
					else
					{
						int32 local_offset = decl_ref->GetLocalOffset();
						Result res(RBP, local_offset);
						ctx.Lea(*result, res);
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
			if (result)
			{
				LU_ASSERT(!IsPointerLikeType(operand->GetType()));
				BitCount bitcount = GetBitCount(operand->GetType()->GetSize());
				if (operand->GetExprKind() == ExprKind::IntLiteral)
				{
					IntLiteralAST* literal = AstCast<IntLiteralAST>(operand.get());
					ctx.Mov(*result, literal->GetValue(), bitcount);
					ctx.Not(*result, bitcount);
				}
				else if (operand->GetExprKind() == ExprKind::DeclRef)
				{
					DeclRefAST* decl_ref = AstCast<DeclRefAST>(operand.get());
					if (decl_ref->IsGlobal())
					{
						char const* name = decl_ref->GetName().data();
						ctx.Mov(*result, name, bitcount);
						ctx.Not(*result, bitcount);
					}
					else
					{
						int32 local_offset = decl_ref->GetLocalOffset();
						Result res(Register::RBP, local_offset);
						ctx.Mov(*result, res, bitcount);
						ctx.Not(*result, bitcount);
					}
				}
				else
				{
					operand->Codegen(ctx, result);
					ctx.Not(*result, bitcount);
				}
			}
		}
		return;
		case UnaryExprKind::LogicalNot:
		{
			BitCount bitcount = GetBitCount(operand->GetType()->GetSize());
			if (result)
			{
				Register tmp_reg = ctx.AllocateRegister();
				operand->Codegen(ctx, &tmp_reg);
				ctx.Cmp(tmp_reg, int64(0), bitcount);
				ctx.Set(tmp_reg, ConditionCode::E);
				ctx.Movzx(*result, tmp_reg, BitCount_64, true);
				ctx.FreeRegister(tmp_reg);
			}
		}
		return;
		default:
			LU_ASSERT(false);
		}
	}

	void BinaryExprAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		size_t type_size = GetType()->GetSize();
		BitCount bitmode = GetBitCount(type_size);

		auto CommonArithmeticCodegen = [&](BinaryExprKind kind)
		{
			if (!result) return; //#todo doesnt cover cases such as i++ + j

			bool const lhs_is_pointer = IsPointerLikeType(lhs->GetType());
			bool const rhs_is_pointer = IsPointerLikeType(rhs->GetType());
			bool const is_pointer_arithmetic = lhs_is_pointer || rhs_is_pointer;

			if (is_pointer_arithmetic)
			{
				LU_ASSERT(kind == BinaryExprKind::Add || kind == BinaryExprKind::Subtract);

				Register tmp_reg = ctx.AllocateRegister();
				if (lhs_is_pointer && rhs_is_pointer)
				{
					LU_ASSERT(kind == BinaryExprKind::Subtract);
					lhs->Codegen(ctx, result);
					rhs->Codegen(ctx, &tmp_reg);
				}
				else if (lhs_is_pointer)
				{
					QualifiedType const& decayed_type = ValueTransformation(lhs->GetType());
					LU_ASSERT(IsPointerType(decayed_type));
					PointerType const& pointer_type = TypeCast<PointerType>(decayed_type);
					lhs->Codegen(ctx, result);
					rhs->Codegen(ctx, &tmp_reg);
					ctx.Imul(tmp_reg, tmp_reg, (int32)pointer_type.PointeeType()->GetSize(), bitmode);
				}
				else
				{
					QualifiedType const& decayed_type = ValueTransformation(rhs->GetType());
					LU_ASSERT(IsPointerType(decayed_type));
					PointerType const& pointer_type = TypeCast<PointerType>(decayed_type);
					rhs->Codegen(ctx, result);
					lhs->Codegen(ctx, &tmp_reg);
					ctx.Imul(tmp_reg, tmp_reg, (int32)pointer_type.PointeeType()->GetSize(), bitmode);
				}

				switch (kind)
				{
				case BinaryExprKind::Add:		ctx.Add(*result, tmp_reg, bitmode); break;
				case BinaryExprKind::Subtract:  ctx.Sub(*result, tmp_reg, bitmode); break;
				}
				ctx.FreeRegister(tmp_reg);
			}
			else
			{
				if (rhs->GetExprKind() == ExprKind::IntLiteral)
				{
					IntLiteralAST* int_literal = AstCast<IntLiteralAST>(rhs.get());
					int32 value = (int32)int_literal->GetValue();
					lhs->Codegen(ctx, result);

					switch (kind)
					{
					case BinaryExprKind::Add:		ctx.Add(*result, value, bitmode); break;
					case BinaryExprKind::Subtract:  ctx.Sub(*result, value, bitmode); break;
					case BinaryExprKind::Multiply:  ctx.Imul(*result, *result, value, bitmode); break;
					case BinaryExprKind::Divide:
					{
						Register tmp_reg = ctx.AllocateRegister();
						ctx.Mov(tmp_reg, value, bitmode);
						ctx.Idiv(*result, tmp_reg, bitmode);
					}
					break;
					case BinaryExprKind::Modulo:	LU_ASSERT_MSG(false, "Not yet implemented"); break;
					}
				}
				else
				{
					Register tmp_reg = ctx.AllocateRegister();
					rhs->Codegen(ctx, &tmp_reg);
					lhs->Codegen(ctx, result);
					switch (kind)
					{
					case BinaryExprKind::Add:		ctx.Add(*result, tmp_reg, bitmode);  break;
					case BinaryExprKind::Subtract:  ctx.Sub(*result, tmp_reg, bitmode);  break;
					case BinaryExprKind::Multiply:  ctx.Imul(*result, tmp_reg, bitmode); break;
					case BinaryExprKind::Divide:	ctx.Idiv(*result, tmp_reg, bitmode); break;
					case BinaryExprKind::Modulo:	LU_ASSERT_MSG(false, "Not yet implemented"); break;
					}
					ctx.FreeRegister(tmp_reg);
				}
			}
		};
		auto CommonComparisonCodegen = [&](BinaryExprKind kind)
		{
			if (!result) return;

			if (rhs->GetExprKind() == ExprKind::IntLiteral)
			{
				IntLiteralAST* int_literal = AstCast<IntLiteralAST>(rhs.get());
				Register reg = ctx.AllocateRegister();
				lhs->Codegen(ctx, &reg);
				ctx.Cmp(reg, int_literal->GetValue(), bitmode);
				ctx.FreeRegister(reg);
			}
			else
			{
				Register reg1 = ctx.AllocateRegister();
				Register reg2 = ctx.AllocateRegister();
				rhs->Codegen(ctx, &reg2);
				lhs->Codegen(ctx, &reg1);
				ctx.Cmp(reg1, reg2, bitmode);
				ctx.FreeRegister(reg2);
				ctx.FreeRegister(reg1);
			}
			switch (kind)
			{
			case BinaryExprKind::Less:			ctx.Set(*result, ConditionCode::L); break;
			case BinaryExprKind::LessEqual:		ctx.Set(*result, ConditionCode::LE); break;
			case BinaryExprKind::Greater:		ctx.Set(*result, ConditionCode::G); break;
			case BinaryExprKind::GreaterEqual:	ctx.Set(*result, ConditionCode::GE); break;
			case BinaryExprKind::Equal:			ctx.Set(*result, ConditionCode::E); break;
			case BinaryExprKind::NotEqual:		ctx.Set(*result, ConditionCode::NE); break;
			}
			ctx.Movzx(*result, *result, BitCount_64, true);

		};
		auto CommonShiftCodegen = [&](BinaryExprKind kind)
		{
			if (!result) return;
			if (rhs->GetExprKind() == ExprKind::IntLiteral)
			{
				IntLiteralAST* int_literal = AstCast<IntLiteralAST>(rhs.get());
				lhs->Codegen(ctx, result);
				if (kind == BinaryExprKind::ShiftLeft) ctx.Shl(*result, (uint8)int_literal->GetValue(), bitmode);
				else if (kind == BinaryExprKind::ShiftRight) ctx.Sar(*result, (uint8)int_literal->GetValue(), bitmode);
			}
			else
			{
				Register reg2 = ctx.AllocateRegister();
				rhs->Codegen(ctx, &reg2);
				lhs->Codegen(ctx, result);
				if (kind == BinaryExprKind::ShiftLeft)  ctx.Shl(*result, reg2, bitmode);
				if (kind == BinaryExprKind::ShiftRight) ctx.Sar(*result, reg2, bitmode); //#todo check if unsigned
				ctx.FreeRegister(reg2);
			}
		};
		auto CommonBitCodegen = [&](BinaryExprKind kind)
		{
			if (!result) return;

			bool const lhs_is_pointer = IsPointerLikeType(lhs->GetType());
			bool const rhs_is_pointer = IsPointerLikeType(rhs->GetType());
			bool const is_pointer_arithmetic = lhs_is_pointer || rhs_is_pointer;
			LU_ASSERT(!is_pointer_arithmetic);

			if (rhs->GetExprKind() == ExprKind::IntLiteral)
			{
				IntLiteralAST* int_literal = AstCast<IntLiteralAST>(rhs.get());
				int32 value = (int32)int_literal->GetValue();
				lhs->Codegen(ctx, result);

				switch (kind)
				{
				case BinaryExprKind::BitAnd: ctx.And(*result, value, bitmode); break;
				case BinaryExprKind::BitOr:	 ctx.Or(*result, value, bitmode); break;
				case BinaryExprKind::BitXor: ctx.Xor(*result, value, bitmode); break;
				}
			}
			else
			{
				Register tmp_reg = ctx.AllocateRegister();
				rhs->Codegen(ctx, &tmp_reg);
				lhs->Codegen(ctx, result);
				switch (kind)
				{
				case BinaryExprKind::BitAnd:		ctx.And(*result, tmp_reg, bitmode); break;
				case BinaryExprKind::BitOr:			ctx.Or(*result, tmp_reg, bitmode); break;
				case BinaryExprKind::BitXor:		ctx.Xor(*result, tmp_reg, bitmode); break;
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
				Result mem_ref(RBP, local_offset);
				bool global = decl_ref->IsGlobal();

				if (rhs->GetExprKind() == ExprKind::IntLiteral)
				{
					IntLiteralAST* int_literal = AstCast<IntLiteralAST>(rhs.get());
					if (global) ctx.Mov(var_name, int_literal->GetValue(), bitmode);
					else ctx.Mov(mem_ref, int_literal->GetValue(), bitmode);
				}
				else if (rhs->GetExprKind() == ExprKind::FunctionCall)
				{
					FunctionCallAST* func_call = AstCast<FunctionCallAST>(rhs.get());
					Register ret_reg = ctx.GetReturnRegister();
					func_call->Codegen(ctx);
					if (global) ctx.Mov(var_name, ret_reg, bitmode);
					else ctx.Mov(mem_ref, ret_reg, bitmode);
					ctx.FreeRegister(ret_reg);
				}
				else
				{
					Register rhs_reg = result ? *result : ctx.AllocateRegister();
					rhs->Codegen(ctx, &rhs_reg);
					if (global) ctx.Mov(var_name, rhs_reg, bitmode);
					else ctx.Mov(mem_ref, rhs_reg, bitmode);
					if (!result) ctx.FreeRegister(rhs_reg);
				}
			}
			else if (lhs->GetExprKind() == ExprKind::Unary)
			{
				UnaryExprAST* unary_expr = AstCast<UnaryExprAST>(lhs.get());
				if (unary_expr->GetUnaryKind() == UnaryExprKind::Dereference)
				{
					Register rhs_reg = result ? *result : ctx.AllocateRegister();
					rhs->Codegen(ctx, &rhs_reg);

					Register address_reg = ctx.AllocateRegister();
					unary_expr->GetOperand()->Codegen(ctx, &address_reg);
					Result mem_ref(address_reg, 0);
					ctx.Mov(mem_ref, rhs_reg, bitmode);
					ctx.FreeRegister(address_reg);
					if (!result) ctx.FreeRegister(rhs_reg);
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
			lhs->Codegen(ctx, result);
			rhs->Codegen(ctx, result);
		}
		break;
		case BinaryExprKind::LogicalAnd:
		{
			if (result)
			{
				uint64 label_id = ctx.GenerateLabelId();

				Register cond_reg = ctx.AllocateRegister();
				lhs->Codegen(ctx, &cond_reg);
				size_t lhs_type_size = lhs->GetType()->GetSize();
				ctx.Cmp(cond_reg, int64(0), GetBitCount(lhs_type_size));
				ctx.Jmp("L_false", label_id, ConditionCode::E);
				rhs->Codegen(ctx, &cond_reg);
				size_t rhs_type_size = rhs->GetType()->GetSize();
				ctx.Cmp(cond_reg, int64(0), GetBitCount(rhs_type_size));
				ctx.Jmp("L_false", label_id, ConditionCode::E);
				ctx.Mov(*result, int64(1), BitCount_64);
				ctx.Jmp("L_end", label_id);
				ctx.Label("L_false", label_id);
				ctx.Mov(*result, int64(0), BitCount_64);
				ctx.Label("L_end", label_id);
			}
		}
		break;
		case BinaryExprKind::LogicalOr:
		{
			if (result)
			{
				uint64 label_id = ctx.GenerateLabelId();

				Register cond_reg = ctx.AllocateRegister();
				lhs->Codegen(ctx, &cond_reg);
				size_t lhs_type_size = lhs->GetType()->GetSize();
				ctx.Cmp(cond_reg, int64(0), GetBitCount(lhs_type_size));
				ctx.Jmp("L_true", label_id, ConditionCode::NE);
				rhs->Codegen(ctx, &cond_reg);
				size_t rhs_type_size = rhs->GetType()->GetSize();
				ctx.Cmp(cond_reg, int64(0), GetBitCount(rhs_type_size));
				ctx.Jmp("L_true", label_id, ConditionCode::NE);
				ctx.Mov(*result, int64(0), BitCount_64);
				ctx.Jmp("L_end", label_id);
				ctx.Label("L_true", label_id);
				ctx.Mov(*result, int64(1), BitCount_64);
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

	void TernaryExprAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		static char const* false_label = "L_false";
		static char const* end_label = "L_end";
		uint64 label_id = ctx.GenerateLabelId();

		Register cond_reg = ctx.AllocateRegister();
		cond_expr->Codegen(ctx, &cond_reg);
		ctx.Cmp(cond_reg, int64(0), BitCount_8);
		ctx.Jmp(false_label, label_id, ConditionCode::E);
		true_expr->Codegen(ctx, result);
		ctx.Jmp(end_label, label_id);
		ctx.Label(false_label, label_id);
		false_expr->Codegen(ctx, result);
		ctx.Label(end_label, label_id);
	}

	void DeclRefAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		if (!result) return;

		size_t type_size = GetType()->GetSize();
		BitCount bitmode = GetBitCount(type_size);

		if (!IsGlobal())
		{
			if (IsArrayType(GetType()))
			{
				Result res(RBP, local_offset);
				ctx.Lea(*result, res);
			}
			else
			{
				Result res(RBP, local_offset);
				ctx.Mov(*result, res, bitmode);
			}
			return;
		}
		if (IsArrayType(GetType())) ctx.MovOffset(*result, GetName().data());
		else ctx.Mov(*result, GetName().data(), bitmode);
	}

	void IntLiteralAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		size_t type_size = GetType()->GetSize();
		BitCount bitmode = GetBitCount(type_size);
		if (result) ctx.Mov(*result, value, bitmode);
	}

	void ExprStmtAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		if(expr) expr->Codegen(ctx, result);
	}

	void CompoundStmtAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		for (auto& stmt : statements) stmt->Codegen(ctx);
	}

	void IfStmtAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		static char const* else_label = "L_else";
		static char const* end_label = "L_end";
		uint64 label_id = ctx.GenerateLabelId();

		Register cond_reg = ctx.AllocateRegister();
		condition->Codegen(ctx, &cond_reg);
		ctx.Cmp(cond_reg, int64(0), BitCount_8);
		ctx.Jmp(else_label, label_id, ConditionCode::E);
		then_stmt->Codegen(ctx);
		ctx.Jmp(end_label, label_id);
		ctx.Label(else_label, label_id);
		if (else_stmt) else_stmt->Codegen(ctx);
		ctx.Label(end_label, label_id);
		ctx.FreeRegister(cond_reg);
	}

	void ReturnStmtAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		//#todo handle cast when return type and type of return expr are not the same

		if (ret_expr->GetExpr()->GetExprKind() == ExprKind::FunctionCall)
		{
			ret_expr->Codegen(ctx);
			ctx.JumpToReturn();
		}
		else
		{
			Register return_reg = ctx.GetReturnRegister();
			ret_expr->Codegen(ctx, &return_reg);
			ctx.JumpToReturn();
			ctx.FreeRegister(return_reg);
		}
	}

	void WhileStmtAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		static char const* start_label = "L_start";
		static char const* end_label = "L_end";
		uint64 label_id = ctx.GenerateLabelId();
		for (ContinueStmtAST* continue_stmt : continue_stmts) continue_stmt->SetLabel(start_label, label_id);
		for (BreakStmtAST* break_stmt : break_stmts) break_stmt->SetLabel(end_label, label_id);

		ctx.Label(start_label, label_id);
		Register cond_reg = ctx.AllocateRegister();
		condition->Codegen(ctx, &cond_reg);
		ctx.Cmp(cond_reg, (int64)0, BitCount_8);
		ctx.Jmp(end_label, label_id, ConditionCode::E);
		body_stmt->Codegen(ctx);
		ctx.Jmp(start_label, label_id);
		ctx.Label(end_label, label_id);
		ctx.FreeRegister(cond_reg);
	}

	void DoWhileStmtAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		static char const* start_label = "L_start";
		static char const* end_label = "L_end";
		uint64 label_id = ctx.GenerateLabelId();
		for (ContinueStmtAST* continue_stmt : continue_stmts) continue_stmt->SetLabel(start_label, label_id);
		for (BreakStmtAST* break_stmt : break_stmts) break_stmt->SetLabel(end_label, label_id);

		ctx.Label(start_label, label_id);
		body_stmt->Codegen(ctx);
		Register cond_reg = ctx.AllocateRegister();
		condition->Codegen(ctx, &cond_reg);
		ctx.Cmp(cond_reg, int64(0), BitCount_8);
		ctx.Jmp(end_label, label_id, ConditionCode::E);
		ctx.Jmp(start_label, label_id);
		ctx.Label(end_label, label_id);
		ctx.FreeRegister(cond_reg);
	}

	void DeclStmtAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		for (auto const& decl : decls) decl->Codegen(ctx);
	}

	void ForStmtAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		static char const* start_label = "L_start";
		static char const* end_label = "L_end";
		static char const* iter_label = "L_iter";
		uint64 label_id = ctx.GenerateLabelId();
		for (ContinueStmtAST* continue_stmt : continue_stmts) continue_stmt->SetLabel(iter_label, label_id);
		for (BreakStmtAST* break_stmt : break_stmts) break_stmt->SetLabel(end_label, label_id);

		if (init_stmt) init_stmt->Codegen(ctx);
		ctx.Label(start_label, label_id);
		Register cond_reg = ctx.AllocateRegister();
		if (cond_expr) cond_expr->Codegen(ctx, &cond_reg);
		ctx.Cmp(cond_reg, int64(0), BitCount_8);
		ctx.Jmp(end_label, label_id, ConditionCode::E);
		if (body_stmt) body_stmt->Codegen(ctx);
		ctx.Label(iter_label, label_id);
		if (iter_expr) iter_expr->Codegen(ctx);
		ctx.Jmp(start_label, label_id);
		ctx.Label(end_label, label_id);
		ctx.FreeRegister(cond_reg);
	}

	void FunctionCallAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		//uint32 pushed_regs = 0;
		//ctx.SaveVolatileRegisters();
		//if (pushed_regs & 1) shadow_space_stack += 8;
		//ctx.RestoreVolatileRegisters();

		//#todo handle the case when some arg is a function call

		//shadow space
		uint32 shadow_space_stack = 0;
		for (uint16 i = 0; i < func_args.size(); ++i)
		{
			if (i < 4)
			{
				shadow_space_stack += 8;
			}
			else
			{
				auto const& type = func_args[i]->GetType();
				size_t type_size = type->GetSize();
				size_t type_align = type->GetAlign();
				shadow_space_stack = AlignTo(shadow_space_stack, (uint32)type_align);
				shadow_space_stack += (uint32)type_size;
			}
		}
		if (shadow_space_stack < 32) shadow_space_stack = 32;
		shadow_space_stack = AlignTo(shadow_space_stack, 16u);

		ctx.AllocateStack(shadow_space_stack);

		for (int32 i = 0; i < std::min(func_args.size(), ARGUMENTS_PASSED_BY_REGISTERS); ++i)
		{
			Register arg_reg = ctx.GetCallRegister(i);
			func_args[i]->Codegen(ctx, &arg_reg);
			ctx.FreeRegister(arg_reg);
		}

		uint32 pushed_args = 0;
		for (int32 i = (int32)func_args.size() - 1; i >= std::min(func_args.size(), ARGUMENTS_PASSED_BY_REGISTERS); --i)
		{
			Register arg_reg = ctx.AllocateRegister();
			func_args[i]->Codegen(ctx, &arg_reg);
			ctx.Push(arg_reg);
			ctx.FreeRegister(arg_reg);
			++pushed_args;
		}

		if (func_expr->GetExprKind() == ExprKind::DeclRef)
		{
			QualifiedType const& type = GetType();
			size_t type_size = type->GetSize();
			IdentifierAST* func_id = AstCast<IdentifierAST>(func_expr.get());
			ctx.Call(func_id->GetName().data());
			if (result)
			{
				Register func_reg = ctx.GetReturnRegister();
				ctx.Mov(*result, func_reg, GetBitCount(type_size));
				ctx.FreeRegister(func_reg);
			}
		}
		else LU_ASSERT(false);

		ctx.FreeStack(shadow_space_stack + (pushed_args & 1 ? 8 : 0));
	}

	void BreakStmtAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		ctx.Jmp(label_name.c_str(), label_id);
	}

	void ContinueStmtAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		ctx.Jmp(label_name.c_str(), label_id);
	}

	void GotoStmtAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		ctx.Jmp(goto_label.c_str());
	}

	void LabelStmtAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		ctx.Label(label_name.c_str());
	}

	void SwitchStmtAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		static char const* end_label = "L_end";
		uint64 label_id = ctx.GenerateLabelId();
		for (BreakStmtAST* break_stmt : break_stmts) break_stmt->SetLabel(end_label, label_id);
		for (CaseStmtAST* case_stmt : case_stmts) case_stmt->SetSwitchId(label_id);

		Register cond_reg = ctx.AllocateRegister();
		condition->Codegen(ctx, &cond_reg);

		BitCount bitmode = GetBitCount(condition->GetType()->GetSize());

		CaseStmtAST* default_case = nullptr;
		for (CaseStmtAST* case_stmt : case_stmts)
		{
			if (!case_stmt->IsDefault())
			{
				ctx.Cmp(cond_reg, case_stmt->GetValue(), bitmode);
				ctx.Jmp(case_stmt->GetLabel().data(), label_id, ConditionCode::E);
			}
			else
			{
				LU_ASSERT(!default_case);
				default_case = case_stmt;
			}
		}
		if (default_case) ctx.Jmp(default_case->GetLabel().data(), label_id);
		ctx.Jmp(end_label, label_id);
		body_stmt->Codegen(ctx);
		ctx.Label(end_label, label_id);
		ctx.FreeRegister(cond_reg);
	}

	void CaseStmtAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		ctx.Label(label_name.c_str(), switch_id);
	}

	void StringLiteralAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		if (!result) return;
		std::string str_label = ctx.DeclareString(str.c_str());
		ctx.MovOffset(*result, str_label.c_str());
	}

	void CastExprAST::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		if (GetType()->Is(TypeKind::Void))
		{
			operand->Codegen(ctx, result);
			return;
		}
		
		QualifiedType const& from_type = operand->GetType();
		QualifiedType const& to_type = GetType();
		BitCount bitcount = GetBitCount(to_type->GetSize());
		bool rhs8bit = from_type->GetSize() == 1;

		Register cast_reg = result ? *result : ctx.AllocateRegister();
		Register tmp_reg = ctx.AllocateRegister();
		operand->Codegen(ctx, &tmp_reg);

		cast::MovType mov_type = cast::GetCastMovType(from_type, to_type);
		switch (mov_type)
		{
		case cast::NoMov:
		case cast::Mov:   ctx.Mov(cast_reg, tmp_reg, bitcount); break;
		case cast::Movzx: ctx.Movzx(cast_reg, tmp_reg, bitcount, rhs8bit); break;
		case cast::Movsx: ctx.Movsx(cast_reg, tmp_reg, bitcount, rhs8bit); break;
		case cast::Movsxd:ctx.Movsxd(cast_reg, tmp_reg); break;
		default: break;
		}
		ctx.FreeRegister(tmp_reg);
		if (!result) ctx.FreeRegister(cast_reg);
	}
}