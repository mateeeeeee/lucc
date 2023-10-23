#pragma once
#include "Frontend/AST.h"
#include "x86_64Context.h"

namespace lucc
{
	namespace
	{
		namespace mov_cast
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
				Mov,
				Movzx,
				Movsx,
				Movsxd
			};
			constexpr MovType cast_table[CastTypeCount][CastTypeCount] =
			{
				//from    i8    i16   i32    i64      u8     u16   u32      u64     //to
					   {Mov,   Mov,   Mov,    Mov, Mov,   Mov,   Mov, Mov },  // i8  
					   {Movsx, Mov,   Mov,    Mov, Movzx, Mov,   Mov, Mov },  // i16
					   {Movsx, Movsx, Mov,    Mov, Movzx, Movzx, Mov, Mov },  // i32
					   {Movsx, Movsx, Movsxd, Mov, Movzx, Movzx, Mov, Mov },  // i64

					   {Mov,   Mov,   Mov,    Mov, Mov,   Mov,   Mov, Mov },  // u8
					   {Movsx, Mov,   Mov,    Mov, Movzx, Mov,   Mov, Mov },  // u16
					   {Movsx, Movsx, Mov,    Mov, Movzx, Movzx, Mov, Mov },  // u32
					   {Movsx, Movsx, Movsxd, Mov, Movzx, Movzx, Mov, Mov },  // u64
			};

			MovType GetCastMovType(QualifiedType const& from, QualifiedType const& to)
			{
				CastTableIdx from_type = _GetCastTableIdx(from);
				CastTableIdx to_type = _GetCastTableIdx(to);
				return cast_table[to_type][from_type];
			}
		}

		template<typename To, typename From>
			requires std::is_base_of_v<NodeAST, To>&& std::is_base_of_v<NodeAST, From>
		To* dynamic_ast_cast(From* from)
		{
			return dynamic_cast<To*>(from);
		}
		template<typename To, typename From>
			requires std::is_base_of_v<NodeAST, To>&& std::is_base_of_v<NodeAST, From>
		To const* dynamic_ast_cast(From const* from)
		{
			return dynamic_cast<To const*>(from);
		}
		template<typename To, typename From>
			requires std::is_base_of_v<NodeAST, To>&& std::is_base_of_v<NodeAST, From>
		To* ast_cast(From* from)
		{
			return static_cast<To*>(from);
		}
		template<typename To, typename From>
			requires std::is_base_of_v<NodeAST, To>&& std::is_base_of_v<NodeAST, From>
		To const* ast_cast(From const* from)
		{
			return static_cast<To const*>(from);
		}
	}

	class VarDeclVisitor : public ASTVisitor
	{
	public:
		VarDeclVisitor(FunctionDecl* func_ref) : func_ref(func_ref) {}
		virtual void Visit(VariableDecl const& node, uint32 depth) override
		{
			func_ref->AddLocalDecl(&node);
		}

	private:
		FunctionDecl* func_ref;
	};
	class FunctionCallVisitor : public ASTVisitor
	{
	public:
		FunctionCallVisitor(FunctionDecl* func_ref) : func_ref(func_ref) {}
		virtual void Visit(FunctionCallExpr const& node, uint32 depth) override
		{
			func_ref->AddFuncCallExpr(&node);
		}

	private:
		FunctionDecl* func_ref;
	};

	void FunctionDecl::AssignLocalOffsets()
	{
		FunctionCallVisitor func_call_visitor(this);
		body->Accept(func_call_visitor, 0);
		VarDeclVisitor var_decl_visitor(this);
		body->Accept(var_decl_visitor, 0);

		int32 top = 16;
		for (uint32 i = ARGUMENTS_PASSED_BY_REGISTERS; i < (uint32)param_decls.size(); ++i)
		{
			VariableDecl* param = param_decls[i].get();
			top = LU_ALIGN(top, 8);
			param->SetLocalOffset(top);
			top += (int32)param->GetSymbol().qtype->GetSize();
		}

		for (uint32 i = 0; i < std::min<uint32>(ARGUMENTS_PASSED_BY_REGISTERS, (uint32)param_decls.size()); ++i)
		{
			VariableDecl* param = param_decls[i].get();
			top = LU_ALIGN(top, 8);
			param->SetLocalOffset(top);
			top += (int32)param->GetSymbol().qtype->GetSize();
		}

		int32 bottom = 0;
		int32 local_stack_space = 0;
		for (auto it = local_variables.rbegin(); it != local_variables.rend(); ++it)
		{
			VariableDecl const* local_var = *it;
			int32 local_var_align = (int32)local_var->GetSymbol().qtype->GetAlign();
			int32 local_var_size = (int32)local_var->GetSymbol().qtype->GetSize();
			bottom += local_var_size;
			local_stack_space += local_var_size;
			bottom = LU_ALIGN(bottom, local_var_align);
			local_var->SetLocalOffset(-bottom);
		}
		stack_size = LU_ALIGN(local_stack_space, 16);
	}
	void TranslationUnit::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		for (auto const& decl : declarations) decl->Codegen(ctx);
	}
	void VariableDecl::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		if (!IsGlobal())
		{
			if (init_expr)
			{
				LU_ASSERT(local_offset != 0);
				uint32 type_size = sym.qtype->GetSize();
				BitCount bitcount = GetBitCount(type_size);
				Result var_res(Register::RBP, local_offset);
				if (init_expr->GetExprKind() == ExprKind::IntLiteral)
				{
					IntLiteral* int_literal = ast_cast<IntLiteral>(init_expr.get());
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
			DeclSymbol const& symbol = GetSymbol();
			if (IsArrayType(symbol.qtype))
			{
				ArrayDeclCG array_decl{};
				array_decl.name = name.c_str();
				array_decl.is_static = symbol.storage == Storage::Static;
				array_decl.is_extern = symbol.storage == Storage::Extern;
				array_decl.is_const = symbol.qtype.IsConst();

				ArrayType const& arr_type = type_cast<ArrayType>(symbol.qtype);
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
						init_value = init_expr->EvaluateConstexpr();
						var_decl.init_value = &init_value;
					}
					else {} //#todo address of another global variable, dereference of another pointer global variable
				}
				ctx.DeclareVariable(var_decl);
			}
		}
	}
	void FunctionDecl::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		FunctionDeclCG func_decl
		{
			.name = name.c_str(),
			.is_static = sym.storage == Storage::Static,
			.is_extern = sym.storage == Storage::Extern,
			.is_definition = IsDefinition()
		};
		ctx.DeclareFunction(func_decl);
		if (!IsDefinition()) return;

		ctx.SaveFrameRegister();

		for (uint32 i = 0; i < std::min<uint32>(ARGUMENTS_PASSED_BY_REGISTERS, (uint32)param_decls.size()); ++i)
		{
			VariableDecl* param_var = param_decls[i].get();
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

	void ExprStmt::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		if (expr) expr->Codegen(ctx, result);
	}
	void CompoundStmt::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		for (auto& stmt : statements) stmt->Codegen(ctx);
	}
	void IfStmt::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		static char const* else_label = "L_else";
		static char const* end_label = "L_end";
		uint64 label_id = ctx.GenerateLabelId();

		Register cond_reg = ctx.AllocateRegister();
		cond_expr->Codegen(ctx, &cond_reg);
		ctx.Cmp(cond_reg, int64(0), BitCount_8);
		ctx.Jmp(else_label, label_id, ConditionCode::E);
		then_stmt->Codegen(ctx);
		ctx.Jmp(end_label, label_id);
		ctx.Label(else_label, label_id);
		if (else_stmt) else_stmt->Codegen(ctx);
		ctx.Label(end_label, label_id);
		ctx.FreeRegister(cond_reg);
	}
	void ReturnStmt::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		//#todo handle cast when return type and type of return expr are not the same

		if (ret_expr->GetExpr()->GetExprKind() == ExprKind::FunctionCall)
		{
			ret_expr->Codegen(ctx);
			ctx.JumpToReturn();
		}
		else
		{
			Register tmp_reg = ctx.AllocateRegister();
			ret_expr->Codegen(ctx, &tmp_reg);
			Register return_reg = ctx.GetReturnRegister();
			ctx.Mov(return_reg, tmp_reg, GetBitCount(ret_expr->GetExpr()->GetType()->GetSize()));
			ctx.JumpToReturn();
			ctx.FreeRegister(return_reg);
		}
	}
	void WhileStmt::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		static char const* start_label = "L_start";
		static char const* end_label = "L_end";
		uint64 label_id = ctx.GenerateLabelId();
		for (ContinueStmt* continue_stmt : continue_stmts) continue_stmt->SetLabel(start_label, label_id);
		for (BreakStmt* break_stmt : break_stmts) break_stmt->SetLabel(end_label, label_id);

		ctx.Label(start_label, label_id);
		Register cond_reg = ctx.AllocateRegister();
		cond_expr->Codegen(ctx, &cond_reg);
		ctx.Cmp(cond_reg, (int64)0, BitCount_8);
		ctx.Jmp(end_label, label_id, ConditionCode::E);
		body_stmt->Codegen(ctx);
		ctx.Jmp(start_label, label_id);
		ctx.Label(end_label, label_id);
		ctx.FreeRegister(cond_reg);
	}
	void DoWhileStmt::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		static char const* start_label = "L_start";
		static char const* end_label = "L_end";
		uint64 label_id = ctx.GenerateLabelId();
		for (ContinueStmt* continue_stmt : continue_stmts) continue_stmt->SetLabel(start_label, label_id);
		for (BreakStmt* break_stmt : break_stmts) break_stmt->SetLabel(end_label, label_id);

		ctx.Label(start_label, label_id);
		body_stmt->Codegen(ctx);
		Register cond_reg = ctx.AllocateRegister();
		cond_expr->Codegen(ctx, &cond_reg);
		ctx.Cmp(cond_reg, int64(0), BitCount_8);
		ctx.Jmp(end_label, label_id, ConditionCode::E);
		ctx.Jmp(start_label, label_id);
		ctx.Label(end_label, label_id);
		ctx.FreeRegister(cond_reg);
	}
	void DeclStmt::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		for (auto const& decl : decls) decl->Codegen(ctx);
	}
	void ForStmt::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		static char const* start_label = "L_start";
		static char const* end_label = "L_end";
		static char const* iter_label = "L_iter";
		uint64 label_id = ctx.GenerateLabelId();
		for (ContinueStmt* continue_stmt : continue_stmts) continue_stmt->SetLabel(iter_label, label_id);
		for (BreakStmt* break_stmt : break_stmts) break_stmt->SetLabel(end_label, label_id);

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
	void BreakStmt::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		ctx.Jmp(label_name.c_str(), label_id);
	}
	void ContinueStmt::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		ctx.Jmp(label_name.c_str(), label_id);
	}
	void GotoStmt::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		ctx.Jmp(goto_label.c_str());
	}
	void LabelStmt::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		ctx.Label(label_name.c_str());
	}
	void SwitchStmt::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		static char const* end_label = "L_end";
		uint64 label_id = ctx.GenerateLabelId();
		for (BreakStmt* break_stmt : break_stmts) break_stmt->SetLabel(end_label, label_id);
		for (CaseStmt* case_stmt : case_stmts) case_stmt->SetSwitchId(label_id);

		Register cond_reg = ctx.AllocateRegister();
		cond_expr->Codegen(ctx, &cond_reg);

		BitCount bitmode = GetBitCount(cond_expr->GetType()->GetSize());

		CaseStmt* default_case = nullptr;
		for (CaseStmt* case_stmt : case_stmts)
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
	void CaseStmt::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		ctx.Label(label_name.c_str(), switch_id);
	}

	void UnaryExpr::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		uint32 type_size = GetType()->GetSize();
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
				uint32 type_size = 0;
				if (IsPointerType(operand->GetType()))
				{
					PointerType const& ptr_type = type_cast<PointerType>(operand->GetType());
					type_size = ptr_type.PointeeType()->GetSize();
				}
				else if (IsArrayType(operand->GetType()))
				{
					ArrayType const& array_type = type_cast<ArrayType>(operand->GetType());
					type_size = array_type.GetElementType()->GetSize();
				}
				LU_ASSERT(type_size);

				if (operand->GetExprKind() == ExprKind::DeclRef)
				{
					DeclRefExpr* decl_ref = ast_cast<DeclRefExpr>(operand.get());
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
					DeclRefExpr* decl_ref = ast_cast<DeclRefExpr>(operand.get());
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
					UnaryExpr* unary_expr = ast_cast<UnaryExpr>(operand.get());
					LU_ASSERT(unary_expr->GetUnaryKind() == UnaryExprKind::Dereference);
					Expr const* dereference_operand = unary_expr->GetOperand();

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
				uint32 type_size = 0;
				if (IsPointerType(operand->GetType()))
				{
					PointerType const& ptr_type = type_cast<PointerType>(operand->GetType());
					type_size = ptr_type.PointeeType()->GetSize();
				}
				else if (IsArrayType(operand->GetType()))
				{
					ArrayType const& array_type = type_cast<ArrayType>(operand->GetType());
					type_size = array_type.GetElementType()->GetSize();
				}
				LU_ASSERT(type_size);

				if (operand->GetExprKind() == ExprKind::DeclRef)
				{
					DeclRefExpr* decl_ref = ast_cast<DeclRefExpr>(operand.get());
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
				uint32 type_size = operand->GetType()->GetSize();
				BitCount bitcount = GetBitCount(type_size);
				if (operand->GetExprKind() == ExprKind::DeclRef)
				{
					DeclRefExpr* decl_ref = ast_cast<DeclRefExpr>(operand.get());
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
					UnaryExpr* unary_expr = ast_cast<UnaryExpr>(operand.get());
					LU_ASSERT(unary_expr->GetUnaryKind() == UnaryExprKind::Dereference);
					Expr const* dereference_operand = unary_expr->GetOperand();

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
					IntLiteral* literal = ast_cast<IntLiteral>(operand.get());
					ctx.Mov(*result, literal->GetValue(), bitcount);
					if (op == UnaryExprKind::Minus) ctx.Neg(*result, bitcount);
				}
				else if (operand->GetExprKind() == ExprKind::DeclRef)
				{
					DeclRefExpr* decl_ref = ast_cast<DeclRefExpr>(operand.get());
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
					DeclRefExpr* decl_ref = ast_cast<DeclRefExpr>(operand.get());
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
					IntLiteral* literal = ast_cast<IntLiteral>(operand.get());
					ctx.Mov(*result, literal->GetValue(), bitcount);
					ctx.Not(*result, bitcount);
				}
				else if (operand->GetExprKind() == ExprKind::DeclRef)
				{
					DeclRefExpr* decl_ref = ast_cast<DeclRefExpr>(operand.get());
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
	void BinaryExpr::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		uint32 type_size = GetType()->GetSize();
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

					if (kind == BinaryExprKind::Subtract)
					{
						if (lhs_is_pointer && rhs_is_pointer)
						{
							QualifiedType const& decayed_type = ValueTransformation(lhs->GetType());
							LU_ASSERT(IsPointerType(decayed_type));
							PointerType const& pointer_type = type_cast<PointerType>(decayed_type);
							Register tmp_reg = ctx.AllocateRegister();
							lhs->Codegen(ctx, result);
							rhs->Codegen(ctx, &tmp_reg);
							ctx.Sub(*result, tmp_reg, bitmode);
							ctx.Mov(tmp_reg, pointer_type.PointeeType()->GetSize(), BitCount_64);
							ctx.Idiv(*result, tmp_reg, BitCount_64);
							ctx.FreeRegister(tmp_reg);
						}
						else if (lhs_is_pointer)
						{
							QualifiedType const& decayed_type = ValueTransformation(lhs->GetType());
							LU_ASSERT(IsPointerType(decayed_type));
							PointerType const& pointer_type = type_cast<PointerType>(decayed_type);
							Register tmp_reg = ctx.AllocateRegister();
							lhs->Codegen(ctx, result);
							rhs->Codegen(ctx, &tmp_reg);
							ctx.Imul(tmp_reg, tmp_reg, (int32)pointer_type.PointeeType()->GetSize(), bitmode);
							ctx.Sub(*result, tmp_reg, bitmode);
							ctx.FreeRegister(tmp_reg);
						}
						else LU_ASSERT(false);
					}
					else if (kind == BinaryExprKind::Add)
					{
						LU_ASSERT(!(lhs_is_pointer && rhs_is_pointer));
						if (lhs_is_pointer)
						{
							QualifiedType const& decayed_type = ValueTransformation(lhs->GetType());
							LU_ASSERT(IsPointerType(decayed_type));
							PointerType const& pointer_type = type_cast<PointerType>(decayed_type);

							Register tmp_reg = ctx.AllocateRegister();
							lhs->Codegen(ctx, result);
							rhs->Codegen(ctx, &tmp_reg);
							ctx.Imul(tmp_reg, tmp_reg, (int32)pointer_type.PointeeType()->GetSize(), bitmode);
							ctx.Add(*result, tmp_reg, bitmode);
							ctx.FreeRegister(tmp_reg);
						}
						else if (rhs_is_pointer)
						{
							QualifiedType const& decayed_type = ValueTransformation(rhs->GetType());
							LU_ASSERT(IsPointerType(decayed_type));
							PointerType const& pointer_type = type_cast<PointerType>(decayed_type);

							Register tmp_reg = ctx.AllocateRegister();
							rhs->Codegen(ctx, result);
							lhs->Codegen(ctx, &tmp_reg);
							ctx.Imul(tmp_reg, tmp_reg, (int32)pointer_type.PointeeType()->GetSize(), bitmode);
							ctx.Add(*result, tmp_reg, bitmode);
							ctx.FreeRegister(tmp_reg);
						}
						else LU_ASSERT(false);
					}

				}
				else
				{
					if (rhs->GetExprKind() == ExprKind::IntLiteral)
					{
						IntLiteral* int_literal = ast_cast<IntLiteral>(rhs.get());
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
						rhs->Codegen(ctx, result);
						Register tmp_reg = ctx.AllocateRegister();
						lhs->Codegen(ctx, &tmp_reg);
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

				bool const lhs_is_unsigned = IsUnsignedType(lhs->GetType());
				bool const rhs_is_unsigned = IsUnsignedType(rhs->GetType());
				bool const is_unsigned = lhs_is_unsigned || rhs_is_unsigned;

				if (rhs->GetExprKind() == ExprKind::IntLiteral)
				{
					IntLiteral* int_literal = ast_cast<IntLiteral>(rhs.get());
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

				if (is_unsigned)
				{
					switch (kind)
					{
					case BinaryExprKind::Less:			ctx.Set(*result, ConditionCode::B); break;
					case BinaryExprKind::LessEqual:		ctx.Set(*result, ConditionCode::BE); break;
					case BinaryExprKind::Greater:		ctx.Set(*result, ConditionCode::A); break;
					case BinaryExprKind::GreaterEqual:	ctx.Set(*result, ConditionCode::AE); break;
					case BinaryExprKind::Equal:			ctx.Set(*result, ConditionCode::E); break;
					case BinaryExprKind::NotEqual:		ctx.Set(*result, ConditionCode::NE); break;
					}
				}
				else
				{
					switch (kind)
					{
					case BinaryExprKind::Less:			ctx.Set(*result, ConditionCode::L); break;
					case BinaryExprKind::LessEqual:		ctx.Set(*result, ConditionCode::LE); break;
					case BinaryExprKind::Greater:		ctx.Set(*result, ConditionCode::G); break;
					case BinaryExprKind::GreaterEqual:	ctx.Set(*result, ConditionCode::GE); break;
					case BinaryExprKind::Equal:			ctx.Set(*result, ConditionCode::E); break;
					case BinaryExprKind::NotEqual:		ctx.Set(*result, ConditionCode::NE); break;
					}
				}
				ctx.Movzx(*result, *result, BitCount_64, true);

			};
		auto CommonShiftCodegen = [&](BinaryExprKind kind)
			{
				if (!result) return;
				if (rhs->GetExprKind() == ExprKind::IntLiteral)
				{
					IntLiteral* int_literal = ast_cast<IntLiteral>(rhs.get());
					lhs->Codegen(ctx, result);
					if (kind == BinaryExprKind::ShiftLeft) ctx.Shl(*result, (uint8)int_literal->GetValue(), bitmode);
					else if (kind == BinaryExprKind::ShiftRight)
					{
						bool lhs_unsigned = IsUnsignedType(lhs->GetType());
						lhs_unsigned ? ctx.Shr(*result, (uint8)int_literal->GetValue(), bitmode) : ctx.Sar(*result, (uint8)int_literal->GetValue(), bitmode);
					}
				}
				else
				{
					rhs->Codegen(ctx, result);
					Register tmp_reg = ctx.AllocateRegister();
					lhs->Codegen(ctx, &tmp_reg);
					if (kind == BinaryExprKind::ShiftLeft)  ctx.Shl(*result, tmp_reg, bitmode);
					if (kind == BinaryExprKind::ShiftRight)
					{
						bool lhs_unsigned = IsUnsignedType(lhs->GetType());
						lhs_unsigned ? ctx.Shr(*result, tmp_reg, bitmode) : ctx.Sar(*result, tmp_reg, bitmode);
					}
					ctx.FreeRegister(tmp_reg);
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
					IntLiteral* int_literal = ast_cast<IntLiteral>(rhs.get());
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
					rhs->Codegen(ctx, result);
					Register tmp_reg = ctx.AllocateRegister();
					lhs->Codegen(ctx, &tmp_reg);
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
				DeclRefExpr* decl_ref = ast_cast<DeclRefExpr>(lhs.get());
				char const* var_name = decl_ref->GetName().data();
				int32 local_offset = decl_ref->GetLocalOffset();
				Result mem_ref(RBP, local_offset);
				bool global = decl_ref->IsGlobal();

				if (rhs->GetExprKind() == ExprKind::IntLiteral)
				{
					IntLiteral* int_literal = ast_cast<IntLiteral>(rhs.get());
					if (global) ctx.Mov(var_name, int_literal->GetValue(), bitmode);
					else ctx.Mov(mem_ref, int_literal->GetValue(), bitmode);
				}
				else if (rhs->GetExprKind() == ExprKind::FunctionCall)
				{
					FunctionCallExpr* func_call = ast_cast<FunctionCallExpr>(rhs.get());
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
				UnaryExpr* unary_expr = ast_cast<UnaryExpr>(lhs.get());
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
				uint32 lhs_type_size = lhs->GetType()->GetSize();
				ctx.Cmp(cond_reg, int64(0), GetBitCount(lhs_type_size));
				ctx.Jmp("L_false", label_id, ConditionCode::E);
				rhs->Codegen(ctx, &cond_reg);
				uint32 rhs_type_size = rhs->GetType()->GetSize();
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
				uint32 lhs_type_size = lhs->GetType()->GetSize();
				ctx.Cmp(cond_reg, int64(0), GetBitCount(lhs_type_size));
				ctx.Jmp("L_true", label_id, ConditionCode::NE);
				rhs->Codegen(ctx, &cond_reg);
				uint32 rhs_type_size = rhs->GetType()->GetSize();
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
	void TernaryExpr::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
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
	void DeclRefExpr::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		if (!result) return;

		uint32 type_size = GetType()->GetSize();
		BitCount bitmode = GetBitCount(type_size);

		if (!IsGlobal())
		{
			if (IsArrayType(GetType()))
			{
				Result res(RBP, GetLocalOffset());
				ctx.Lea(*result, res);
			}
			else
			{
				Result res(RBP, GetLocalOffset());
				ctx.Mov(*result, res, bitmode);
			}
			return;
		}
		if (IsArrayType(GetType()) || IsFunctionType(GetType())) ctx.MovOffset(*result, GetName().data());
		else ctx.Mov(*result, GetName().data(), bitmode);
	}
	void IntLiteral::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		uint32 type_size = GetType()->GetSize();
		BitCount bitmode = GetBitCount(type_size);
		if (result) ctx.Mov(*result, value, bitmode);
	}
	void FunctionCallExpr::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
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
				uint32 type_size = type->GetSize();
				uint32 type_align = type->GetAlign();
				shadow_space_stack = LU_ALIGN(shadow_space_stack, (uint32)type_align);
				shadow_space_stack += (uint32)type_size;
			}
		}
		if (shadow_space_stack < 32) shadow_space_stack = 32;
		shadow_space_stack = LU_ALIGN(shadow_space_stack, 16u);

		//uint32 pushed_regs = ctx.SaveVolatileRegisters();
		//if (pushed_regs & 1) shadow_space_stack += 8;

		ctx.AllocateStack(shadow_space_stack);

		for (uint32 i = 0; i < std::min<uint32>((uint32)func_args.size(), ARGUMENTS_PASSED_BY_REGISTERS); ++i)
		{
			Register arg_reg = ctx.GetCallRegister(i);
			func_args[i]->Codegen(ctx, &arg_reg);
			ctx.FreeRegister(arg_reg);
		}

		uint32 pushed_args = 0;
		for (int32 i = (int32)func_args.size() - 1; i >= std::min((int32)func_args.size(), (int32)ARGUMENTS_PASSED_BY_REGISTERS); --i)
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
			uint32 type_size = type->GetSize();
			DeclRefExpr* func_ref = ast_cast<DeclRefExpr>(func_expr.get());
			if (IsFunctionType(func_ref->GetType()))
			{
				ctx.Call(func_ref->GetDecl()->GetName().data());
				if (result)
				{
					Register func_reg = ctx.GetReturnRegister();
					ctx.Mov(*result, func_reg, GetBitCount(type_size));
					ctx.FreeRegister(func_reg);
				}
			}
			else if (IsFunctionPointerType(func_ref->GetType()))
			{
				Register reg = ctx.AllocateRegister();
				func_ref->Codegen(ctx, &reg);
				ctx.Call(reg);
				ctx.FreeRegister(reg);
				if (result)
				{
					Register func_reg = ctx.GetReturnRegister();
					ctx.Mov(*result, func_reg, GetBitCount(type_size));
					ctx.FreeRegister(func_reg);
				}
			}
		}
		else LU_ASSERT(false);

		//ctx.RestoreVolatileRegisters();
		ctx.FreeStack(shadow_space_stack + (pushed_args & 1 ? 8 : 0));
	}
	void StringLiteral::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		if (!result) return;
		std::string str_label = ctx.DeclareString(str.c_str());
		ctx.MovOffset(*result, str_label.c_str());
	}
	void CastExpr::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
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

		mov_cast::MovType mov_type = mov_cast::GetCastMovType(from_type, to_type);
		switch (mov_type)
		{
		case mov_cast::Mov:   ctx.Mov(cast_reg, tmp_reg, bitcount); break;
		case mov_cast::Movzx: ctx.Movzx(cast_reg, tmp_reg, bitcount, rhs8bit); break;
		case mov_cast::Movsx: ctx.Movsx(cast_reg, tmp_reg, bitcount, rhs8bit); break;
		case mov_cast::Movsxd:ctx.Movsxd(cast_reg, tmp_reg); break;
		default: break;
		}
		ctx.FreeRegister(tmp_reg);
		if (!result) ctx.FreeRegister(cast_reg);
	}
	void MemberRefExpr::Codegen(x86_64Context& ctx, Register* result /*= nullptr*/) const
	{
		if (!result) return;

		uint32 type_size = GetType()->GetSize();
		BitCount bitmode = GetBitCount(type_size);

		if (!decl->GetSymbol().global)
		{
			if (IsArrayType(GetType()))
			{
				Result res(RBP, GetLocalOffset());
				ctx.Lea(*result, res);
			}
			else
			{
				Result res(RBP, GetLocalOffset());
				ctx.Mov(*result, res, bitmode);
			}
			return;
		}

		if (IsArrayType(GetType()) || IsFunctionType(GetType()))
		{

		}
		else
		{

		}
	}
}