#include "AST.h"
#include "Diagnostics.h"

namespace lucc
{
	void TranslationUnit::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
		for (auto&& decl : declarations) decl->Accept(visitor, depth + 1);
	}

	void Decl::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
	}
	void VariableDecl::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
		if (init_expr) init_expr->Accept(visitor, depth + 1);
	}
	void FunctionDecl::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
		for (auto&& param : param_decls) param->Accept(visitor, depth + 1);
		if (body) body->Accept(visitor, depth + 1);
	}
	void TypedefDecl::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
	}

	void Stmt::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		LU_ASSERT(false);
	}
	void NullStmt::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
	}
	void ExprStmt::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
		if (expr) expr->Accept(visitor, depth + 1);
	}
	void CompoundStmt::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
		for (auto&& stmt : statements) stmt->Accept(visitor, depth + 1);
	}
	void DeclStmt::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
		for(auto&& decl : decls) decl->Accept(visitor, depth + 1);
	}
	void IfStmt::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		LU_ASSERT(cond_expr && then_stmt);
		visitor.Visit(*this, depth);
		cond_expr->Accept(visitor, depth + 1);
		then_stmt->Accept(visitor, depth + 1);
		if (else_stmt) else_stmt->Accept(visitor, depth + 1);
	}
	void WhileStmt::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		LU_ASSERT(condition && body_stmt);
		visitor.Visit(*this, depth);
		condition->Accept(visitor, depth + 1);
		body_stmt->Accept(visitor, depth + 1);
	}
	void DoWhileStmt::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		LU_ASSERT(condition && body_stmt);
		visitor.Visit(*this, depth);
		condition->Accept(visitor, depth + 1);
		body_stmt->Accept(visitor, depth + 1);
	}
	void SwitchStmt::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		LU_ASSERT(condition && body_stmt);
		visitor.Visit(*this, depth);
		condition->Accept(visitor, depth + 1);
		body_stmt->Accept(visitor, depth + 1);
	}
	void CaseStmt::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
	}
	void ForStmt::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		LU_ASSERT(body_stmt);
		visitor.Visit(*this, depth);
		if (init_stmt) init_stmt->Accept(visitor, depth + 1);
		if (cond_expr) cond_expr->Accept(visitor, depth + 1);
		if (iter_expr) iter_expr->Accept(visitor, depth + 1);
		body_stmt->Accept(visitor, depth + 1);
	}
	void ReturnStmt::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
		if (ret_expr) ret_expr->Accept(visitor, depth + 1);
	}
	void GotoStmt::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
	}
	void LabelStmt::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
	}
	void BreakStmt::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
	}
	void ContinueStmt::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
	}

	void Expr::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		LU_ASSERT(false);
	}
	void UnaryExpr::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
		operand->Accept(visitor, depth + 1);
	}
	void BinaryExpr::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
		lhs->Accept(visitor, depth + 1);
		rhs->Accept(visitor, depth + 1);
	}
	void TernaryExpr::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		LU_ASSERT(cond_expr && true_expr && false_expr);
		visitor.Visit(*this, depth);
		cond_expr->Accept(visitor, depth + 1);
		true_expr->Accept(visitor, depth + 1);
		false_expr->Accept(visitor, depth + 1);
	}
	void FunctionCallExpr::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
		func_expr->Accept(visitor, depth + 1);
		for (auto const& arg : func_args) arg->Accept(visitor, depth + 1);
	}
	void IntLiteral::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
	}
	void StringLiteral::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
	}
	void DeclRefExpr::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
	}
	void MemberRefExpr::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
	}
	void CastExpr::Accept(ASTVisitor& visitor, uint32 depth) const
	{
		visitor.Visit(*this, depth);
		operand->Accept(visitor, depth + 1);
	}


	bool UnaryExpr::IsConstexpr() const
	{
		return operand->IsConstexpr();
	}
	int64 UnaryExpr::EvaluateConstexpr() const
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

	bool BinaryExpr::IsConstexpr() const
	{
		return lhs->IsConstexpr() && rhs->IsConstexpr();
	}
	int64 BinaryExpr::EvaluateConstexpr() const
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

	bool TernaryExpr::IsConstexpr() const
	{
		if (!cond_expr->IsConstexpr()) return false;
		if (cond_expr->EvaluateConstexpr()) return true_expr->IsConstexpr();
		else return false_expr->IsConstexpr();
	}
	int64 TernaryExpr::EvaluateConstexpr() const
	{
		LU_ASSERT_MSG(IsConstexpr(), "Cannot call EvaluateConstexpr on Expr that isn't constexpr");
		if (cond_expr->EvaluateConstexpr()) return true_expr->EvaluateConstexpr();
		else return false_expr->EvaluateConstexpr();
	}

	bool IntLiteral::IsConstexpr() const
	{
		return true;
	}
	int64 IntLiteral::EvaluateConstexpr() const
	{
		return value;
	}


	void UnaryExpr::SetExpressionType()
	{
		g_Diagnostics.SetDefaultLocation(GetLocation());
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
	void BinaryExpr::SetExpressionType()
	{
		g_Diagnostics.SetDefaultLocation(GetLocation());
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
	void CastExpr::SetCastType()
	{
		g_Diagnostics.SetDefaultLocation(GetLocation());
		QualifiedType operand_type = ValueTransformation(operand->GetType());
		if (IsVoidType(GetType()))
		{
			// If the target type is void, then expression is evaluated for its
			// side-effects and its returned value is discarded.
		}
		else if (!IsScalarType(GetType()))
		{
			g_Diagnostics.Report(cast_invalid_type);
		}
		else if (!IsScalarType(operand_type))
		{
			g_Diagnostics.Report(cast_invalid_type);
		}
		else if (IsPointerType(GetType()) && IsFloatingType(operand_type))
		{
			g_Diagnostics.Report(cast_invalid_type);
		}
		else if (IsPointerType(operand_type) && IsFloatingType(GetType()))
		{
			g_Diagnostics.Report(cast_invalid_type);
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
}

#include "Backend/ASTCodegen.h"