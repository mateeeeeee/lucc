#include "AST.h"

namespace lucc
{

	void TranslationUnitAST::Accept(NodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		for (auto&& decl : declarations) decl->Accept(visitor, depth + 1);
	}

	void StmtAST::Accept(NodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void ExprAST::Accept(NodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void BinaryExprAST::Accept(NodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		lhs->Accept(visitor, depth + 1);
		rhs->Accept(visitor, depth + 1);
	}

	void IntegerLiteralAST::Accept(NodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void NullStmtAST::Accept(NodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void ExprStmtAST::Accept(NodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		if (expr) expr->Accept(visitor, depth + 1);
	}

	void DeclAST::Accept(NodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
	}

	void VarDeclAST::Accept(NodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		if (init_expr) init_expr->Accept(visitor, depth + 1);
	}

	void CompoundStmtAST::AddStatement(std::unique_ptr<StmtAST>&& stmt)
	{
		statements.push_back(std::move(stmt));
	}

	void CompoundStmtAST::Accept(NodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		for (auto&& stmt : statements) stmt->Accept(visitor, depth + 1);
	}

	void FunctionDeclAST::Accept(NodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		if (body) body->Accept(visitor, depth + 1);
	}

	void DeclStmtAST::Accept(NodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		if (decl) decl->Accept(visitor, depth + 1);
	}

	void IfStmtAST::Accept(NodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		if (condition) condition->Accept(visitor, depth + 1);
		if (then_stmt) then_stmt->Accept(visitor, depth + 1);
		if (else_stmt) else_stmt->Accept(visitor, depth + 1);
	}

	void TernaryExprAST::Accept(NodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		if (cond_expr) cond_expr->Accept(visitor, depth + 1);
		if (true_expr) true_expr->Accept(visitor, depth + 1);
		if (false_expr) false_expr->Accept(visitor, depth + 1);
	}

	void UnaryExprAST::Accept(NodeVisitorAST& visitor, size_t depth) const
	{
		visitor.Visit(*this, depth);
		operand->Accept(visitor, depth + 1);
	}

}
