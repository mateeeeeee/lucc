#pragma once
#include <memory>
#include <vector>
#include "ASTForwardDeclarations.h"

namespace lucc
{
	template<typename T>
	using UniquePtr = std::unique_ptr<T>;

	template<typename Type, typename... Args>
	inline UniquePtr<Type> MakeUnique(Args&&... args)
	{
		return std::make_unique<Type>(std::forward<Args>(args)...);
	}

	using UniqueTranslationUnitPtr	= UniquePtr<TranslationUnit>;

	using UniqueExprPtr				= UniquePtr<Expr>;
	using UniqueUnaryExprPtr		= UniquePtr<UnaryExpr>;
	using UniqueBinaryExprPtr		= UniquePtr<BinaryExpr>;
	using UniqueTernaryExprPtr		= UniquePtr<TernaryExpr>;
	using UniqueCastExprPtr			= UniquePtr<CastExpr>;
	using UniqueFunctionCallExprPtr = UniquePtr<FunctionCallExpr>;
	using UniqueIntLiteralPtr		= UniquePtr<IntLiteral>;
	using UniqueStringLiteralPtr	= UniquePtr<StringLiteral>;
	using UniqueIdentifierExprPtr	= UniquePtr<IdentifierExpr>;
	using UniqueDeclRefExprPtr		= UniquePtr<DeclRefExpr>;
	using UniqueMemberRefExprPtr	= UniquePtr<MemberRefExpr>;

	using UniqueStmtPtr				= UniquePtr<Stmt>;
	using UniqueCompoundStmtPtr		= UniquePtr<CompoundStmt>;
	using UniqueDeclStmtPtr			= UniquePtr<DeclStmt>;
	using UniqueExprStmtPtr			= UniquePtr<ExprStmt>;
	using UniqueNullStmtPtr			= UniquePtr<NullStmt>;
	using UniqueIfStmtPtr			= UniquePtr<IfStmt>;
	using UniqueWhileStmtPtr		= UniquePtr<WhileStmt>;
	using UniqueDoWhileStmtPtr		= UniquePtr<DoWhileStmt>;
	using UniqueForStmtPtr			= UniquePtr<ForStmt>;
	using UniqueSwitchStmtPtr		= UniquePtr<SwitchStmt>;
	using UniqueCaseStmtPtr			= UniquePtr<CaseStmt>;
	using UniqueReturnStmtPtr		= UniquePtr<ReturnStmt>;
	using UniqueGotoStmtPtr			= UniquePtr<GotoStmt>;
	using UniqueLabelStmtPtr		= UniquePtr<LabelStmt>;
	using UniqueBreakStmtPtr		= UniquePtr<BreakStmt>;
	using UniqueContinueStmtPtr		= UniquePtr<ContinueStmt>;

	using UniqueDeclPtr				= UniquePtr<Decl>;
	using UniqueVariableDeclPtr		= UniquePtr<VariableDecl>;
	using UniqueFunctionDeclPtr		= UniquePtr<FunctionDecl>;
	using UniqueTypedefDeclPtr		= UniquePtr<TypedefDecl>;

	using UniqueDeclPtrList			= std::vector<UniqueDeclPtr>;
	using UniqueStmtPtrList			= std::vector<UniqueStmtPtr>;
	using UniqueExprPtrList			= std::vector<UniqueExprPtr>;
	using UniqueVariableDeclPtrList = std::vector<UniqueVariableDeclPtr>;
	using UniqueTypedefDeclPtrList  = std::vector<UniqueTypedefDeclPtr>;

	using DeclPtrList				= std::vector<Decl*>;
	using StmtPtrList				= std::vector<Stmt*>;
	using ExprPtrList				= std::vector<Expr*>;
	using VariableDeclPtrList		= std::vector<VariableDecl*>;
	using FunctionCallExprPtrList	= std::vector<FunctionCallExpr*>;
	using BreakStmtPtrList			= std::vector<BreakStmt*>;
	using CaseStmtPtrList			= std::vector<CaseStmt*>;
	using ContinueStmtPtrList		= std::vector<ContinueStmt*>;
	using SwitchStmtPtrList			= std::vector<SwitchStmt*>;

	using ConstVariableDeclPtrList		= std::vector<VariableDecl const*>;
	using ConstFunctionCallExprPtrList	= std::vector<FunctionCallExpr const*>;
}