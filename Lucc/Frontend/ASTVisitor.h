#pragma once
#include "ASTForwardDeclarations.h"

namespace lucc
{
	class ASTVisitor
	{
	public:
		virtual ~ASTVisitor() = default;
		virtual void Visit(NodeAST const&, uint32) {}
		virtual void Visit(TranslationUnit const&, uint32) {}

		virtual void Visit(Decl const&, uint32) {}
		virtual void Visit(VariableDecl const&, uint32) {}
		virtual void Visit(FunctionDecl const&, uint32) {}
		virtual void Visit(TypedefDecl const&, uint32) {}

		virtual void Visit(Expr const&, uint32) {}
		virtual void Visit(UnaryExpr const&, uint32) {}
		virtual void Visit(BinaryExpr const&, uint32) {}
		virtual void Visit(TernaryExpr const&, uint32) {}
		virtual void Visit(FunctionCallExpr const&, uint32) {}
		virtual void Visit(CastExpr const&, uint32) {}
		virtual void Visit(IntLiteral const&, uint32) {}
		virtual void Visit(StringLiteral const&, uint32) {}
		virtual void Visit(IdentifierExpr const&, uint32) {}
		virtual void Visit(DeclRefExpr const&, uint32) {}
		virtual void Visit(MemberRefExpr const&, uint32) {}

		virtual void Visit(Stmt const&, uint32) {}
		virtual void Visit(CompoundStmt const&, uint32) {}
		virtual void Visit(DeclStmt const&, uint32) {}
		virtual void Visit(ExprStmt const&, uint32) {}
		virtual void Visit(NullStmt const&, uint32) {}
		virtual void Visit(ReturnStmt const&, uint32) {}
		virtual void Visit(IfStmt const&, uint32) {}
		virtual void Visit(BreakStmt const&, uint32) {}
		virtual void Visit(ContinueStmt const&, uint32) {}
		virtual void Visit(WhileStmt const&, uint32) {}
		virtual void Visit(DoWhileStmt const&, uint32) {}
		virtual void Visit(ForStmt const&, uint32) {}
		virtual void Visit(SwitchStmt const&, uint32) {}
		virtual void Visit(CaseStmt const&, uint32) {}
		virtual void Visit(GotoStmt const&, uint32) {}
		virtual void Visit(LabelStmt const&, uint32) {}
	};

}