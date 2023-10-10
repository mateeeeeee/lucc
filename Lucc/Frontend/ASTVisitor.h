#pragma once
#include "ASTForwardDeclarations.h"

namespace lucc
{
	class ASTVisitor
	{
	public:
		virtual ~ASTVisitor() = default;
		virtual void Visit(NodeAST const& node, uint32 depth) {}
		virtual void Visit(TranslationUnit const& node, uint32 depth) {}

		virtual void Visit(Decl const& node, uint32 depth) {}
		virtual void Visit(VariableDecl const& node, uint32 depth) {}
		virtual void Visit(FunctionDecl const& node, uint32 depth) {}
		virtual void Visit(TypedefDecl const& node, uint32 depth) {}

		virtual void Visit(Expr const& node, uint32 depth) {}
		virtual void Visit(UnaryExpr const& node, uint32 depth) {}
		virtual void Visit(BinaryExpr const& node, uint32 depth) {}
		virtual void Visit(TernaryExpr const& node, uint32 depth) {}
		virtual void Visit(FunctionCallExpr const& node, uint32 depth) {}
		virtual void Visit(CastExpr const& node, uint32 depth) {}
		virtual void Visit(IntLiteral const& node, uint32 depth) {}
		virtual void Visit(StringLiteral const& node, uint32 depth) {}
		virtual void Visit(IdentifierExpr const& node, uint32 depth) {}
		virtual void Visit(DeclRefExpr const& node, uint32 depth) {}
		virtual void Visit(MemberRefExpr const& node, uint32 depth) {}

		virtual void Visit(Stmt const& node, uint32 depth) {}
		virtual void Visit(CompoundStmt const& node, uint32 depth) {}
		virtual void Visit(DeclStmt const& node, uint32 depth) {}
		virtual void Visit(ExprStmt const& node, uint32 depth) {}
		virtual void Visit(NullStmt const& node, uint32 depth) {}
		virtual void Visit(IfStmt const& node, uint32 depth) {}
		virtual void Visit(WhileStmt const& node, uint32 depth) {}
		virtual void Visit(DoWhileStmt const& node, uint32 depth) {}
		virtual void Visit(ForStmt const& node, uint32 depth) {}
		virtual void Visit(SwitchStmt const& node, uint32 depth) {}
		virtual void Visit(CaseStmt const& node, uint32 depth) {}
		virtual void Visit(ReturnStmt const& node, uint32 depth) {}
		virtual void Visit(GotoStmt const& node, uint32 depth) {}
		virtual void Visit(LabelStmt const& node, uint32 depth) {}
		virtual void Visit(BreakStmt const& node, uint32 depth) {}
		virtual void Visit(ContinueStmt const& node, uint32 depth) {}
	};

}