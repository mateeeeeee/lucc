#pragma once
#include "ForwardAST.h"

namespace lucc
{
	class IVisitorAST
	{
	public:
		virtual ~IVisitorAST() = default;
		virtual void Visit(NodeAST const& node, uint32 depth) {}
		virtual void Visit(TranslationUnitAST const& node, uint32 depth) {}

		virtual void Visit(DeclAST const& node, uint32 depth) {}
		virtual void Visit(VariableDeclAST const& node, uint32 depth) {}
		virtual void Visit(FunctionDeclAST const& node, uint32 depth) {}
		virtual void Visit(TypedefDeclAST const& node, uint32 depth) {}

		virtual void Visit(ExprAST const& node, uint32 depth) {}
		virtual void Visit(UnaryExprAST const& node, uint32 depth) {}
		virtual void Visit(BinaryExprAST const& node, uint32 depth) {}
		virtual void Visit(TernaryExprAST const& node, uint32 depth) {}
		virtual void Visit(FunctionCallExprAST const& node, uint32 depth) {}
		virtual void Visit(CastExprAST const& node, uint32 depth) {}
		virtual void Visit(IntLiteralAST const& node, uint32 depth) {}
		virtual void Visit(StringLiteralAST const& node, uint32 depth) {}
		virtual void Visit(IdentifierExprAST const& node, uint32 depth) {}
		virtual void Visit(DeclRefExprAST const& node, uint32 depth) {}
		virtual void Visit(MemberRefExprAST const& node, uint32 depth) {}

		virtual void Visit(StmtAST const& node, uint32 depth) {}
		virtual void Visit(CompoundStmtAST const& node, uint32 depth) {}
		virtual void Visit(DeclStmtAST const& node, uint32 depth) {}
		virtual void Visit(ExprStmtAST const& node, uint32 depth) {}
		virtual void Visit(NullStmtAST const& node, uint32 depth) {}
		virtual void Visit(IfStmtAST const& node, uint32 depth) {}
		virtual void Visit(WhileStmtAST const& node, uint32 depth) {}
		virtual void Visit(DoWhileStmtAST const& node, uint32 depth) {}
		virtual void Visit(ForStmtAST const& node, uint32 depth) {}
		virtual void Visit(SwitchStmtAST const& node, uint32 depth) {}
		virtual void Visit(CaseStmtAST const& node, uint32 depth) {}
		virtual void Visit(ReturnStmtAST const& node, uint32 depth) {}
		virtual void Visit(GotoStmtAST const& node, uint32 depth) {}
		virtual void Visit(LabelStmtAST const& node, uint32 depth) {}
		virtual void Visit(BreakStmtAST const& node, uint32 depth) {}
		virtual void Visit(ContinueStmtAST const& node, uint32 depth) {}
	};

}