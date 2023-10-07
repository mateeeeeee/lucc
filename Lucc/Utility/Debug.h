#pragma once
#include "Frontend/Token.h"
#include "Frontend/AST.h"


namespace lucc::debug
{
	void PrintTokens(char const* name, std::vector<Token> const& tokens);

	class DebugNodeVisitorAST : public INodeVisitorAST
	{
	public:

		explicit DebugNodeVisitorAST(AST* ast);

		virtual void Visit(TranslationUnitAST const& node, uint32 depth) override;
		virtual void Visit(NodeAST const& node, uint32 depth) override;
		virtual void Visit(ExprAST const& node, uint32 depth);
		virtual void Visit(UnaryExprAST const& node, uint32 depth) override;
		virtual void Visit(BinaryExprAST const& node, uint32 depth) override;
		virtual void Visit(TernaryExprAST const& node, uint32 depth) override;
		virtual void Visit(CastExprAST const& node, uint32 depth) override;
		virtual void Visit(FunctionCallExprAST const& node, uint32 depth) override;
		virtual void Visit(IntLiteralAST const& node, uint32 depth) override;
		virtual void Visit(StringLiteralAST const& node, uint32 depth) override;
		virtual void Visit(IdentifierExprAST const& node, uint32 depth) override;
		virtual void Visit(StmtAST const& node, uint32 depth) override;
		virtual void Visit(CompoundStmtAST const& node, uint32 depth) override;
		virtual void Visit(DeclStmtAST const& node, uint32 depth) override;
		virtual void Visit(ExprStmtAST const& node, uint32 depth) override;
		virtual void Visit(NullStmtAST const& node, uint32 depth) override;
		virtual void Visit(DeclAST const& node, uint32 depth) override;
		virtual void Visit(VarDeclAST const& node, uint32 depth) override;
		virtual void Visit(FunctionDeclAST const& node, uint32 depth) override;
		virtual void Visit(TypedefDeclAST const& node, uint32 depth) override;
		virtual void Visit(IfStmtAST const& node, uint32 depth) override;
		virtual void Visit(WhileStmtAST const& node, uint32 depth) override;
		virtual void Visit(DoWhileStmtAST const& node, uint32 depth) override;
		virtual void Visit(ForStmtAST const& node, uint32 depth) override;
		virtual void Visit(SwitchStmtAST const& node, uint32 depth) override;
		virtual void Visit(CaseStmtAST const& node, uint32 depth) override;
		virtual void Visit(ReturnStmtAST const& node, uint32 depth) override;
		virtual void Visit(GotoStmtAST const& node, uint32 depth) override;
		virtual void Visit(LabelStmtAST const& node, uint32 depth) override;
		virtual void Visit(BreakStmtAST const& node, uint32 depth) override;
		virtual void Visit(ContinueStmtAST const& node, uint32 depth) override;
		virtual void Visit(DeclRefExprAST const& node, uint32 depth) override;
		virtual void Visit(MemberRefExprAST const& node, uint32 depth) override;
	};

}