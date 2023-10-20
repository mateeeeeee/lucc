#pragma once
#include "Frontend/Token.h"
#include "Frontend/ASTVisitor.h"


namespace lucc::debug
{
	void PrintTokens(char const* name, std::vector<Token> const& tokens);

	class DebugVisitor : public ASTVisitor
	{
	public:
		explicit DebugVisitor(AST const* ast);

		virtual void Visit(TranslationUnit const&, uint32) override;
		virtual void Visit(NodeAST const&, uint32) override;

		virtual void Visit(Decl const&, uint32) override;
		virtual void Visit(VariableDecl const&, uint32) override;
		virtual void Visit(FunctionDecl const&, uint32) override;
		virtual void Visit(TypedefDecl const&, uint32) override;

		virtual void Visit(Expr const&, uint32);
		virtual void Visit(UnaryExpr const&, uint32) override;
		virtual void Visit(BinaryExpr const&, uint32) override;
		virtual void Visit(TernaryExpr const&, uint32) override;
		virtual void Visit(CastExpr const&, uint32) override;
		virtual void Visit(FunctionCallExpr const&, uint32) override;
		virtual void Visit(IntLiteral const&, uint32) override;
		virtual void Visit(StringLiteral const&, uint32) override;
		virtual void Visit(IdentifierExpr const&, uint32) override;
		virtual void Visit(DeclRefExpr const&, uint32) override;
		virtual void Visit(MemberRefExpr const&, uint32) override;

		virtual void Visit(Stmt const&, uint32) override;
		virtual void Visit(CompoundStmt const&, uint32) override;
		virtual void Visit(DeclStmt const&, uint32) override;
		virtual void Visit(ExprStmt const&, uint32) override;
		virtual void Visit(NullStmt const&, uint32) override;
		virtual void Visit(ReturnStmt const&, uint32) override;
		virtual void Visit(IfStmt const&, uint32) override;
		virtual void Visit(BreakStmt const&, uint32) override;
		virtual void Visit(ContinueStmt const&, uint32) override;
		virtual void Visit(WhileStmt const&, uint32) override;
		virtual void Visit(DoWhileStmt const&, uint32) override;
		virtual void Visit(ForStmt const&, uint32) override;
		virtual void Visit(SwitchStmt const&, uint32) override;
		virtual void Visit(CaseStmt const&, uint32) override;
		virtual void Visit(LabelStmt const&, uint32) override;
		virtual void Visit(GotoStmt const&, uint32) override;
	};

}