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

		virtual void Visit(TranslationUnit const& node, uint32 depth) override;
		virtual void Visit(NodeAST const& node, uint32 depth) override;

		virtual void Visit(Decl const& node, uint32 depth) override;
		virtual void Visit(VariableDecl const& node, uint32 depth) override;
		virtual void Visit(FunctionDecl const& node, uint32 depth) override;
		virtual void Visit(TypedefDecl const& node, uint32 depth) override;

		virtual void Visit(Expr const& node, uint32 depth);
		virtual void Visit(UnaryExpr const& node, uint32 depth) override;
		virtual void Visit(BinaryExpr const& node, uint32 depth) override;
		virtual void Visit(TernaryExpr const& node, uint32 depth) override;
		virtual void Visit(CastExpr const& node, uint32 depth) override;
		virtual void Visit(FunctionCallExpr const& node, uint32 depth) override;
		virtual void Visit(IntLiteral const& node, uint32 depth) override;
		virtual void Visit(StringLiteral const& node, uint32 depth) override;
		virtual void Visit(IdentifierExpr const& node, uint32 depth) override;
		virtual void Visit(DeclRefExpr const& node, uint32 depth) override;
		virtual void Visit(MemberRefExpr const& node, uint32 depth) override;

		virtual void Visit(Stmt const& node, uint32 depth) override;
		virtual void Visit(CompoundStmt const& node, uint32 depth) override;
		virtual void Visit(DeclStmt const& node, uint32 depth) override;
		virtual void Visit(ExprStmt const& node, uint32 depth) override;
		virtual void Visit(NullStmt const& node, uint32 depth) override;
		virtual void Visit(IfStmt const& node, uint32 depth) override;
		virtual void Visit(WhileStmt const& node, uint32 depth) override;
		virtual void Visit(DoWhileStmt const& node, uint32 depth) override;
		virtual void Visit(ForStmt const& node, uint32 depth) override;
		virtual void Visit(SwitchStmt const& node, uint32 depth) override;
		virtual void Visit(CaseStmt const& node, uint32 depth) override;
		virtual void Visit(ReturnStmt const& node, uint32 depth) override;
		virtual void Visit(GotoStmt const& node, uint32 depth) override;
		virtual void Visit(LabelStmt const& node, uint32 depth) override;
		virtual void Visit(BreakStmt const& node, uint32 depth) override;
		virtual void Visit(ContinueStmt const& node, uint32 depth) override;
	};

}