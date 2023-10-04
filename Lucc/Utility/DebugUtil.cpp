#include <iostream>
#include "DebugUtil.h"
#include "Core/Logger.h"

void lucc::debug::PrintOutput(char const* str)
{
	LU_DEBUG("{}\n", str);
}

void lucc::debug::PrintTokens(char const* name, std::vector<Token> const& tokens)
{
	LU_DEBUG("{}\n", name);
	for (auto&& token : tokens)
	{
		LU_DEBUG("Type: {}\tValue: {}\n", GetTokenName(token.GetKind()), token.GetIdentifier());
	}
	LU_DEBUG("\n\n");
}

lucc::debug::DebugNodeVisitorAST::DebugNodeVisitorAST(AST* ast)
{
	LU_DEBUG("AST Traversal:\n");
	ast->translation_unit->Accept(*this, 0);
}

void lucc::debug::DebugNodeVisitorAST::Visit(LabelStmtAST const& node, size_t depth)
{
	LU_DEBUG("{}LabelStmtAST \n", GetIndentation(depth));
}


void lucc::debug::DebugNodeVisitorAST::Visit(MemberRefExprAST const& node, size_t depth)
{
	LU_DEBUG("{}MemberRefExprAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(DeclRefExprAST const& node, size_t depth)
{
	LU_DEBUG("{}DeclRefExprAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(CastExprAST const& node, size_t depth)
{
	LU_DEBUG("{}CastExprAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(CaseStmtAST const& node, size_t depth)
{
	LU_DEBUG("{}CaseStmtAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(SwitchStmtAST const& node, size_t depth)
{
	LU_DEBUG("{}SwitchStmtAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(DoWhileStmtAST const& node, size_t depth)
{
	LU_DEBUG("{}DoWhileStmtAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(ContinueStmtAST const& node, size_t depth)
{
	LU_DEBUG("{}ContinueStmtAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(BreakStmtAST const& node, size_t depth)
{
	LU_DEBUG("{}BreakStmtAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(FunctionCallExprAST const& node, size_t depth)
{
	LU_DEBUG("{}FunctionCallAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(TranslationUnitAST const& node, size_t depth)
{
	LU_DEBUG("{}TranslationUnitAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(NodeAST const& node, size_t depth)
{
	LU_DEBUG("{}DoWhileStmtAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(ExprAST const& node, size_t depth)
{
	LU_DEBUG("{}ExprAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(UnaryExprAST const& node, size_t depth)
{
	LU_DEBUG("{}UnaryExprAST, Op: {}\n", GetIndentation(depth), UnaryExprKindToString(node.GetUnaryKind()));
}

void lucc::debug::DebugNodeVisitorAST::Visit(BinaryExprAST const& node, size_t depth)
{
	LU_DEBUG("{}BinaryExprAST, Op: {}\n", GetIndentation(depth), BinaryExprKindToString(node.GetBinaryKind()));
}

void lucc::debug::DebugNodeVisitorAST::Visit(TernaryExprAST const& node, size_t depth)
{
	LU_DEBUG("{}TernaryExprAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(IntLiteralAST const& node, size_t depth)
{
	LU_DEBUG("{}IntegerLiteralAST, Value: {}\n", GetIndentation(depth), std::to_string(node.GetValue()));
}

void lucc::debug::DebugNodeVisitorAST::Visit(StringLiteralAST const& node, size_t depth)
{
	LU_DEBUG("{}StringLiteralAST, Value: {}\n", GetIndentation(depth), node.GetString());
}

void lucc::debug::DebugNodeVisitorAST::Visit(IdentifierExprAST const& node, size_t depth)
{
	LU_DEBUG("{}IdentifierAST, Name: {}\n", GetIndentation(depth), node.GetName());
}

void lucc::debug::DebugNodeVisitorAST::Visit(StmtAST const& node, size_t depth)
{
	LU_DEBUG("{}StmtAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(CompoundStmtAST const& node, size_t depth)
{
	LU_DEBUG("{}CompoundStmtAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(DeclStmtAST const& node, size_t depth)
{
	LU_DEBUG("{}DeclStmtAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(ExprStmtAST const& node, size_t depth)
{
	LU_DEBUG("{}ExprStmtAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(NullStmtAST const& node, size_t depth)
{
	LU_DEBUG("{}NullStmtAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(DeclAST const& node, size_t depth)
{
	LU_DEBUG("{}DeclAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(VarDeclAST const& node, size_t depth)
{
	LU_DEBUG("{}VarDeclAST, name: {}\n", GetIndentation(depth), node.GetName());
}

void lucc::debug::DebugNodeVisitorAST::Visit(FunctionDeclAST const& node, size_t depth)
{
	LU_DEBUG("{}FunctionDeclAST, type: {}\n", GetIndentation(depth), node.GetName());
}

void lucc::debug::DebugNodeVisitorAST::Visit(TypedefDeclAST const& node, size_t depth)
{
	LU_DEBUG("{}TypedefDeclAST, name: {}\n", GetIndentation(depth), node.GetName());
}

void lucc::debug::DebugNodeVisitorAST::Visit(IfStmtAST const& node, size_t depth)
{
	LU_DEBUG("{}IfStmtAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(WhileStmtAST const& node, size_t depth)
{
	LU_DEBUG("{}WhileStmtAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(ForStmtAST const& node, size_t depth)
{
	LU_DEBUG("{}ForStmtAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(ReturnStmtAST const& node, size_t depth)
{
	LU_DEBUG("{}ReturnStmtAST \n", GetIndentation(depth));
}

void lucc::debug::DebugNodeVisitorAST::Visit(GotoStmtAST const& node, size_t depth)
{
	LU_DEBUG("{}GotoStmtAST \n", GetIndentation(depth));
}
