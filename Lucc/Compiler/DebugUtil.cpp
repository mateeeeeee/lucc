#include <iostream>
#include "DebugUtil.h"

void lucc::debug::PrintOutput(char const* str)
{
	std::cout << str << "\n";
}

void lucc::debug::PrintTokens(char const* name, std::vector<Token> const& tokens)
{
	std::cout << name << "\n";
	for (auto&& token : tokens)
	{
		std::cout << "Type: " << GetTokenName(token.GetKind()) << "\t";
		std::cout << "Value: " << token.GetIdentifier() << "\t";
		//auto const& loc = token.GetLocation();
		//std::cout << "Location: " << loc.filename << ", line: " << loc.line << ", column: " << loc.column;
		std::cout << "\n";
	}
	std::cout << "\n\n";
}

lucc::debug::DebugNodeVisitorAST::DebugNodeVisitorAST(AST* ast)
{
	std::cout << "AST Traversal:\n";
	ast->translation_unit->Accept(*this, 0);
}

void lucc::debug::DebugNodeVisitorAST::Visit(LabelStmtAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "LabelStmtAST \n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(TranslationUnitAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "TranslationUnitAST \n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(NodeAST const& node, size_t depth)
{

}

void lucc::debug::DebugNodeVisitorAST::Visit(ExprAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "ExprAST \n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(UnaryExprAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "UnaryExprAST, Op:" << UnaryExprKindToString(node.GetOp()) << "\n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(BinaryExprAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "BinaryExprAST, Op:" << BinaryExprKindToString(node.GetOp()) << "\n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(TernaryExprAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "TernaryExprAST \n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(ImplicitCastExprAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "ImplicitCastExprAST: " << ImplicitCastKindToString(node.GetKind()) << "\n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(Int64LiteralAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "IntegerLiteralAST, Value:" << std::to_string(node.GetValue()) << "\n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(StringLiteralAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "StringLiteralAST, Value:" << node.GetString() << "\n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(IdentifierAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "IdentifierAST, Name:" << node.GetName() << "\n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(StmtAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "StmtAST \n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(CompoundStmtAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "CompoundStmtAST \n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(DeclStmtAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "DeclStmtAST \n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(ExprStmtAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "ExprStmtAST \n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(NullStmtAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "NullStmtAST \n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(DeclAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "DeclAST \n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(VarDeclAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "VarDeclAST, name: " << node.GetName() << "\n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(FunctionDeclAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "FunctionDeclAST, type: " << node.GetName() << "\n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(TypedefDeclAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "TypedefDeclAST, name: " << node.GetName() << "\n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(IfStmtAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "IfStmtAST \n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(WhileStmtAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "WhileStmtAST \n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(ForStmtAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "ForStmtAST \n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(ReturnStmtAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "ReturnStmtAST \n";
}

void lucc::debug::DebugNodeVisitorAST::Visit(GotoStmtAST const& node, size_t depth)
{
	std::cout << GetIndentation(depth) << "GotoStmtAST \n";
}
