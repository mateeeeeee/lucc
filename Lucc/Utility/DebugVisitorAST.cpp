#include "DebugVisitorAST.h"
#include "Frontend/AST.h"
#include "Core/Logger.h"

namespace lucc::debug
{
	namespace
	{
		constexpr std::string GetIndentation(uint32 indent)
		{
			std::string indentation(indent * 3, ' ');
			if (!indentation.empty())
			{
				indentation[indent * 3 - 3] = '`';
				indentation[indent * 3 - 2] = '-';
				indentation[indent * 3 - 1] = '>';
			}
			return indentation;
		}
		constexpr std::string BinaryExprKindToString(BinaryExprKind kind)
		{
			switch (kind)
			{
			case BinaryExprKind::Add:
				return "+";
			case BinaryExprKind::Subtract:
				return "-";
			case BinaryExprKind::Multiply:
				return "*";
			case BinaryExprKind::Divide:
				return "/";
			case BinaryExprKind::Modulo:
				return "%";
			case BinaryExprKind::ShiftLeft:
				return "<<";
			case BinaryExprKind::ShiftRight:
				return ">>";
			case BinaryExprKind::BitAnd:
				return "&";
			case BinaryExprKind::BitOr:
				return "|";
			case BinaryExprKind::BitXor:
				return "^";
			case BinaryExprKind::Assign:
				return "=";
			case BinaryExprKind::Comma:
				return ",";
			case BinaryExprKind::LogicalAnd:
				return "&&";
			case BinaryExprKind::LogicalOr:
				return "||";
			case BinaryExprKind::Equal:
				return "==";
			case BinaryExprKind::NotEqual:
				return "!=";
			case BinaryExprKind::Less:
				return "<";
			case BinaryExprKind::Greater:
				return ">";
			case BinaryExprKind::LessEqual:
				return "<=";
			case BinaryExprKind::GreaterEqual:
				return ">=";
			case BinaryExprKind::Invalid:
			default:
				return "invalid";
			}
		}
		constexpr std::string UnaryExprKindToString(UnaryExprKind kind)
		{
			switch (kind)
			{
			case UnaryExprKind::PreIncrement:
				return "++";
			case UnaryExprKind::PreDecrement:
				return "--";
			case UnaryExprKind::PostIncrement:
				return "++";
			case UnaryExprKind::PostDecrement:
				return "--";
			case UnaryExprKind::Plus:
				return "+";
			case UnaryExprKind::Minus:
				return "-";
			case UnaryExprKind::BitNot:
				return "~";
			case UnaryExprKind::LogicalNot:
				return "!";
			case UnaryExprKind::Dereference:
				return "*";
			case UnaryExprKind::AddressOf:
				return "&";
			default:
				return "invalid";
			}
		}
	}
	void PrintTokens(char const* name, std::vector<Token> const& tokens)
	{
		LU_DEBUG("{}\n", name);
		for (auto&& token : tokens)
		{
			LU_DEBUG("Type: {}\tValue: {}\n", GetTokenName(token.GetKind()), token.GetIdentifier());
		}
		LU_DEBUG("\n\n");
	}

	DebugVisitorAST::DebugVisitorAST(AST const* ast)
	{
		LU_DEBUG("AST Traversal:\n");
		ast->translation_unit->Accept(*this, 0);
	}

	void DebugVisitorAST::Visit(NodeAST const& node, uint32 depth)
	{
		LU_DEBUG("{}DoWhileStmtAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(TranslationUnitAST const& node, uint32 depth)
	{
		LU_DEBUG("{}TranslationUnitAST \n", GetIndentation(depth));
	}

	void DebugVisitorAST::Visit(DeclAST const& node, uint32 depth)
	{
		LU_DEBUG("{}DeclAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(VariableDeclAST const& node, uint32 depth)
	{
		LU_DEBUG("{}VarDeclAST, name: {}\n", GetIndentation(depth), node.GetName());
	}
	void DebugVisitorAST::Visit(FunctionDeclAST const& node, uint32 depth)
	{
		LU_DEBUG("{}FunctionDeclAST, type: {}\n", GetIndentation(depth), node.GetName());
	}
	void DebugVisitorAST::Visit(TypedefDeclAST const& node, uint32 depth)
	{
		LU_DEBUG("{}TypedefDeclAST, name: {}\n", GetIndentation(depth), node.GetName());
	}

	void DebugVisitorAST::Visit(StmtAST const& node, uint32 depth)
	{
		LU_DEBUG("{}StmtAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(CompoundStmtAST const& node, uint32 depth)
	{
		LU_DEBUG("{}CompoundStmtAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(DeclStmtAST const& node, uint32 depth)
	{
		LU_DEBUG("{}DeclStmtAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(ExprStmtAST const& node, uint32 depth)
	{
		LU_DEBUG("{}ExprStmtAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(NullStmtAST const& node, uint32 depth)
	{
		LU_DEBUG("{}NullStmtAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(IfStmtAST const& node, uint32 depth)
	{
		LU_DEBUG("{}IfStmtAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(WhileStmtAST const& node, uint32 depth)
	{
		LU_DEBUG("{}WhileStmtAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(ForStmtAST const& node, uint32 depth)
	{
		LU_DEBUG("{}ForStmtAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(ReturnStmtAST const& node, uint32 depth)
	{
		LU_DEBUG("{}ReturnStmtAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(GotoStmtAST const& node, uint32 depth)
	{
		LU_DEBUG("{}GotoStmtAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(LabelStmtAST const& node, uint32 depth)
	{
		LU_DEBUG("{}LabelStmtAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(CaseStmtAST const& node, uint32 depth)
	{
		LU_DEBUG("{}CaseStmtAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(SwitchStmtAST const& node, uint32 depth)
	{
		LU_DEBUG("{}SwitchStmtAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(DoWhileStmtAST const& node, uint32 depth)
	{
		LU_DEBUG("{}DoWhileStmtAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(ContinueStmtAST const& node, uint32 depth)
	{
		LU_DEBUG("{}ContinueStmtAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(BreakStmtAST const& node, uint32 depth)
	{
		LU_DEBUG("{}BreakStmtAST \n", GetIndentation(depth));
	}

	void DebugVisitorAST::Visit(ExprAST const& node, uint32 depth)
	{
		LU_DEBUG("{}ExprAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(MemberRefExprAST const& node, uint32 depth)
	{
		LU_DEBUG("{}MemberRefExprAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(DeclRefExprAST const& node, uint32 depth)
	{
		LU_DEBUG("{}DeclRefExprAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(CastExprAST const& node, uint32 depth)
	{
		LU_DEBUG("{}CastExprAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(FunctionCallExprAST const& node, uint32 depth)
	{
		LU_DEBUG("{}FunctionCallAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(UnaryExprAST const& node, uint32 depth)
	{
		LU_DEBUG("{}UnaryExprAST, Op: {}\n", GetIndentation(depth), UnaryExprKindToString(node.GetUnaryKind()));
	}
	void DebugVisitorAST::Visit(BinaryExprAST const& node, uint32 depth)
	{
		LU_DEBUG("{}BinaryExprAST, Op: {}\n", GetIndentation(depth), BinaryExprKindToString(node.GetBinaryKind()));
	}
	void DebugVisitorAST::Visit(TernaryExprAST const& node, uint32 depth)
	{
		LU_DEBUG("{}TernaryExprAST \n", GetIndentation(depth));
	}
	void DebugVisitorAST::Visit(IntLiteralAST const& node, uint32 depth)
	{
		LU_DEBUG("{}IntegerLiteralAST, Value: {}\n", GetIndentation(depth), std::to_string(node.GetValue()));
	}
	void DebugVisitorAST::Visit(StringLiteralAST const& node, uint32 depth)
	{
		LU_DEBUG("{}StringLiteralAST, Value: {}\n", GetIndentation(depth), node.GetString());
	}
	void DebugVisitorAST::Visit(IdentifierExprAST const& node, uint32 depth)
	{
		LU_DEBUG("{}IdentifierAST, Name: {}\n", GetIndentation(depth), node.GetName());
	}

}