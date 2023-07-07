#pragma once
#include "Frontend/Token.h"
#include "Frontend/AST.h"


namespace lucc
{
	namespace debug
	{
		void PrintOutput(char const* str);
		void PrintTokens(char const* name, std::vector<Token> const& tokens);
		class DebugNodeVisitorAST : public INodeVisitorAST
		{
			static constexpr std::string GetIndentation(size_t indent)
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
			static constexpr std::string BinaryExprKindToString(BinaryExprKind kind)
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
			static constexpr std::string UnaryExprKindToString(UnaryExprKind kind)
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
				case UnaryExprKind::Cast:
					return "cast ()";
				default:
					return "invalid";
				}
			}
		public:

			explicit DebugNodeVisitorAST(AST* ast);

			virtual void Visit(TranslationUnitAST const& node, size_t depth) override;
			virtual void Visit(NodeAST const& node, size_t depth) override;
			virtual void Visit(ExprAST const& node, size_t depth);
			virtual void Visit(UnaryExprAST const& node, size_t depth) override;
			virtual void Visit(BinaryExprAST const& node, size_t depth) override;
			virtual void Visit(TernaryExprAST const& node, size_t depth) override;
			virtual void Visit(FunctionCallAST const& node, size_t depth) override;
			virtual void Visit(IntLiteralAST const& node, size_t depth) override;
			virtual void Visit(StringLiteralAST const& node, size_t depth) override;
			virtual void Visit(IdentifierAST const& node, size_t depth) override;
			virtual void Visit(StmtAST const& node, size_t depth) override;
			virtual void Visit(CompoundStmtAST const& node, size_t depth) override;
			virtual void Visit(DeclStmtAST const& node, size_t depth) override;
			virtual void Visit(ExprStmtAST const& node, size_t depth) override;
			virtual void Visit(NullStmtAST const& node, size_t depth) override;
			virtual void Visit(DeclAST const& node, size_t depth) override;
			virtual void Visit(VarDeclAST const& node, size_t depth) override;
			virtual void Visit(FunctionDeclAST const& node, size_t depth) override;
			virtual void Visit(TypedefDeclAST const& node, size_t depth) override;
			virtual void Visit(IfStmtAST const& node, size_t depth) override;
			virtual void Visit(WhileStmtAST const& node, size_t depth) override;
			virtual void Visit(ForStmtAST const& node, size_t depth) override;
			virtual void Visit(ReturnStmtAST const& node, size_t depth) override;
			virtual void Visit(GotoStmtAST const& node, size_t depth) override;
			virtual void Visit(LabelStmtAST const& node, size_t depth) override;
			virtual void Visit(BreakStmtAST const& node, size_t depth) override;
			virtual void Visit(ContinueStmtAST const& node, size_t depth) override;
		};
	}
}