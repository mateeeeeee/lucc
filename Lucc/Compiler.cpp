#include "Compiler.h"
#include "Frontend/SourceBuffer.h"
#include "Frontend/Diagnostics.h"
#include "Frontend/Preprocessor.h"
#include "Frontend/Lexer.h"
#include "Frontend/Parser.h"
#include "Frontend/AST.h"
#include <iostream>

namespace lucc
{
	namespace debug
	{
		void PrintTokens(char const* name, std::vector<Token> const& tokens)
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

		class DebugNodeVisitorAST : public NodeVisitorAST
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

			DebugNodeVisitorAST(AST* ast)
			{
				std::cout << "AST Traversal:\n";
				ast->translation_unit->Accept(*this, 0);
			}

			virtual void Visit(TranslationUnitAST const& node, size_t depth) override
			{
				std::cout << GetIndentation(depth) << "TranslationUnitAST \n";
			}
			virtual void Visit(NodeAST const& node, size_t depth) override {}
			virtual void Visit(ExprAST const& node, size_t depth)
			{
				std::cout << GetIndentation(depth) << "ExprAST \n";
			}
			virtual void Visit(UnaryExprAST const& node, size_t depth) override
			{
				std::cout << GetIndentation(depth) << "UnaryExprAST, Op:" << UnaryExprKindToString(node.GetOp()) << "\n";
			}
			virtual void Visit(BinaryExprAST const& node, size_t depth) override
			{
				std::cout << GetIndentation(depth) << "BinaryExprAST, Op:" << BinaryExprKindToString(node.GetOp()) << "\n";
			}
			virtual void Visit(TernaryExprAST const& node, size_t depth) override
			{
				std::cout << GetIndentation(depth) << "TernaryExprAST \n";
			}
			virtual void Visit(IntegerLiteralAST const& node, size_t depth) override
			{
				std::cout << GetIndentation(depth) << "IntegerLiteralAST, Value:" << std::to_string(node.GetValue()) << "\n";
			}
			virtual void Visit(StringLiteralAST const& node, size_t depth) override
			{
				std::cout << GetIndentation(depth) << "StringLiteralAST, Value:" << node.GetString() << "\n";
			}
			virtual void Visit(IdentifierAST const& node, size_t depth) override
			{
				std::cout << GetIndentation(depth) << "IdentifierAST, Name:" << node.GetName() << "\n";
			}
			virtual void Visit(StmtAST const& node, size_t depth) override
			{
				std::cout << GetIndentation(depth) << "StmtAST \n";
			}
			virtual void Visit(CompoundStmtAST const& node, size_t depth) override
			{
				std::cout << GetIndentation(depth) << "CompoundStmtAST \n";
			}
			virtual void Visit(DeclStmtAST const& node, size_t depth) override
			{
				std::cout << GetIndentation(depth) << "DeclStmtAST \n";
			}
			virtual void Visit(ExprStmtAST const& node, size_t depth) override
			{
				std::cout << GetIndentation(depth) << "ExprStmtAST \n";
			}
			virtual void Visit(NullStmtAST const& node, size_t depth) override
			{
				std::cout << GetIndentation(depth) << "NullStmtAST \n";
			}
			virtual void Visit(DeclAST const& node, size_t depth) override
			{
				std::cout << GetIndentation(depth) << "DeclAST \n";
			}
			virtual void Visit(VarDeclAST const& node, size_t depth) override
			{
				std::cout << GetIndentation(depth) << "VarDeclAST, name: " << node.GetName() << "\n";
			}
			virtual void Visit(FunctionDeclAST const& node, size_t depth) override
			{
				std::cout << GetIndentation(depth) << "FunctionDeclAST, name: " << node.GetName() << "\n";
			}
			virtual void Visit(IfStmtAST const& node, size_t depth) override
			{
				std::cout << GetIndentation(depth) << "IfStmtAST \n";
			}
			virtual void Visit(WhileStmtAST const& node, size_t depth) override
			{
				std::cout << GetIndentation(depth) << "WhileStmtAST \n";
			}
			virtual void Visit(ForStmtAST const& node, size_t depth) override
			{
				std::cout << GetIndentation(depth) << "ForStmtAST \n";
			}
		};
	}

	bool Compile(CompilerInput& input)
	{
		diag::Initialize();

		SourceBuffer src(input.input);
		Lexer lex(src);
		if (!lex.Lex())
		{
			Report(diag::lexing_failed);
			return false;
		}

		debug::PrintTokens("After lexer:", lex.GetTokens());

		Preprocessor pp(lex);
		if (!pp.Preprocess())
		{
			Report(diag::preprocessing_failed);
			return false;
		}
		debug::PrintTokens("\n\nAfter preprocessor:", lex.GetTokens());
		Parser parser(lex.GetTokens());
		
		if (!parser.Parse())
		{
			Report(diag::parsing_failed);
			return false;
		}
		AST* ast = parser.GetAST();
		debug::DebugNodeVisitorAST visitor(ast);

		//do optimizations
		//do codegen

		return true;
	}

}
