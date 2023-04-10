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
				std::string indentation(indent * 3, '-');
				if (!indentation.empty())
				{
					indentation.front() = '`';
					indentation.back() = '>';
				}
				return indentation;
			}
		public:

			DebugNodeVisitorAST(AST* ast)
			{
				std::cout << "AST Traversal:\n";
				ast->tr_unit->Accept(*this, 0);
			}

			virtual void Visit(TranslationUnitDeclAST const& node, size_t indent) override
			{
				std::cout << GetIndentation(indent) << "TranslationUnitDeclAST \n";
			}
			virtual void Visit(TypedefDeclAST const& node, size_t indent) override
			{
				std::cout << GetIndentation(indent) << "TypedefDeclAST \n";
			}
			virtual void Visit(FunctionDeclAST const& node, size_t indent) override
			{
				std::cout << GetIndentation(indent) << "FunctionDeclAST \n";
			}
			virtual void Visit(NodeAST const& node, size_t indent)
			{
				std::cout << GetIndentation(indent) << "NodeType not implemented \n";
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
