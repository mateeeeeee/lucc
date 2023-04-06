#include "Compiler.h"
#include "Frontend/SourceBuffer.h"
#include "Frontend/Preprocessor.h"
#include "Frontend/Lexer.h"
#include "Frontend/Parser.h"
#include "Frontend/AST.h"


#include <iostream>
namespace lu
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
		public:

			DebugNodeVisitorAST(AST* ast)
			{
				Visit(*ast->tr_unit);
			}

			virtual void Visit(TranslationUnitDeclAST const& node) override
			{

			}
			virtual void Visit(TypedefDeclAST const& node) override
			{

			}
			virtual void Visit(NodeAST const& node)
			{
				//
			}
		};
	}

	bool Compile(CompilerInput& input)
	{
		SourceBuffer src(input.input);
		Lexer lex(src);
		if (!lex.Lex())
		{
			//diag
			return false;
		}

		debug::PrintTokens("After lexer", lex.GetTokens());

		Preprocessor pp(lex);
		if (!pp.Preprocess())
		{
			//diag
			return false;
		}

		debug::PrintTokens("\n\nAfter preprocessor", lex.GetTokens());
		//do parsing

		Parser parser(lex.GetTokens());

		if (!parser.Parse())
		{
			//diag
			return false;
		}
		AST* ast = parser.GetAST();

		debug::DebugNodeVisitorAST visitor(ast);
		//do optimizations

		//do codegen

		return true;
	}

}
