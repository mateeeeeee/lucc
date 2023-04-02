#include "Compiler.h"
#include "Frontend/Preprocessor.h"
#include "Frontend/Lexer.h"
#include "Frontend/SourceBuffer.h"

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
				std::cout << "Type: " << GetTokenName(token.GetType()) << "\t";
				std::cout << "Value: " << token.GetIdentifier() << "\t";
				//auto const& loc = token.GetLocation();
				//std::cout << "Location: " << loc.filename << ", line: " << loc.line << ", column: " << loc.column;
				std::cout << "\n";
			}
			std::cout << "\n\n";
		}
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

		//do optimizations

		//do codegen

		return true;
	}

}
