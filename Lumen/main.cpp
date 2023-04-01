#include "Frontend/SourceBuffer.h"
#include "Frontend/Lexer.h"
#include <iostream>

using namespace lu;

int main()
{
	//#todo add cmd line parsing 
	//#todo add diagnostics 

	SourceBuffer buff("test.txt");
	Lexer lex(buff);

	if (lex.Lex())
	{
		auto const& tokens = lex.GetTokens();
		for (auto&& token : tokens)
		{
			std::cout << "Type: " << GetTokenName(token.GetType()) << "\t";
			std::cout << "Value: " << token.GetData() << "\t";
			auto const& loc = token.GetLocation();
			std::cout << "Location: " << loc.filename << ", line: " << loc.line << ", column: " << loc.column;
			std::cout << "\n";
		}
	}
	else std::cout << "Lexing failed!";
}