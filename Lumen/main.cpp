#include "Frontend/SourceBuffer.h"
#include "Frontend/Lexer.h"
#include <iostream>

using namespace lumen;

int main()
{
	SourceBuffer buff("test.txt");
	Lexer lex(buff);

	if (lex.Lex())
	{
		auto const& tokens = lex.GetTokens();
		for (auto&& token : tokens)
		{
			std::cout << "Type: " << GetTokenName(token.GetType()) << "\t";
			if (token.HasData())
			{
				if (token.GetType() != TokenType::number) std::cout << "Value: " << token.GetData<std::string>() << "\t";
				else std::cout << "Value: " << token.GetData<int32>() << "\t";
			}
			auto const& loc = token.GetLocation();
			std::cout << "Location: " << loc.filename << ", line: " << loc.line << ", column: " << loc.column;
			std::cout << "\n";
		}
	}
	else std::cout << "Lexing failed!";
}