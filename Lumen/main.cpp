#include "Frontend/SourceBuffer.h"
#include "Frontend/Lexer.h"
#include "Frontend/Preprocessor.h"
#include "Core/CLIParser.h"
#include <iostream>

using namespace lu;

int main(int argc, char** argv)
{
	//cmd
	CLIParser parser{};
	CLIArg& opt = parser.AddArg(false, "-opt");
	parser.Parse(argc, argv);

	//#todo add diagnostics 

	SourceBuffer buff("test.txt");
	Lexer lex(buff);
	if (lex.Lex())
	{
		std::cout << "Before postprocessor: \n";
		auto const& tokens = lex.GetTokens();
		for (auto&& token : tokens)
		{
			std::cout << "Type: " << GetTokenName(token.GetType()) << "\t";
			std::cout << "Value: " << token.GetIdentifier() << "\t";
			//auto const& loc = token.GetLocation();
			//std::cout << "Location: " << loc.filename << ", line: " << loc.line << ", column: " << loc.column;
			std::cout << "\n";
		}
	}
	else std::cout << "Lexing failed!";

	Preprocessor pp{};
	if (pp.Preprocess(lex))
	{
		std::cout << "\n \nAfter postprocessor: \n";
		auto const& tokens = lex.GetTokens();
		for (auto&& token : tokens)
		{
			std::cout << "Type: " << GetTokenName(token.GetType()) << "\t";
			std::cout << "Value: " << token.GetIdentifier() << "\t";
			//auto const& loc = token.GetLocation();
			//std::cout << "Location: " << loc.filename << ", line: " << loc.line << ", column: " << loc.column;
			std::cout << "\n";
		}
	}
	else std::cout << "Postprocessing failed!";
}