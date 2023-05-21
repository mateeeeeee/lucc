#include "Compiler.h"
#include "DebugUtil.h"
#include "Frontend/SourceBuffer.h"
#include "Frontend/Diagnostics.h"
#include "Frontend/Preprocessor.h"
#include "Frontend/Lexer.h"
#include "Frontend/Parser.h"
#include "Backend/x86_64CodeGenerator.h"


namespace lucc
{
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

		//do codegen
		x86_64CodeGenerator x86(input.output, ast);
		x86.Generate();

		return true;
	}

}
