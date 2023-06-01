#include <filesystem>
#include <format>
#include "Compiler.h"
#include "DebugUtil.h"
#include "Core/Enums.h"
#include "Frontend/SourceBuffer.h"
#include "Frontend/Diagnostics.h"
#include "Frontend/Preprocessor.h"
#include "Frontend/Lexer.h"
#include "Frontend/Parser.h"
#include "Backend/x86_64CodeGenerator.h"

namespace fs = std::filesystem;

namespace lucc
{
	namespace
	{
		static constexpr char const* compiler_path = "Compiler/";

		std::string ExecuteSystemCmd(std::string const& _cmd)
		{
			char tmpname[L_tmpnam];
			tmpnam_s(tmpname);
			std::string cmd = _cmd + " >> " + tmpname;
			std::system(cmd.c_str());
			std::ifstream file(tmpname, std::ios::in | std::ios::binary);
			std::string result;
			if (file) 
			{
				while (!file.eof()) result.push_back(file.get());
				file.close();
			}
			remove(tmpname);
			return result;
		}
	}

	bool Compile(CompilerInput const& input)
	{
		diag::Initialize();
		fs::current_path(fs::current_path() /= compiler_path);

		SourceBuffer src(input.source);
		Lexer lex(src);
		if (!lex.Lex())
		{
			Report(diag::lexing_failed);
			return false;
		}
		if(input.flags & CompilerFlag_OutputDebugInfo) debug::PrintTokens("After lexer:", lex.GetTokens());

		Preprocessor pp(lex);
		if (!pp.Preprocess())
		{
			Report(diag::preprocessing_failed);
			return false;
		}
		if (input.flags & CompilerFlag_OutputDebugInfo) debug::PrintTokens("\n\nAfter preprocessor:", lex.GetTokens());
		
		Parser parser(lex.GetTokens());
		if (!parser.Parse())
		{
			Report(diag::parsing_failed);
			return false;
		}
		AST* ast = parser.GetAST();
		if (input.flags & CompilerFlag_OutputDebugInfo) debug::DebugNodeVisitorAST visitor(ast);

		
		x86_64CodeGenerator x86_64(input.assembly, ast);
		x86_64.Generate();

		std::string masm_cmd = std::format("ml64.exe /Fo {} /c {}", input.object, input.assembly);
		std::string link_cmd = std::format("link.exe /out:{} {} /subsystem:console /entry:main", input.executable, input.object);
		std::string exe_cmd  = std::format("{}", input.executable);

		system(masm_cmd.c_str());
		system(link_cmd.c_str());
		system(exe_cmd.c_str());
		return true;
	}

}
