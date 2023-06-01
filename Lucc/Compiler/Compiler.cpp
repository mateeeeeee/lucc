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
		static constexpr char const* _compiler_path = "Compiler/";
		static constexpr char const* _executables_path = "C:\\Program Files\\Microsoft Visual Studio\\2022\\Preview\\VC\\Tools\\MSVC\\14.34.31823\\bin\\Hostx64\\x64";
		static constexpr char const* _lib_path = "C:\\Program Files\\Microsoft Visual Studio\\2022\\Preview\\VC\\Tools\\MSVC\\14.34.31823\\lib\\x64";

	}

	bool Compile(CompilerInput const& input)
	{
		diag::Initialize();

		fs::path compiler_path = fs::current_path() /= _compiler_path;
		fs::current_path(compiler_path);

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

		std::string masm_cmd = std::format("\"{}/ml64.exe\" /Fo {} /c {}", _executables_path, input.object, input.assembly);
		system(masm_cmd.c_str());
		std::string link_cmd = std::format("\"{}/link.exe\" /out:{} {} /subsystem:console /entry:main", _executables_path, input.executable, input.object);
		system(link_cmd.c_str());
		std::string exe_cmd = std::format("{}", input.executable);
		system(exe_cmd.c_str());

		return true;
	}

}
