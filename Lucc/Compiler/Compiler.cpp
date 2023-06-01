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
		static constexpr char const* _assembler_path = "Compiler/Assembler/";
		static constexpr char const* _linker_path = "Compiler/Linker/";

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

		fs::path compiler_path = fs::current_path() /= _compiler_path;
		fs::path assembler_path = fs::current_path() /= _assembler_path;
		fs::path linker_path = fs::current_path() /= _linker_path;
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
		fs::rename(compiler_path/input.assembly, assembler_path/input.assembly);

		fs::current_path(assembler_path);
		std::string masm_cmd = std::format("ml64.exe /Fo {} /c {}", input.object, input.assembly);
		system(masm_cmd.c_str());
		fs::current_path(compiler_path);
		fs::rename(assembler_path/input.object, linker_path/input.object);

		fs::current_path(linker_path);
		std::string link_cmd = std::format("link.exe /out:{} {} /subsystem:console "
			//libucrt.lib kernel32.lib 
			//"/libpath:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.19041.0\\um\\x64"
			//"/libpath:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.19041.0\\ucrt\\x64"
			" /entry:main", input.executable, input.object);
		system(link_cmd.c_str());

		std::string exe_cmd = std::format("{}", input.executable);
		system(exe_cmd.c_str());

		return true;
	}

}
