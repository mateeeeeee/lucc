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
		constexpr char const* _compiler_path = "Compiler/";
		//C:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Tools\MSVC\14.34.31933
		//C:\\Program Files\\Microsoft Visual Studio\\2022\\Preview\\VC\\Tools\\MSVC\\14.34.31823
		constexpr char const* _executables_path = "C:\\Program Files\\Microsoft Visual Studio\\2022\\Preview\\VC\\Tools\\MSVC\\14.34.31823\\bin\\Hostx64\\x64";
		constexpr char const* _lib_path = "C:\\Program Files\\Microsoft Visual Studio\\2022\\Preview\\VC\\Tools\\MSVC\\14.34.31823\\lib\\x64";

		void CompileTranslationUnit(std::string_view source_file, std::string_view assembly_file, bool output_debug)
		{
			SourceBuffer src(source_file);
			Lexer lex(src);
			lex.Lex();
			if (output_debug) debug::PrintTokens("After lexer:", lex.GetTokens());

			Preprocessor pp(lex);
			pp.Preprocess();
			if (output_debug) debug::PrintTokens("\n\nAfter preprocessor:", lex.GetTokens());

			Parser parser(lex.GetTokens());
			parser.Parse();
			AST* ast = parser.GetAST();
			if (output_debug) debug::DebugNodeVisitorAST visitor(ast);

			x86_64CodeGenerator x86_64(assembly_file);
			x86_64.Generate(ast);
		}
	}

	void Compile(CompilerInput const& input)
	{
		diag::Initialize();
		fs::path compiler_path = fs::current_path() /= _compiler_path;
		fs::current_path(compiler_path);
		bool const output_debug = input.flags & CompilerFlag_OutputDebugInfo;

		std::vector<fs::path> object_files(input.sources.size());
		std::string masm_cmd = std::format("\"{}/ml64.exe\"", _executables_path);
		for (size_t i = 0; i < input.sources.size(); ++i)
		{
			fs::path file_name = fs::path(input.sources[i]).stem();
			fs::path file_ext  = fs::path(input.sources[i]).extension();
			fs::path assembly_file = file_name;  assembly_file += ".asm";
			fs::path object_file   = file_name; object_file += ".obj";

			CompileTranslationUnit(input.sources[i], assembly_file.string(), output_debug);

			masm_cmd += std::format(" /Fo {} /c {} ", object_file.string(), assembly_file.string());
			object_files[i] = object_file;
		}
		system(masm_cmd.c_str());

		std::string link_cmd = std::format("\"{}/link.exe\" /out:{} ", _executables_path, input.exe_file);
		for (auto const& obj_file : object_files) link_cmd += obj_file.string() + " "; ///msvcrt.lib libpath:\"\"{}\"\"
		link_cmd += "/subsystem:console /entry:main";
		system(link_cmd.c_str());

		std::string exe_cmd = std::format("{}", input.exe_file);
		system(exe_cmd.c_str());
	}

}
