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
#if 0
		constexpr char const* _executables_path = "C:\\Program Files\\Microsoft Visual Studio\\2022\\Professional\\VC\\Tools\\MSVC\\14.34.31933\\bin\\Hostx64\\x64";
		constexpr char const* _lib_path = "C:\\Program Files\\Microsoft Visual Studio\\2022\\Professional\\VC\\Tools\\MSVC\\14.34.31933\\lib\\x64";
#else
		constexpr char const* _executables_path = "C:\\Program Files\\Microsoft Visual Studio\\2022\\Preview\\VC\\Tools\\MSVC\\14.34.31823\\bin\\Hostx64\\x64";
		constexpr char const* _lib_path = "C:\\Program Files\\Microsoft Visual Studio\\2022\\Preview\\VC\\Tools\\MSVC\\14.34.31823\\lib\\x64";
#endif

		void CompileTranslationUnit(std::string_view source_file, std::string_view assembly_file, bool only_pp, bool ast_dump, bool output_debug)
		{
			SourceBuffer src(source_file);
			Lexer lex(src);
			lex.Lex();
			if (output_debug) debug::PrintTokens("After lexer:", lex.GetTokens());

			Preprocessor pp(lex);
			pp.Preprocess();
			if (only_pp)
			{
				//#todo write preprocessed file
				return;
			}
			if (output_debug) debug::PrintTokens("\n\nAfter preprocessor:", lex.GetTokens());

			Parser parser(lex.GetTokens());
			parser.Parse();
			AST* ast = parser.GetAST();
			if (ast_dump) debug::DebugNodeVisitorAST visitor(ast);

			x86_64CodeGenerator x86_64(assembly_file);
			x86_64.Generate(ast);
		}
	}

	int Compile(CompilerInput const& input)
	{
		diag::Initialize();
		bool const output_debug = input.flags & CompilerFlag_OutputDebugInfo;
		bool const ast_dump		= input.flags & CompilerFlag_DumpAST;
		bool const only_pp		= input.flags & CompilerFlag_OnlyPreprocessor;
		bool const no_link		= input.flags & CompilerFlag_NoLinking;
		bool const no_assembly	= input.flags & CompilerFlag_NoAssembling;

		fs::path directory_path = input.input_directory;
		std::vector<fs::path> object_files(input.sources.size());
		std::string masm_cmd = std::format("\"{}/ml64.exe\"", _executables_path);
		for (size_t i = 0; i < input.sources.size(); ++i)
		{
			fs::path file_name = fs::path(input.sources[i]).stem();
			fs::path file_ext  = fs::path(input.sources[i]).extension();
			fs::path assembly_file = directory_path / file_name;  assembly_file += ".asm";
			fs::path object_file   = directory_path / file_name; object_file += ".obj";
			fs::path source_file = directory_path / input.sources[i];

			CompileTranslationUnit(source_file.string(), assembly_file.string(), only_pp, ast_dump, output_debug);

			masm_cmd += std::format(" /Fo {} /c {} ", object_file.string(), assembly_file.string());
			object_files[i] = object_file;
		}

		if (no_assembly) return 0;
		system(masm_cmd.c_str());

		if (no_link) return 0;
		std::string link_cmd = std::format("\"\"{}/link.exe\" /out:{} ", _executables_path, input.exe_file);
		for (auto const& obj_file : object_files) link_cmd += obj_file.string() + " "; ///msvcrt.lib libpath:\"\"{}\"\"
		link_cmd += std::format("\"/libpath:{}\"", _lib_path);
		link_cmd += "/subsystem:console /entry:main\"";
		system(link_cmd.c_str());

		std::string exe_cmd = std::format("{}", input.exe_file);
		return system(exe_cmd.c_str());
	}

	int CompileTest(std::string_view test_code, bool debug)
	{
		std::string code(test_code);
		diag::Initialize();

		fs::path tmp_directory = std::filesystem::current_path() / "Temp";
		fs::create_directory(tmp_directory);

		fs::path file_name = "tmp";
		fs::path assembly_file = tmp_directory / file_name; assembly_file += ".asm";
		fs::path object_file   = tmp_directory / file_name; object_file += ".obj";
		fs::path output_file   = tmp_directory / file_name; output_file += ".exe";

		//compilation
		{
			SourceBuffer src(code.data(), code.size());
			Lexer lex(src);
			lex.Lex();
			if (debug) debug::PrintTokens("After lexer:", lex.GetTokens());

			Preprocessor pp(lex);
			pp.Preprocess();
			if (debug) debug::PrintTokens("\n\nAfter preprocessor:", lex.GetTokens());

			Parser parser(lex.GetTokens());
			parser.Parse();

			AST* ast = parser.GetAST();
			if (debug) debug::DebugNodeVisitorAST visitor(ast);

			x86_64CodeGenerator x86_64(assembly_file.string());
			x86_64.Generate(ast);
		}
		std::string masm_cmd = std::format("\"{}/ml64.exe\"  /Fo {} /c {}", _executables_path, object_file.string(), assembly_file.string());
		system(masm_cmd.c_str());
		std::string link_cmd = std::format("\"{}/link.exe\" /out:{} {} /subsystem:console /entry:main", _executables_path, output_file.string(), object_file.string());
		system(link_cmd.c_str());

		std::string exe_cmd = std::format("{}", output_file.string());
		int32 exitcode = system(exe_cmd.c_str());
		if(!debug) fs::remove_all(tmp_directory);
		return exitcode;
	}

}
