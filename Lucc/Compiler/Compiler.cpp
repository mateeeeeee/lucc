#include <filesystem>
#include <format>
#include "Compiler.h"
#include "Utility/DebugVisitor.h"
#include "Frontend/Diagnostics.h"
#include "Frontend/SourceBuffer.h"
#include "Frontend/Preprocessor.h"
#include "Frontend/Lexer.h"
#include "Frontend/Parser.h"
#include "Backend/x86_64Codegen.h"
#include "spdlog/spdlog.h"
#include "spdlog/sinks/stdout_color_sinks.h"
#include "spdlog/sinks/basic_file_sink.h"


namespace fs = std::filesystem;

namespace lucc
{
	namespace
	{
		constexpr char const* exe_path = EXE_PATH;
		constexpr char const* sdk_lib_path = SDK_PATH;
		constexpr char const* ucrt_lib_path = UCRT_PATH;
		constexpr char const* vc_lib_path = VC_PATH;
		constexpr char const* sdk_libs[] = { "kernel32.lib", "user32.lib", "gdi32.lib", "winspool.lib", "comdlg32.lib", "advapi32.lib", "shell32.lib", "ole32.lib", "oleaut32.lib", "uuid.lib", "odbc32.lib", "odbccp32.lib" };
		constexpr char const* ucrt_libs[] = { "ucrt.lib" };
		constexpr char const* vc_libs[] = { "legacy_stdio_definitions.lib", "legacy_stdio_wide_specifiers.lib", "msvcrt.lib" };

		void InitLogger()
		{
			auto console_sink = std::make_shared<spdlog::sinks::stdout_color_sink_mt>();
			console_sink->set_level(spdlog::level::trace);
			console_sink->set_pattern("[%^%l%$] %v");

			auto file_sink = std::make_shared<spdlog::sinks::basic_file_sink_mt>("lu_log.txt", true);
			file_sink->set_level(spdlog::level::trace);

			std::shared_ptr<spdlog::logger> lu_logger = std::make_shared<spdlog::logger>(std::string("lucc logger"), spdlog::sinks_init_list{ console_sink, file_sink });
			lu_logger->set_level(spdlog::level::trace);
			spdlog::set_default_logger(lu_logger);

		}
		void AddBuiltins(SourceBuffer& src)
		{
			src.Prepend("#define NULL (void*)0\n");
		}
		void CompileTranslationUnit(std::string_view source_file, std::string_view assembly_file, bool only_pp, bool ast_dump, bool output_debug)
		{
			SourceBuffer src(source_file);
			AddBuiltins(src);
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
			AST const* ast = parser.GetAST();
			if (ast_dump) debug::DebugVisitor visitor(ast);

			x86_64Codegen x86_64(assembly_file);
			x86_64.Generate(ast);
		}
	}

	int32 Compile(CompilerInput const& input)
	{
		InitLogger();
		bool const output_debug = input.flags & CompilerFlag_OutputDebugInfo;
		bool const ast_dump		= input.flags & CompilerFlag_DumpAST;
		bool const only_pp		= input.flags & CompilerFlag_OnlyPreprocessor;
		bool const no_link		= input.flags & CompilerFlag_NoLinking;
		bool const no_assembly	= input.flags & CompilerFlag_NoAssembling;
		bool const no_libs		= input.flags & CompilerFlag_NoDefaultLibs;

		fs::path directory_path = input.input_directory;
		std::vector<fs::path> object_files(input.sources.size());
		std::string masm_cmd = std::format("\"{}/ml64.exe\"", exe_path);
		for (uint64 i = 0; i < input.sources.size(); ++i)
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

		fs::path output_file = directory_path / input.output_file;
		switch (input.output_type)
		{
		case CompilerOutput::Exe:
		{
			output_file.replace_extension("exe");
			std::string link_cmd = std::format("\"\"{}/link.exe\" /out:{} ", exe_path, output_file.string());
			link_cmd += std::format("\"/libpath:{}\"", sdk_lib_path);
			for (auto const& obj_file : object_files) link_cmd += std::format(" {} ", obj_file.string());
			if (!no_libs)
			{
				for (char const* sdk_lib : sdk_libs) link_cmd += std::format("\"{}\\{}\" ", sdk_lib_path, sdk_lib);
				for (char const* ucrt_lib : ucrt_libs) link_cmd += std::format("\"{}\\{}\" ", ucrt_lib_path, ucrt_lib);
				for (char const* vc_lib : vc_libs) link_cmd += std::format("\"{}\\{}\" ", vc_lib_path, vc_lib);
			}
			link_cmd += "/subsystem:console /entry:main\"";
			system(link_cmd.c_str());

			std::string exe_cmd = std::format("{}", output_file.string());
			return system(exe_cmd.c_str());
		}
		case CompilerOutput::Dll:
		{
			output_file.replace_extension("dll");
			std::string link_cmd = std::format("\"\"{}/link.exe\" /dll /out:{} ", exe_path, output_file.string());
			link_cmd += std::format("\"/libpath:{}\"", sdk_lib_path);
			for (auto const& obj_file : object_files) link_cmd += std::format(" {} ", obj_file.string());
			if (!no_libs)
			{
				for (char const* sdk_lib : sdk_libs) link_cmd += std::format("\"{}\\{}\" ", sdk_lib_path, sdk_lib);
				for (char const* ucrt_lib : ucrt_libs) link_cmd += std::format("\"{}\\{}\" ", ucrt_lib_path, ucrt_lib);
				for (char const* vc_lib : vc_libs) link_cmd += std::format("\"{}\\{}\" ", vc_lib_path, vc_lib);
			}
			link_cmd += "/entry:DllMain\"";
			system(link_cmd.c_str());
			return 0;
		}
		case CompilerOutput::Lib:
		{
			output_file.replace_extension("lib");
			std::string lib_cmd = std::format("\"\"{}/lib.exe\" /out:{} ", exe_path, output_file.string());
			lib_cmd += std::format("\"/libpath:{}\"", sdk_lib_path);
			for (auto const& obj_file : object_files) lib_cmd += std::format(" {} ", obj_file.string());
			if (!no_libs)
			{
				for (char const* sdk_lib : sdk_libs) lib_cmd += std::format(" \"{}\\{}\" ", sdk_lib_path, sdk_lib);
				for (char const* ucrt_lib : ucrt_libs) lib_cmd += std::format("\"{}\\{}\" ", ucrt_lib_path, ucrt_lib);
				for (char const* vc_lib : vc_libs) lib_cmd += std::format("\"{}\\{}\" ", vc_lib_path, vc_lib);
			}
			system(lib_cmd.c_str());
		}
		}
		return 0;
	}

	int32 CompileTest(std::string_view test_code, bool debug)
	{
		InitLogger();
		std::string code(test_code);

		fs::path tmp_directory = std::filesystem::current_path() / "Tmp";
		fs::create_directory(tmp_directory);

		fs::path file_name = "tmp";
		fs::path assembly_file = tmp_directory / file_name; assembly_file += ".asm";
		fs::path object_file   = tmp_directory / file_name; object_file += ".obj";
		fs::path output_file   = tmp_directory / file_name; output_file += ".exe";

		//compilation
		{
			SourceBuffer src(code.data(), code.size());
			AddBuiltins(src);
			Lexer lex(src);
			lex.Lex();
			if (debug) debug::PrintTokens("After lexer:", lex.GetTokens());

			Preprocessor pp(lex);
			pp.Preprocess();
			if (debug) debug::PrintTokens("\n\nAfter preprocessor:", lex.GetTokens());

			Parser parser(lex.GetTokens());
			parser.Parse();

			AST const* ast = parser.GetAST();
			if (debug) debug::DebugVisitor visitor(ast);

			x86_64Codegen x86_64(assembly_file.string());
			x86_64.Generate(ast);
		}

		std::string masm_cmd = std::format("\"{}/ml64.exe\"  /Fo {} /c {}", exe_path, object_file.string(), assembly_file.string());
		system(masm_cmd.c_str());
		std::string link_cmd = std::format("\"\"{}/link.exe\" /out:{} {}", exe_path, output_file.string(), object_file.string());
		for (char const* sdk_lib : sdk_libs) link_cmd += std::format(" \"{}\\{}\" ", sdk_lib_path, sdk_lib);
		for (char const* ucrt_lib : ucrt_libs) link_cmd += std::format("\"{}\\{}\" ", ucrt_lib_path, ucrt_lib);
		link_cmd += " /subsystem:console /entry:main \"";
		system(link_cmd.c_str());

		std::string exe_cmd = std::format("{}", output_file.string());
		int32 exitcode = system(exe_cmd.c_str());
		fs::remove_all(tmp_directory);
		return exitcode;
	}

}