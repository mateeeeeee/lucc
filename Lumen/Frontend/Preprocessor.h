#pragma once
#include <stack>
#include <string>
#include <list>
#include <unordered_set>
#include "Token.h"

namespace lu
{
	class Lexer;
	class Token;

	class Preprocessor
	{
		enum class ConditionalIncludeContext : uint8
		{
			If,
			Elif,
			Else
		};
		struct ConditionalInclude
		{
			ConditionalIncludeContext ctx = ConditionalIncludeContext::If;
			bool included = false;
		};

		using MacroParam = std::string_view;
		struct MacroArg
		{
			std::string_view name;
			Token* tok;
		};
		struct Macro
		{
			std::string_view name = "";
			bool is_function = false;
			std::vector<MacroParam> params{};
			std::vector<Token*> body{};
		};

	public:	
		Preprocessor(Lexer&);
		Preprocessor(Preprocessor const&) = delete;
		Preprocessor& operator=(Preprocessor const&) = delete;

		bool Preprocess();

	private:
		Lexer& lexer;
		std::list<Token> pp_tokens;
		std::stack<ConditionalInclude> conditional_includes;
		std::unordered_map<std::string_view, Macro> macros;
		std::unordered_map<std::string_view, bool> pragma_once;

	private:
		using TokenPtr = std::list<Token>::iterator;
		bool ProcessInclude(TokenPtr& curr);
		bool ProcessDefine(TokenPtr& curr);
		bool ProcessUndef(TokenPtr& curr);
		bool ProcessIfDef(TokenPtr& curr);
		bool ProcessElifDef(TokenPtr& curr);
		bool ProcessElifNDef(TokenPtr& curr);
		bool ProcessIfNDef(TokenPtr& curr);
		bool ProcessElse(TokenPtr& curr);
		bool ProcessEndif(TokenPtr& curr);

		void IgnoreConditionalIncludes(TokenPtr& curr);
		void IgnoreConditionalIncludesUtil(TokenPtr& curr);
		bool ExpandMacro(TokenPtr& curr);
	};
}