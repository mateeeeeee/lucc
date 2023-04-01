#pragma once
#include <stack>
#include <string>
#include <list>
#include <unordered_set>

namespace lu
{
	class Lexer;
	class Token;
	class Hideset;

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
		struct PreprocessorToken
		{
			PreprocessorToken(Token* token) : token(token) {}
			Token* token;
			std::unique_ptr<Hideset> hideset;
		};
		using PPTokenPtr = std::list<PreprocessorToken>::iterator;

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
			std::vector<PreprocessorToken*> body{};
		};

	public:	
		Preprocessor();
		Preprocessor(Preprocessor const&) = delete;
		Preprocessor& operator=(Preprocessor const&) = delete;
		void Reset();

		bool Preprocess(Lexer&);

	private:
		std::list<PreprocessorToken> pp_items;
		std::stack<ConditionalInclude> conditional_includes;
		std::unordered_map<std::string_view, Macro> macros;
		std::unordered_map<std::string_view, bool> pragma_once;

	private:
		bool ProcessDefine(PPTokenPtr& curr);
		bool ProcessUndef(PPTokenPtr& curr);
		bool ProcessIfDef(PPTokenPtr& curr);
		bool ProcessIfNDef(PPTokenPtr& curr);
		bool ProcessElse(PPTokenPtr& curr);

		void IgnoreConditionalIncludes(PPTokenPtr& curr);
		void IgnoreConditionalIncludesUtil(PPTokenPtr& curr);
	};
}