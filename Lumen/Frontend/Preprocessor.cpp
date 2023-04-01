#include <stack>
#include <string>
#include <list>
#include <unordered_set>
#include "Preprocessor.h"
#include "Token.h"
#include "Lexer.h"


namespace lumen
{
	namespace
	{
		class Hideset
		{
		public:
			explicit Hideset(char const* name) : hideset() 
			{
				hideset.push_back(name);
			}

			void Union(Hideset const& hs)
			{
				//if(!std::is_sorted(hideset.begin(), hideset.end())) std::sort(hideset.begin(), hideset.end());
				//if(!std::is_sorted(hs->hideset.begin(), hs->hideset.end())) std::sort(hs->hideset.begin(), hs->hideset.end());
				std::vector<std::string> union_hs;
				std::set_union(hideset.begin(), hideset.end(), hs.hideset.begin(), hs.hideset.end(), std::back_inserter(union_hs));
				std::swap(hideset, union_hs);
			}
			void Intersection(Hideset const& hs)
			{
				std::vector<std::string> intersection_hs;
				std::set_intersection(hideset.begin(), hideset.end(), hs.hideset.begin(), hs.hideset.end(), std::back_inserter(intersection_hs));
				std::swap(hideset, intersection_hs);
			}
			bool Contains(char const* name) const
			{
				for (auto const& s : hideset) if (s.compare(name)) return true;
				return false;
			}
		private:
			std::vector<std::string> hideset;
		};
		struct PreprocessorItem
		{
			PreprocessorItem(Token* token) : token(token) {}
			void InitializeHideset(char const* name)
			{
				hideset = std::make_unique<Hideset>(name);
			}
			
			Token* token;
			std::unique_ptr<Hideset> hideset;
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
			std::vector<PreprocessorItem*> body{};
		};

		enum class CondInclContext : uint8
		{
			If,
			Elif,
			Else
		};
		struct ConditionalInclude
		{
			CondInclContext ctx = CondInclContext::If;
			bool included = false;
			Token* token = nullptr;
		};

		std::list<PreprocessorItem> pp_items;
		std::stack<ConditionalInclude> conditional_includes;
		std::unordered_map<std::string_view, Macro> macros;
		std::unordered_map<std::string_view, bool> pragma_once;
		
		using PPListIterator = std::list<PreprocessorItem>::iterator;

		void Reset()
		{
			while (!conditional_includes.empty()) conditional_includes.pop();
			macros.clear();
			pragma_once.clear();
			pp_items.clear();
		}

		bool ProcessDefine(PPListIterator& pp_item)
		{
			std::string_view macro_name = pp_item->token->GetData();
			Macro m{.name = macro_name};
			++pp_item;
			if (!pp_item->token->HasFlag(TokenFlag_LeadingSpace) && pp_item->token->Is(TokenType::left_round))
			{
				m.is_function = true;
				while (pp_item->token->IsNot(TokenType::right_brace))
				{
					++pp_item;
					if (pp_item->token->IsNot(TokenType::identifier)) return false;
					m.params.push_back(pp_item->token->GetData());
					++pp_item;
					if (pp_item->token->Is(TokenType::comma)) ++pp_item;
				}
				while (pp_item->token->IsNot(TokenType::newline))
				{
					m.body.push_back(&*pp_item);
				}
			}
			else
			{
				m.is_function = false;
				while (pp_item->token->IsNot(TokenType::newline))
				{
					m.body.push_back(&*pp_item);
				}
			}
			macros[macro_name] = std::move(m);
		}
		
	}

	bool Preprocessor::Preprocess(Lexer& lexer)
	{
		Reset();
		for (auto& token : lexer.tokens) pp_items.emplace_back(&token);
		
		auto curr = pp_items.begin();
		while (curr->token->IsNot(TokenType::eof))
		{
			//#todo expand macro
			
			if (!curr->token->IsPPKeyword())
			{
				++curr;
				continue;
			}
			
			switch (curr->token->GetType())
			{
			case TokenType::PP_if:
			{

			}
			break;
			case TokenType::PP_else:
			{

			}
			break;
			case TokenType::PP_elif:
			{

			}
			break;
			case TokenType::PP_ifdef:
			{

			}
			break;
			case TokenType::PP_ifndef:
			{

			}
			break;
			case TokenType::PP_elifdef:
			{

			}
			break;
			case TokenType::PP_elifndef:
			{

			}
			break;
			case TokenType::PP_endif:
			{

			}
			break;
			case TokenType::PP_defined:
			{

			}
			break;
			case TokenType::PP_include:
			{

			}
			break;
			case TokenType::PP_define:
			{
				++curr;
				if (curr->token->IsNot(TokenType::identifier))
				{
					//diag-error 
					return false;
				}
				if (!ProcessDefine(curr)) return false;
			}
			break;
			case TokenType::PP_undef:
			{

			}
			break;
			}
		}

		return true;
	}

}
