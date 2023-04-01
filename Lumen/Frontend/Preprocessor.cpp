#include <stack>
#include <string>
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

			void Union(Hideset const* hs)
			{
				//if(!std::is_sorted(hideset.begin(), hideset.end())) std::sort(hideset.begin(), hideset.end());
				//if(!std::is_sorted(hs->hideset.begin(), hs->hideset.end())) std::sort(hs->hideset.begin(), hs->hideset.end());
				std::vector<std::string> union_hs;
				std::set_union(hideset.begin(), hideset.end(), hs->hideset.begin(), hs->hideset.end(), std::back_inserter(union_hs));
				std::swap(hideset, union_hs);
			}
			void Intersection(Hideset const* hs)
			{
				std::vector<std::string> intersection_hs;
				std::set_intersection(hideset.begin(), hideset.end(), hs->hideset.begin(), hs->hideset.end(), std::back_inserter(intersection_hs));
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
		struct PreprocessorToken
		{
			PreprocessorToken(Token* token) : lexer_token(token) {}
			void InitializeHideset(char const* name)
			{
				hideset = std::make_unique<Hideset>(name);
			}

			Token* lexer_token;
			std::unique_ptr<Hideset> hideset;
		};

		struct MacroParam
		{

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
		std::stack<ConditionalInclude> conditional_includes;
		
		void Reset()
		{
			while (!conditional_includes.empty()) conditional_includes.pop();
		}
		bool IsPrepreprocessorKeyword(Token* tok)
		{
			
		}
	}

	void Preprocessor::Preprocess(Lexer& lexer)
	{
		Reset();

		std::vector<PreprocessorToken> pp_tokens; pp_tokens.reserve((lexer.tokens.size()));
		for (auto& token : lexer.tokens) pp_tokens.emplace_back(&token);
		
		for (size_t i = 2; i < pp_tokens.size(); ++i)
		{
			auto const& token1 = pp_tokens[i - 2].lexer_token;
			auto const& token2 = pp_tokens[i - 1].lexer_token;
			if (token1->Is(TokenType::hash) && token1->GetFlag(TokenFlag_BeginningOfLine))
			{
				//if (token2->Is(TokenType::identifier) && IsPreprocessorKeyword())
				//{
				//
				//}
			}
		}
	}

}
