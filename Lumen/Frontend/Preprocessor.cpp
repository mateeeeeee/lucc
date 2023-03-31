#include <stack>
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
			explicit Hideset(char const* name) : hideset(name) {}

			void Union(Hideset const* hs)
			{
				std::set_union(hideset.begin(), hideset.end(), hs->hideset.begin(), hs->hideset.end(), hideset.begin());
			}
			void Intersection(Hideset const* hs)
			{
				std::set_intersection(hideset.begin(), hideset.end(), hs->hideset.begin(), hs->hideset.end(), hideset.begin());
			}
			bool Contains(char const* name) const
			{
				return hideset.contains(name);
			}
		private:
			std::unordered_set<std::string> hideset;
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
				if (token2->Is(TokenType::identifier) && IsPreprocessorKeyword())
				{

				}
			}
		}
	}

}
