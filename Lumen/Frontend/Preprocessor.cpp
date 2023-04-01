#include "Preprocessor.h"
#include "SourceBuffer.h"
#include "Token.h"
#include "Lexer.h"
#include "Hideset.h"
#include "Core/Defines.h"

namespace lu
{
	Preprocessor::Preprocessor()
	{
		Reset();
	}

	void Preprocessor::Reset()
	{
		while (!conditional_includes.empty()) conditional_includes.pop();
		macros.clear();
		pragma_once.clear();
		pp_tokens.clear();
	}

	bool Preprocessor::Preprocess(Lexer& lexer)
	{
		if (lexer.pp_tokens_count == 0)
		{
			//diag
			return true;
		}

		for (auto& token : lexer.tokens) pp_tokens.emplace_back(token);
		
		auto curr = pp_tokens.begin();
		while (curr->token.IsNot(TokenType::eof))
		{
			//#todo expand macro
			
			if (!curr->token.IsPPKeyword())
			{
				++curr;
				continue;
			}
			
			TokenType pp_token_type = curr->token.GetType();
			++curr;
			switch (pp_token_type)
			{
			case TokenType::PP_if:
			{
				LU_ASSERT_MSG(false, "Not supported yet!");
			}
			break;
			case TokenType::PP_else:
			{
				if (!ProcessElse(curr)) return false;
				break;
			}
			break;
			case TokenType::PP_elif:
			{
				LU_ASSERT_MSG(false, "Not supported yet!");
			}
			break;
			case TokenType::PP_ifdef:
				if (!ProcessIfDef(curr)) return false;
				break;
			break;
			case TokenType::PP_ifndef:
				if (!ProcessIfNDef(curr)) return false;
				break;
			case TokenType::PP_elifdef:
				LU_ASSERT_MSG(false, "Not supported yet!");
				break;
			case TokenType::PP_elifndef:
				LU_ASSERT_MSG(false, "Not supported yet!");
				break;
			case TokenType::PP_endif:
				LU_ASSERT_MSG(false, "Not supported yet!");
				break;
			case TokenType::PP_defined:
				LU_ASSERT_MSG(false, "Not supported yet!");
				break;
			case TokenType::PP_include:
				if (!ProcessInclude(curr)) return false;
				break;
			case TokenType::PP_define:
				if (!ProcessDefine(curr)) return false;
				break;
			case TokenType::PP_undef:
				if (!ProcessUndef(curr)) return false;
				break;
			}
		}

		lexer.tokens.clear();
		for (auto&& pp_token : pp_tokens)
		{
			if (pp_token.token.IsPPKeyword()) continue;
			lexer.tokens.push_back(std::move(pp_token.token));
		}
		return true;
	}

	bool Preprocessor::ProcessInclude(PPTokenPtr& curr)
	{
		using enum TokenType;
		if (curr->token.IsOneOf(string_literal)) //add <>
		{
			std::string_view filename = curr->token.GetIdentifier();
			if (pragma_once.contains(filename)) return true;
			
			SourceBuffer src(filename);
			Lexer lexer(src);
			if (!lexer.Lex())
			{
				//diag
				return false;
			}
			curr = pp_tokens.erase(curr);
			lexer.tokens.pop_back(); //pop eof
			for (auto&& token : lexer.tokens) pp_tokens.emplace(curr, token);
			return true;
		}
		return false;
	}

	bool Preprocessor::ProcessDefine(PPTokenPtr& curr)
	{
		if (curr->token.IsNot(TokenType::identifier))
		{
			return false;
		}

		std::string_view macro_name = curr->token.GetIdentifier();
		Macro m{ .name = macro_name };
		++curr;
		if (!curr->token.HasFlag(TokenFlag_LeadingSpace) && curr->token.Is(TokenType::left_round))
		{
			m.is_function = true;
			while (curr->token.IsNot(TokenType::right_brace))
			{
				++curr;
				if (curr->token.IsNot(TokenType::identifier)) return false;
				m.params.push_back(curr->token.GetIdentifier());
				++curr;
				if (curr->token.Is(TokenType::comma)) ++curr;
			}
			while (curr->token.IsNot(TokenType::newline))
			{
				m.body.push_back(&*curr);
			}
		}
		else
		{
			m.is_function = false;
			while (curr->token.IsNot(TokenType::newline))
			{
				m.body.push_back(&*curr);
			}
		}
		macros[macro_name] = std::move(m);
		return true;
	}

	bool Preprocessor::ProcessUndef(PPTokenPtr& curr)
	{
		if (curr->token.IsNot(TokenType::identifier)) 
		{
			return false;
		}
		macros.erase(curr->token.GetIdentifier());
		return true;
	}

	bool Preprocessor::ProcessIfDef(PPTokenPtr& curr)
	{
		if (curr->token.IsNot(TokenType::identifier))
		{
			return false;
		}
		bool defined = macros.contains(curr->token.GetIdentifier());
		conditional_includes.emplace(ConditionalIncludeContext::If, defined);

		++curr;
		if (curr->token.IsNot(TokenType::newline))
		{
			//too much tokens
			return false;
		}
		if (!defined) IgnoreConditionalIncludes(curr);
		return true;
	}

	bool Preprocessor::ProcessIfNDef(PPTokenPtr& curr)
	{
		if (curr->token.IsNot(TokenType::identifier))
		{
			return false;
		}
		bool defined = macros.contains(curr->token.GetIdentifier());
		conditional_includes.emplace(ConditionalIncludeContext::If, !defined);

		++curr;
		if (curr->token.IsNot(TokenType::newline))
		{
			return false;
		}
		if (defined) IgnoreConditionalIncludes(curr);
		return true;
	}

	bool Preprocessor::ProcessElse(PPTokenPtr& curr)
	{
		if (conditional_includes.empty() 
			|| conditional_includes.top().ctx == ConditionalIncludeContext::Else)
		{
			return false;
		}
		conditional_includes.top().ctx = ConditionalIncludeContext::Else;
		++curr;
		if (curr->token.IsNot(TokenType::newline))
		{
			//too much tokens
			return false;
		}
		if (conditional_includes.top().included) IgnoreConditionalIncludes(curr);
		return true;
	}

	void Preprocessor::IgnoreConditionalIncludes(PPTokenPtr& curr)
	{
		using enum TokenType;
		while (curr->token.IsNot(eof))
		{
			if (curr->token.IsOneOf(PP_if, PP_ifdef, PP_ifndef))
			{
				IgnoreConditionalIncludesUtil(++curr);
				continue;
			}
			if (curr->token.IsOneOf(PP_elif, PP_else, PP_endif)) break;
			++curr;
		}
	}

	void Preprocessor::IgnoreConditionalIncludesUtil(PPTokenPtr& curr)
	{
		using enum TokenType;
		while (curr->token.IsNot(eof))
		{
			if (curr->token.IsOneOf(PP_if, PP_ifdef, PP_ifndef))
			{
				IgnoreConditionalIncludesUtil(++curr);
				continue;
			}
			if (curr->token.IsOneOf(PP_endif))
			{
				++curr;
				return;
			}
			++curr;
		}
	}

}
