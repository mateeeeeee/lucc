#include "Preprocessor.h"
#include "SourceBuffer.h"
#include "Token.h"
#include "Lexer.h"
#include "Hideset.h"
#include "Diagnostics.h"

//preprocessor todo list:
//- macros function
//- stringify, concat operators
//- #if,#elif

namespace lucc
{
	Preprocessor::Preprocessor(Lexer& lexer) : lexer(lexer)
	{}

	void Preprocessor::Preprocess()
	{
		for (auto const& token : lexer.tokens)
		{
			if(token.IsNot(TokenKind::comment)) pp_tokens.emplace_back(token);
		}
		
		auto curr = pp_tokens.begin();
		while (curr->IsNot(TokenKind::eof))
		{
			if (ExpandMacro(curr)) continue;
			
			if (!curr->IsPPKeyword())
			{
				++curr;
				continue;
			}
			
			TokenKind pp_token_type = curr->GetKind();
			++curr;
			switch (pp_token_type)
			{
			case TokenKind::PP_if:
			{
				LU_ASSERT_MSG(false, "Not supported yet!");
			}
			break;
			case TokenKind::PP_else:
			{
				if (!ProcessElse(curr)) Report(diag::preprocessing_failed);;
				break;
			}
			break;
			case TokenKind::PP_elif:
			{
				LU_ASSERT_MSG(false, "Not supported yet!");
			}
			break;
			case TokenKind::PP_ifdef:
				if (!ProcessIfDef(curr)) Report(diag::preprocessing_failed);
				break;
			break;
			case TokenKind::PP_ifndef:
				if (!ProcessIfNDef(curr)) Report(diag::preprocessing_failed);
				break;
			case TokenKind::PP_elifdef:
				if (!ProcessElifDef(curr)) Report(diag::preprocessing_failed);
				break;
			case TokenKind::PP_elifndef:
				if (!ProcessElifNDef(curr)) Report(diag::preprocessing_failed);
				break;
			case TokenKind::PP_endif:
				if (!ProcessEndif(curr)) Report(diag::preprocessing_failed);
				break;
			case TokenKind::PP_include:
				if (!ProcessInclude(curr)) Report(diag::preprocessing_failed);
				break;
			case TokenKind::PP_define:
				if (!ProcessDefine(curr)) Report(diag::preprocessing_failed);
				break;
			case TokenKind::PP_undef:
				if (!ProcessUndef(curr)) Report(diag::preprocessing_failed);
				break;
			case TokenKind::PP_defined:
				Report(diag::defined_misplaced, curr->GetLocation());
			}
		}

		lexer.tokens.clear();
		for (auto&& token : pp_tokens)
		{
			if (token.IsPPKeyword()) continue;
			if (token.IsOneOf(TokenKind::comment, TokenKind::newline)) continue;
			if (token.HasFlag(TokenFlag_PartOfPPDirective)) continue;
			if (token.HasFlag(TokenFlag_SkipedByPP)) continue;
			lexer.tokens.push_back(std::move(token));
		}
	}

	bool Preprocessor::ProcessInclude(TokenPtr& curr)
	{
		using enum TokenKind;
		if (curr->IsOneOf(string_literal)) //add <>
		{
			std::string_view filename = curr->GetIdentifier();
			if (pragma_once.contains(filename)) return true;
			
			SourceBuffer src(filename);
			Lexer include_lexer(src);
			include_lexer.Lex();
			curr->SetFlag(TokenFlag_PartOfPPDirective);
			++curr;
			include_lexer.tokens.pop_back(); //pop eof
			for (auto&& token : include_lexer.tokens) pp_tokens.emplace(curr, token);
			std::advance(curr, -static_cast<int64>(include_lexer.tokens.size()));
			return true;
		}
		Report(diag::header_name_error, curr->GetLocation());
		return false;
	}

	bool Preprocessor::ProcessDefine(TokenPtr& curr)
	{
		if (curr->IsNot(TokenKind::identifier))
		{
			return false;
		}
		TokenPtr start = curr;

		std::string_view macro_name = curr->GetIdentifier();
		Macro m{ .name = macro_name };
		
		++curr;
		if (!curr->HasFlag(TokenFlag_LeadingSpace) && curr->Is(TokenKind::left_round))
		{
			m.is_function = true;
			while (curr->IsNot(TokenKind::right_brace))
			{
				++curr;
				if (curr->IsNot(TokenKind::identifier)) return false;
				m.params.push_back(curr->GetIdentifier());
				++curr;
				if (curr->Is(TokenKind::comma)) ++curr;
			}
			while (curr->IsNot(TokenKind::newline))
			{
				m.body.push_back(&*curr);
				++curr;
			}
		}
		else
		{
			m.is_function = false;
			while (curr->IsNot(TokenKind::newline))
			{
				m.body.push_back(&*curr);
				++curr;
			}
		}
		TokenPtr end = curr;
		for (TokenPtr tok = start; tok != end; ++tok) tok->SetFlag(TokenFlag_PartOfPPDirective);
		macros[macro_name] = std::move(m);
		return true;
	}

	bool Preprocessor::ProcessUndef(TokenPtr& curr)
	{
		if (curr->IsNot(TokenKind::identifier)) 
		{
			return false;
		}
		curr->SetFlag(TokenFlag_PartOfPPDirective);
		macros.erase(curr->GetIdentifier());
		return true;
	}

	bool Preprocessor::ProcessIfDef(TokenPtr& curr)
	{
		if (curr->IsNot(TokenKind::identifier))
		{
			return false;
		}
		curr->SetFlag(TokenFlag_PartOfPPDirective);
		bool defined = macros.contains(curr->GetIdentifier());
		++curr;
		if (curr->IsNot(TokenKind::newline))
		{
			//too much tokens
			return false;
		}
		conditional_includes.emplace(ConditionalIncludeContext::If, defined);
		if (!defined) IgnoreConditionalIncludes(curr);
		return true;
	}

	bool Preprocessor::ProcessElifDef(TokenPtr& curr)
	{
		if (conditional_includes.empty())
		{
			return false;
		}
		ConditionalInclude& curr_cond_incl = conditional_includes.top();
		if (curr_cond_incl.ctx == ConditionalIncludeContext::Else)
		{
			return false;
		}
		curr_cond_incl.ctx = ConditionalIncludeContext::Elif;
		if (curr->IsNot(TokenKind::identifier))
		{
			return false;
		}
		curr->SetFlag(TokenFlag_PartOfPPDirective);
		bool defined = macros.contains(curr->GetIdentifier());

		++curr;
		if (curr->IsNot(TokenKind::newline))
		{
			//too much tokens
			return false;
		}
		if (!curr_cond_incl.included && defined) curr_cond_incl.included = true;
		else IgnoreConditionalIncludes(curr);
		return true;
	}

	bool Preprocessor::ProcessElifNDef(TokenPtr& curr)
	{
		if (conditional_includes.empty())
		{
			return false;
		}
		ConditionalInclude& curr_cond_incl = conditional_includes.top();
		if (curr_cond_incl.ctx == ConditionalIncludeContext::Else)
		{
			return false;
		}
		curr_cond_incl.ctx = ConditionalIncludeContext::Elif;
		if (curr->IsNot(TokenKind::identifier))
		{
			return false;
		}
		curr->SetFlag(TokenFlag_PartOfPPDirective);
		bool defined = macros.contains(curr->GetIdentifier());
		++curr;
		if (curr->IsNot(TokenKind::newline))
		{
			//too much tokens
			return false;
		}
		if (!curr_cond_incl.included && !defined) curr_cond_incl.included = true;
		else IgnoreConditionalIncludes(curr);
		return true;
	}

	bool Preprocessor::ProcessIfNDef(TokenPtr& curr)
	{
		if (curr->IsNot(TokenKind::identifier))
		{
			return false;
		}
		curr->SetFlag(TokenFlag_PartOfPPDirective);
		++curr;
		if (curr->IsNot(TokenKind::newline))
		{
			return false;
		}
		bool defined = macros.contains(curr->GetIdentifier());
		conditional_includes.emplace(ConditionalIncludeContext::If, !defined);
		if (defined) IgnoreConditionalIncludes(curr);
		return true;
	}

	bool Preprocessor::ProcessElse(TokenPtr& curr)
	{
		if (conditional_includes.empty())
		{
			return false;
		}
		ConditionalInclude& curr_cond_incl = conditional_includes.top();
		if (curr_cond_incl.ctx == ConditionalIncludeContext::Else)
		{
			return false;
		}
		curr_cond_incl.ctx = ConditionalIncludeContext::Else;
		if (curr->IsNot(TokenKind::newline))
		{
			//too much tokens
			return false;
		}
		if (curr_cond_incl.included) IgnoreConditionalIncludes(curr);
		return true;
	}

	bool Preprocessor::ProcessEndif(TokenPtr& curr)
	{
		if (conditional_includes.empty())
		{
			return false;
		}
		if (curr->IsNot(TokenKind::newline) && curr->IsNot(TokenKind::eof))
		{
			//too much tokens
			return false;
		}
		conditional_includes.pop();
		return true;
	}

	void Preprocessor::IgnoreConditionalIncludes(TokenPtr& curr)
	{
		using enum TokenKind;
		while (curr->IsNot(eof))
		{
			if (curr->IsOneOf(PP_if, PP_ifdef, PP_ifndef))
			{
				IgnoreConditionalIncludesUtil(++curr);
				continue;
			}
			if (curr->IsOneOf(PP_elif, PP_elifdef, PP_elifndef, PP_else, PP_endif)) break;
			curr->SetFlag(TokenFlag_SkipedByPP);
			++curr;
		}
	}

	void Preprocessor::IgnoreConditionalIncludesUtil(TokenPtr& curr)
	{
		using enum TokenKind;
		while (curr->IsNot(eof))
		{
			if (curr->IsOneOf(PP_if, PP_ifdef, PP_ifndef))
			{
				IgnoreConditionalIncludesUtil(++curr);
				continue;
			}
			if (curr->IsOneOf(PP_endif))
			{
				++curr;
				return;
			}
			curr->SetFlag(TokenFlag_SkipedByPP);
			++curr;
		}
	}

	bool Preprocessor::ExpandMacro(TokenPtr& curr)
	{
		std::string_view id = curr->GetIdentifier();
		if (!macros.contains(id)) return false;

		Macro const& m = macros[id];
		if (!m.is_function) 
		{
			TokenFlags flags = curr->GetFlags();
			curr = pp_tokens.erase(curr);
			for (auto&& token : m.body)
			{
				auto emplaced_token = pp_tokens.emplace(curr, *token);
				emplaced_token->ClearFlag(TokenFlag_PartOfPPDirective);
			}
			std::advance(curr, -static_cast<int64>(m.body.size()));
			curr->SetFlags(flags);
			curr->ClearFlag(TokenFlag_PartOfPPDirective);
			return true;
		}
		return false;
	}

}

/*
The ‘#if’ directive allows you to test the value of an arithmetic expression, rather than the mere existence of one macro. Its syntax is

#if expression

controlled text

#endif // expression
expression is a C expression of integer type, subject to stringent restrictions.It may contain

- Integer constants.
- Character constants, which are interpreted as they would be in normal code.
- Arithmetic operators for addition, subtraction, multiplication, division, bitwise operations, shifts, comparisons, and logical operations(&& and|| ).The latter two obey the usual short - circuiting rules of standard C.
- Macros.All macros in the expression are expanded before actual computation of the expression’s value begins.
- Uses of the defined operator, which lets you check whether macros are defined in the middle of an ‘#if’.
- Identifiers that are not macros, which are all considered to be the number zero.This allows you to write #if MACRO instead of #ifdef MACRO, if you know that MACRO, when defined, will always have a nonzero value.Function - like macros used without their function call parentheses are also treated as zero.

preprocessor does not know anything about types in the language.Therefore, sizeof operators are not recognized in ‘#if’, and neither are enum constants.They will be taken as identifiers which are not macros, and replaced by zero.In the case of sizeof, this is likely to cause the expression to be invalid.
The preprocessor calculates the value of expression.It carries out all calculations in the widest integer type known to the compiler; on most machines supported by GCC this is 64 bits.This is not the same rule as the compiler uses to calculate the value of a constant expression, and may give different results in some cases.If the value comes out to be nonzero, the ‘#if’ succeedsand the controlled text is included; otherwise it is skipped.
*/
