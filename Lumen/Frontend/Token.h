#pragma once
#include <string_view>
#include "TokenTypes.h"
#include "SourceLocation.h"
#include "Core/Enums.h"

namespace lu
{

	enum TokenFlag : uint32
	{
		TokenFlag_None = 0x0,
		TokenFlag_LeadingSpace = 0x1,
		TokenFlag_BeginningOfLine = 0x2,
	};
	DEFINE_ENUM_BIT_OPERATORS(TokenFlag);
	using TokenFlags = uint32;

	class Token
	{
	public:
		Token() : type(TokenType::unknown), flags(TokenFlag_None), loc{}, data{} {}
		
		void Reset()
		{
			type = TokenType::unknown;
			flags = TokenFlag_None;
			loc = {};
			data = {};
		}

		TokenType GetType() const { return type; }
		void SetType(TokenType t) { type = t; }

		bool Is(TokenType t) const { return type == t; }
		bool IsNot(TokenType t) const { return type != t; }
		template <typename... Ts>
		bool IsOneOf(TokenType t1, Ts... ts) const
		{
			if constexpr (sizeof...(Ts) == 0) return Is(t1);
			else return Is(t1) || IsOneOf(ts...);
		}

		bool IsPPKeyword() const
		{
			return IsOneOf(TokenType::PP_define,  TokenType::PP_defined,  TokenType::PP_elif,
						   TokenType::PP_elifdef, TokenType::PP_elifndef, TokenType::PP_else,
						   TokenType::PP_endif,   TokenType::PP_if,       TokenType::PP_ifdef, 
						   TokenType::PP_ifndef,  TokenType::PP_include,  TokenType::PP_undef);
		}

		void SetFlag(TokenFlag flag)
		{
			flags |= flag;
		}
		bool HasFlag(TokenFlag flag) const
		{
			return (flags & flag) != 0;
		}
		void ClearFlag(TokenFlag flag)
		{
			flags &= ~flag;
		}

		void SetIdentifier(char const* p_data, size_t count)
		{
			data = std::string(p_data, count);
		}
		void SetIdentifier(char const* start, char const* end)
		{
			data = std::string(start, end - start);
		}
		std::string_view GetIdentifier() const
		{
			return data;
		}

		void SetLocation(SourceLocation const& _loc)
		{
			loc = _loc;
		}
		SourceLocation const& GetLocation() const { return loc; }

	private:
		TokenType type;
		TokenFlags flags;
		SourceLocation loc;
		std::string data;
	};
}