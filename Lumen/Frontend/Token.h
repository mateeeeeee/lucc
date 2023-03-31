#pragma once
#include <any>
#include "TokenTypes.h"
#include "SourceLocation.h"
#include "Core/Enums.h"

namespace lumen
{

	enum TokenFlagBits : uint32
	{
		TokenFlag_None = 0x0,
		TokenFlag_LeadingSpace = 0x1,
		TokenFlag_BeginningOfLine = 0x2,
	};
	DEFINE_ENUM_BIT_OPERATORS(TokenFlagBits);
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
			data.reset();
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

		void SetFlag(TokenFlagBits flag)
		{
			flags |= flag;
		}
		bool GetFlag(TokenFlagBits flag) const
		{
			return (flags & flag) != 0;
		}
		void ClearFlag(TokenFlagBits flag)
		{
			flags &= ~flag;
		}

		template<typename T>
		void SetData(T const& _data)
		{
			data = _data;
		}
		template<typename T>
		T const& GetData() const
		{
			return std::any_cast<T const&>(data);
		}
		bool HasData() const
		{
			return data.has_value();
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
		std::any data;
	};
}