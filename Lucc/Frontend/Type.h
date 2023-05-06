#pragma once
#pragma once
#include "Core/Enums.h"
#include <memory>
#include <span>

namespace lucc
{
	enum class PrimitiveTypeKind : uint8
	{
		Void,
		Arithmetic,
		Enum,
		Pointer,
		Function,
		Array,
		Struct,
		Union
	};

	class Type
	{
	public:
		constexpr bool IsComplete() const { return is_complete; }
		constexpr size_t GetSize() const { return size; }
		constexpr size_t GetAlign() const { return align; }
		constexpr void SetAlign(size_t _align) { align = _align; }

		PrimitiveTypeKind GetKind() const { return kind; }
		bool Is(PrimitiveTypeKind t) const { return kind == t; }
		bool IsNot(PrimitiveTypeKind t) const { return kind != t; }
		template <typename... Ts>
		bool IsOneOf(PrimitiveTypeKind t1, Ts... ts) const
		{
			if constexpr (sizeof...(Ts) == 0) return Is(t1);
			else return Is(t1) || IsOneOf(ts...);
		}

	private:
		PrimitiveTypeKind kind;
		size_t size;
		size_t align;
		mutable bool is_complete;

	protected:
		constexpr Type(PrimitiveTypeKind kind, bool is_complete,
			size_t size = 0, size_t align = 0)
			: kind(kind), is_complete(is_complete), size(size), align(align) {}
		constexpr void SetComplete() const { is_complete = true; }
		constexpr void SetSize(size_t _size) { size = _size; }
	};

	enum QualifierFlag : uint8
	{
		QualifierNone = 0x0,
		QualifierConst = 0x1,
		QualifierVolatile = 0x2
	};
	DEFINE_ENUM_BIT_OPERATORS(QualifierFlag);
	using Qualifiers = uint8;

	class QualifiedType
	{
	public:
		friend class Type;

		explicit QualifiedType(Qualifiers qualifiers = QualifierNone) : qualifiers(qualifiers) {}

		template<std::derived_from<Type> DType>
		QualifiedType(DType const& _type, Qualifiers qualifiers = QualifierNone) : qualifiers(qualifiers)
		{
			type = std::make_shared<DType>(_type);
		}

		Qualifiers GetQualifiers() const { return qualifiers; }
		bool IsConst() const { return qualifiers & QualifierConst; }
		bool IsVolatile() const { return qualifiers & QualifierVolatile; }
		void AddConst() { qualifiers |= QualifierConst; }
		void AddVolatile() { qualifiers |= QualifierVolatile; }

		void RmConst() { qualifiers &= ~QualifierConst; }
		void RmVolatile() { qualifiers &= ~QualifierVolatile; }
		void RemoveQualifiers() { qualifiers = QualifierNone; }

		bool HasRawType() const { return type != nullptr; }

		template<std::derived_from<Type> DType>
		void SetRawType(DType const& _type)
		{
			type = std::make_shared<DType>(_type);
		}
		void ResetRawType() { type = nullptr; }
		Type const& RawType() const { return *type; }

		Type const* operator->() const {
			return type.get();
		}
		Type const& operator*() const { return RawType(); }
		operator Type const& () const { return RawType(); }

	private:
		std::shared_ptr<Type> type = nullptr;
		Qualifiers qualifiers;
	};
}