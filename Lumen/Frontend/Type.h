#pragma once
#include "Core/Enums.h"

namespace lu
{
	enum class TypeKind : uint8
	{
		Void,
		Arithmetic,
		Enum,
		Pointer,
		Function,
		Array,
		Struct,
		Union,
	};

	class Type
	{
	public:
		bool IsComplete() const { return is_complete; }
		size_t GetSize() const { return size; }
		size_t GetAlign() const { return align; }
		void SetAlign(size_t _align) { align = _align; }

		TypeKind GetKind() const { return kind; }
		bool Is(TypeKind t) const { return kind == t; }
		bool IsNot(TypeKind t) const { return kind != t; }
		template <typename... Ts>
		bool IsOneOf(TypeKind t1, Ts... ts) const
		{
			if constexpr (sizeof...(Ts) == 0) return Is(t1);
			else return Is(t1) || IsOneOf(ts...);
		}

	private:
		TypeKind kind;
		size_t size;
		size_t align;
		bool is_complete;

	protected:
		constexpr Type(TypeKind kind, bool is_complete,
			size_t size = 0, size_t align = 0)
			: kind{ kind }, is_complete{ is_complete }, size{ size }, align{ align } {}
		void SetComplete() { is_complete = true; }
		void SetSize(std::size_t size) { size = size; }
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

		explicit QualifiedType(Qualifiers qualifiers = QualifierNone) : qualifiers{ qualifiers } {}
		QualifiedType(Type const* type, Qualifiers qualifiers = QualifierNone) : type{ type }, qualifiers{ qualifiers } {}
		bool IsConst() const { return qualifiers & QualifierConst; }
		bool IsVolatile() const { return qualifiers & QualifierVolatile; }
		void AddConst() { qualifiers |= QualifierConst; }
		void AddVolatile() { qualifiers |= QualifierVolatile; }

		void RmConst() { qualifiers &= ~QualifierConst; }
		void RmVolatile() { qualifiers &= ~QualifierVolatile; }
		void RemoveQualifiers() { qualifiers = QualifierNone; }

		bool HasRawType() const { return type != nullptr; }
		void SetRawType(Type const* _type) { type = _type; }
		void ResetRawType() { type = nullptr; }
		Type const* RawType() const { return type; }

		Type const* operator->() const {
			return type;
		}
		Type const& operator*() const { return *RawType(); }
		operator Type const& () const { return *RawType(); }

	private:
		Type const* type = nullptr;
		Qualifiers qualifiers;
	};

	class VoidType : public Type
	{
	public:
		constexpr VoidType() : Type{ TypeKind::Void, false } {}
	};

	class PointerType : public Type
	{
	public:
		PointerType(QualifiedType const& pointee_qtype)
			: Type{ TypeKind::Pointer, true, 8, 8 },
			pointee_qtype{ pointee_qtype } {}

		QualifiedType PointeeQType() const { return pointee_qtype; }

	private:
		QualifiedType pointee_qtype;
	};

	class ArrayType : public Type
	{
	public:
		explicit ArrayType(QualifiedType const& base_qtype)
			: Type{ TypeKind::Array, false, 0, base_qtype->GetAlign() },
			base_qtype{ base_qtype } {}

		ArrayType(QualifiedType const& base_qtype, size_t arr_size)
			: Type{ TypeKind::Array, true, base_qtype->GetSize() * arr_size, base_qtype->GetAlign() },
			base_qtype{ base_qtype }, arr_size{ arr_size } {}

		QualifiedType BaseQualifiedType() const { return base_qtype; }
		size_t ArrSize() const { return arr_size; }
		void SetArrSize(size_t _arr_size)
		{
			arr_size = _arr_size;
			SetSize(arr_size * base_qtype->GetSize());
			SetComplete();
		}

	private:
		QualifiedType base_qtype;
		size_t arr_size = 0;
	};

	enum ArithmeticFlag : uint32
	{
		Bool = 1 << 0,
		Char = 1 << 1,
		Int = 1 << 2,
		Float = 1 << 3,
		Double = 1 << 4,
		Short = 1 << 5,
		Long = 1 << 6,
		LongLong = 1 << 7,
		Signed = 1 << 8,
		Unsigned = 1 << 9
	};
	DEFINE_ENUM_BIT_OPERATORS(ArithmeticFlag);
	using ArithmeticFlags = uint32;

	class ArithmeticType : public Type
	{
	public:
		explicit ArithmeticType(ArithmeticFlags flags) : Type(TypeKind::Arithmetic, true), flags(flags) {}
		ArithmeticFlags GetFlags() const { return flags; }
	private:
		ArithmeticFlags flags;
	};

	//todo: add functiontype and structtype

	template<typename T>
	T& TypeCast(Type& t)
	{
		return static_cast<T&>(t);
	}
	template<typename T>
	T const& TypeCast(Type const& t)
	{
		return static_cast<T&>(t);
	}
}