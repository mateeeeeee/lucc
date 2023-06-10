#pragma once
#include <memory>
#include <span>
#include "SourceLocation.h"
#include "Core/Enums.h"
#include "Core/Defines.h"

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

		virtual bool IsCompatible(Type const& other) const { return true; }

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

	template<typename T> requires std::derived_from<T, Type>
	T& TypeCast(Type& t)
	{
		return static_cast<T&>(t);
	}
	template<typename T> requires std::derived_from<T, Type>
	T const& TypeCast(Type const& t)
	{
		return static_cast<T const&>(t);
	}

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

		void RemoveConst() { qualifiers &= ~QualifierConst; }
		void RemoveVolatile() { qualifiers &= ~QualifierVolatile; }
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

	//C11 6.2.5p19: The void type comprises an empty set of values;
	//it is an incomplete object type that cannot be completed.
	class VoidType : public Type
	{
	public:
		constexpr VoidType() : Type{ PrimitiveTypeKind::Void, false } {}
		virtual bool IsCompatible(Type const& other) const override
		{
			return other.Is(PrimitiveTypeKind::Void);
		}
	};

	class PointerType : public Type
	{
	public:
		PointerType(QualifiedType const& pointee_qtype)
			: Type{ PrimitiveTypeKind::Pointer, true, 8, 8 },
			pointee_qtype{ pointee_qtype } {}

		QualifiedType PointeeType() const { return pointee_qtype; }
		virtual bool IsCompatible(Type const& other) const override
		{
			if (other.IsNot(PrimitiveTypeKind::Pointer)) return false;
			auto const& other_ptr = TypeCast<PointerType>(other);
			return pointee_qtype->IsCompatible(other_ptr.pointee_qtype);
		}

	private:
		QualifiedType pointee_qtype;
	};

	class ArrayType : public Type
	{
	public:
		explicit ArrayType(QualifiedType const& base_qtype)
			: Type(PrimitiveTypeKind::Array, false, 0, base_qtype->GetAlign()),
			elem_type(base_qtype) {}

		ArrayType(QualifiedType const& base_qtype, size_t arr_size)
			: Type(PrimitiveTypeKind::Array, true, base_qtype->GetSize() * arr_size, base_qtype->GetAlign()),
			elem_type(base_qtype), arr_size(arr_size) {}

		QualifiedType GetElementType() const { return elem_type; }
		size_t GetArraySize() const { return arr_size; }
		void SetArraySize(size_t _arr_size)
		{
			arr_size = _arr_size;
			SetSize(arr_size * (*elem_type).GetSize());
			SetComplete();
		}

		virtual bool IsCompatible(Type const& other) const override
		{
			if (other.IsNot(PrimitiveTypeKind::Array)) return false;
			auto const& other_arr = TypeCast<ArrayType>(other);
			return (elem_type->IsCompatible(other_arr.elem_type) &&
				(IsComplete() != elem_type->IsComplete() || arr_size == other_arr.arr_size));
		}

	private:
		QualifiedType elem_type;
		size_t arr_size = 0;
	};

	class ArithmeticType : public Type
	{
		static constexpr size_t CHAR_SIZE = 1;
		static constexpr size_t SHORT_SIZE = 2;
		static constexpr size_t INT_SIZE = 4;
		static constexpr size_t LONG_LONG_SIZE = 8;
		static constexpr size_t LONG_DOUBLE_SIZE = 16;
	public:
		enum ArithmeticFlag : uint32
		{
			Bool = 1 << 0,
			Char = 1 << 1,
			Short = 1 << 2,
			Int = 1 << 3,
			Long = 1 << 4,
			LongLong = 1 << 5,
			Float = 1 << 6,
			Double = 1 << 7
		};
		using ArithmeticFlags = uint32;

	public:
		constexpr explicit ArithmeticType(ArithmeticFlags flags, bool is_unsigned = false)
			: Type(PrimitiveTypeKind::Arithmetic, true), flags(flags)
		{
			if ((flags & Short) || (flags & Long) || (flags & LongLong)) flags &= ~Int;
			switch (flags)
			{
			case Bool:
			case Char:
				SetSize(CHAR_SIZE); break;
			case Short:
				SetSize(SHORT_SIZE); break;
			case Int:
			case Float:
			case Long:
				SetSize(INT_SIZE); break;
			case Double:
			case LongLong:
				SetSize(LONG_LONG_SIZE); break;
			case Double | Long:
				SetSize(LONG_DOUBLE_SIZE); break;
			}
			SetAlign(GetSize());
		}
		ArithmeticFlags GetFlags() const { return flags; }
		bool IsUnsigned() const { return is_unsigned; }
		uint32 ConversionRank() const
		{
			int rank = 0;
			switch (flags)
			{
			case Bool: rank = 1; break;
			case Char: rank = 2; break;
			case Short: rank = 4; break;
			case Int: rank = 6; break;
			case Long: rank = 8; break;
			case LongLong: rank = 10; break;
			case Float: rank = 12; break;
			case Double: rank = 13; break;
			case Double | Long: rank = 14; break;
			}
			if (is_unsigned) ++rank;
			return rank;
		}

	private:
		ArithmeticFlags flags;
		bool is_unsigned = false;

	private:
	};
	using EnumType = ArithmeticType;

	struct FunctionParameter
	{
		std::string name = "";
		QualifiedType qtype;
	};
	enum class FunctionSpecifier : bool
	{
		None,
		Inline
	};
	class FunctionType : public Type
	{
	public:

		FunctionType(QualifiedType const& return_qtype, std::span<FunctionParameter> param_types = {}, bool is_variadic = false)
			: Type(PrimitiveTypeKind::Function, false),
			return_qtype(return_qtype), param_types(param_types.begin(), param_types.end()), is_variadic(is_variadic), has_prototype(false) {}

		bool IsInline() const { return specifier == FunctionSpecifier::Inline; }
		void SetInline() { specifier = FunctionSpecifier::Inline; }

		QualifiedType GetReturnType() const { return return_qtype; }
		std::span<FunctionParameter const> GetParamTypes() const { return param_types; }

		void UpdateParamTypes(std::span<FunctionParameter> _param_types) { param_types.assign(_param_types.begin(), _param_types.end()); }

		void EncounteredDefinition() const { SetComplete(); }
		bool HasDefinition() const { return IsComplete(); }
		void EncounterPrototype() const { has_prototype = true; }
		bool HasPrototype() const { return has_prototype; }

		virtual bool IsCompatible(Type const& other) const override
		{
			return true;
		}

	private:
		QualifiedType return_qtype;
		std::vector<FunctionParameter> param_types;
		bool is_variadic = false;
		mutable bool has_prototype = false;
		FunctionSpecifier specifier = FunctionSpecifier::None;
	};

	template<PrimitiveTypeKind K>
	inline bool IsType(Type const& type)
	{
		return type.Is(K);
	}

	inline bool (*IsVoidType)(Type const& type)			= IsType<PrimitiveTypeKind::Void>;
	inline bool (*IsArrayType)(Type const& type)		= IsType<PrimitiveTypeKind::Array>;
	inline bool (*IsPointerType)(Type const& type)		= IsType<PrimitiveTypeKind::Pointer>;
	inline bool (*IsArithmeticType)(Type const& type)	= IsType<PrimitiveTypeKind::Arithmetic>;
	inline bool (*IsFunctionType)(Type const& type)		= IsType<PrimitiveTypeKind::Function>;
	inline bool (*IsStructType)(Type const& type)		= IsType<PrimitiveTypeKind::Struct>;
	inline bool (*IsUnionType)(Type const& type)		= IsType<PrimitiveTypeKind::Union>;

	inline bool IsPointerLikeType(Type const& type)
	{
		return  IsPointerType(type) || IsArrayType(type);
	}
	inline bool IsScalarType(Type const& type)
	{
		return type.IsOneOf(PrimitiveTypeKind::Arithmetic, PrimitiveTypeKind::Pointer);
	}
	inline bool IsBoolType(ArithmeticType const& arithmetic_type)
	{
		return arithmetic_type.GetFlags() & ArithmeticType::Bool;
	}
	inline bool IsBoolType(Type const& type)
	{
		return type.Is(PrimitiveTypeKind::Arithmetic) ? IsBoolType(TypeCast<ArithmeticType>(type)) : false;
	}
	inline bool IsFloatingType(ArithmeticType const& arithmetic_type)
	{
		return	(arithmetic_type.GetFlags() & ArithmeticType::Float) ||
			(arithmetic_type.GetFlags() & ArithmeticType::Double);
	}
	inline bool IsFloatingType(Type const& type)
	{
		return type.Is(PrimitiveTypeKind::Arithmetic) ? IsFloatingType(TypeCast<ArithmeticType>(type)) : false;
	}
	inline bool IsIntegerType(ArithmeticType const& arithmetic_type)
	{
		return !IsFloatingType(arithmetic_type);
	}
	inline bool IsIntegerType(Type const& type) {
		return type.Is(PrimitiveTypeKind::Arithmetic) && !IsFloatingType(type);
	}
	inline bool IsSignedType(ArithmeticType const& arithmetic_type)
	{
		return !arithmetic_type.IsUnsigned();
	}
	inline bool IsSignedType(Type const& type)
	{
		return type.Is(PrimitiveTypeKind::Arithmetic) ? IsSignedType(TypeCast<ArithmeticType>(type)) : false;
	}
	inline bool IsUnsignedType(ArithmeticType const& arithmetic_type)
	{
		return !arithmetic_type.IsUnsigned();
	}
	inline bool IsUnsignedType(Type const& type)
	{
		return !IsSignedType(type);
	}
	inline bool IsObjectType(Type const& type)
	{
		return type.Is(PrimitiveTypeKind::Function);
	}
	inline bool IsFunctionPointerType(Type const& type)
	{
		return type.Is(PrimitiveTypeKind::Pointer) && TypeCast<PointerType>(type).PointeeType()->Is(PrimitiveTypeKind::Function);;
	}
	inline bool IsObjPtrType(Type const& type)
	{
		return !IsFunctionPointerType(type);
	}
	inline bool IsVoidPointerType(Type const& type)
	{
		return type.Is(PrimitiveTypeKind::Pointer) && TypeCast<PointerType>(type).PointeeType()->Is(PrimitiveTypeKind::Void);
	}
	inline bool IsCharArrayType(Type const& type)
	{
		if (type.IsNot(PrimitiveTypeKind::Array)) return false;
		auto elem_type = TypeCast<ArrayType>(type).GetElementType();
		if (elem_type->IsNot(PrimitiveTypeKind::Arithmetic)) return false;
		ArithmeticType const& elem_arithmetic_type = TypeCast<ArithmeticType>(elem_type);
		auto elem_arithmetic_flags = elem_arithmetic_type.GetFlags();
		return (elem_arithmetic_flags & ArithmeticType::Char) ||
			(elem_arithmetic_flags == ArithmeticType::Short && elem_arithmetic_type.IsUnsigned()) ||
			(elem_arithmetic_flags == ArithmeticType::Int && elem_arithmetic_type.IsUnsigned());
	}

	namespace builtin_types
	{
		static constexpr Type Void = VoidType();
		static constexpr ArithmeticType Bool = ArithmeticType(ArithmeticType::Bool);
		static constexpr ArithmeticType Char = ArithmeticType(ArithmeticType::Char);
		static constexpr ArithmeticType UnsignedChar = ArithmeticType(ArithmeticType::Char, true);
		static constexpr ArithmeticType Short = ArithmeticType(ArithmeticType::Short);
		static constexpr ArithmeticType UnsignedShort = ArithmeticType(ArithmeticType::Short, true);
		static constexpr ArithmeticType Int = ArithmeticType(ArithmeticType::Int);
		static constexpr ArithmeticType UnsignedInt = ArithmeticType(ArithmeticType::Int, true);
		static constexpr ArithmeticType Long = ArithmeticType(ArithmeticType::Long);
		static constexpr ArithmeticType UnsignedLong = ArithmeticType(ArithmeticType::Long, true);
		static constexpr ArithmeticType LongLong = ArithmeticType(ArithmeticType::LongLong);
		static constexpr ArithmeticType UnsignedLongLong = ArithmeticType(ArithmeticType::LongLong, true);
		static constexpr ArithmeticType Float = ArithmeticType(ArithmeticType::Float);
		static constexpr ArithmeticType Double = ArithmeticType(ArithmeticType::Double);
		static constexpr ArithmeticType LongDouble = ArithmeticType(ArithmeticType::Double | ArithmeticType::Long);
	}

	inline QualifiedType RemoveQualifiers(QualifiedType const& qtype)
	{
		QualifiedType unqualified_type(qtype);
		unqualified_type.RemoveQualifiers();
		return unqualified_type;
	}
	// C11 6.3.1.1p2: If an int can represent all values of the original type (as
	// restricted by the width, for a bit-field), the value is converted to an int;
	// otherwise, it is converted to an unsigned int. These are called the integer
	// promotions. All other types are unchanged by the integer promotions.
	inline QualifiedType IntegerPromotion(QualifiedType const& qtype)
	{
		LU_ASSERT(IsIntegerType(qtype));
		auto const& arithmetic_type = TypeCast<ArithmeticType>(qtype);
		if (arithmetic_type.ConversionRank() < builtin_types::Int.ConversionRank()) return builtin_types::Int;
		else return builtin_types::UnsignedInt;
	}

	//Value transformations
	//1. Lvalue conversion
	/*Lvalue conversion
	Any lvalue expression of any non - array type, when used in any context other than

	as the operand of the address - of operator (if allowed)
	as the operand of the pre / post increment and decrement operators.
	as the left - hand operand of the member access(dot) operator.
	as the left - hand operand of the assignment and compound assignment operators.
	as the operand of sizeof
	undergoes lvalue conversion : the type remains the same, but loses const / volatile / restrict - qualifiers and atomic properties, if any.The value remains the same, but loses its lvalue properties(the address may no longer be taken).

	If the lvalue has incomplete type, the behavior is undefined.

	If the lvalue designates an object of automatic storage duration whose address was never takenand if that object was uninitialized(not declared with an initializer and no assignment to it has been performed prior to use), the behavior is undefined.

	This conversion models the memory load of the value of the object from its location.
	*/
	//2. Array to pointer conversion
	/* Array to pointer conversion
	Array to pointer conversion
	Any lvalue expression of array type, when used in any context other than

	as the operand of the address-of operator
	as the operand of sizeof
	as the string literal used for array initialization
	undergoes a conversion to the non-lvalue pointer to its first element.

	If the array was declared register, the behavior is undefined.
	*/
	//3. Function to pointer conversion
	/* Function to pointer conversion
	Any function designator expression, when used in any context other than

	as the operand of the address-of operator
	as the operand of sizeof
	undergoes a conversion to the non-lvalue pointer to the function designated by the expression.
	*/
	inline QualifiedType ValueTransformation(QualifiedType const& qtype)
	{
		if (qtype->Is(PrimitiveTypeKind::Array))
		{
			auto const& arr_type = TypeCast<ArrayType>(qtype);
			return PointerType(arr_type.GetElementType());
		}
		else if (qtype->Is(PrimitiveTypeKind::Function))
		{
			return PointerType(qtype);
		}
		else return RemoveQualifiers(qtype);
	}
	/*6.5.16.1 Simple assignment
	Constraints
	1 One of the following shall hold :
	— the left operand has qualified or unqualified arithmetic type and the right has
		arithmetic type;
	— the left operand has a qualified or unqualified version of a structure or union type
		compatible with the type of the right;
	— both operands are pointers to qualified or unqualified versions of compatible types,
		and the type pointed to by the left has all the qualifiers of the type pointed to by the
		right;
	— one operand is a pointer to an object or incomplete type and the other is a pointer to a
		qualified or unqualified version of void, and the type pointed to by the left has all
		the qualifiers of the type pointed to by the right;
	— the left operand is a pointerand the right is a null pointer constant; or
		— the left operand has type _Booland the right is a pointer.
		Semantics
	2 In simple assignment(=), the value of the right operand is converted to the type of the
	  assignment expression and replaces the value stored in the object designated by the left
	  operand.
	3 If the value being stored in an object is read from another object that overlaps in any way
	  the storage of the first object, then the overlap shall be exact and the two objects shall
	  have qualified or unqualified versions of a compatible type; otherwise, the behavior is
	  undefined. */
	QualifiedType AsIfByAssignment(QualifiedType const& src_type, QualifiedType const& dst_type);
	// C11 6.5.6 Additive operators
	QualifiedType AdditiveOperatorType(QualifiedType const& lhs_type, QualifiedType const& rhs_type, bool subtract);


	// C11 6.3.1.1p2: If an int can represent all values of the original type (as
	// restricted by the width, for a bit-field), the value is converted to an int;
	// otherwise, it is converted to an unsigned int. These are called the integer
	// promotions. All other types are unchanged by the integer promotions.
	QualifiedType IntPromote(QualifiedType const& type);
	QualifiedType TryIntPromote(QualifiedType const& type);

	// C11 6.3.1.8 Usual arithmetic conversions
	QualifiedType UsualArithmeticConversion(QualifiedType const& lhs, QualifiedType const& rhs);
}