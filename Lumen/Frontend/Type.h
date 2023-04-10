#pragma once
#include "Core/Enums.h"
#include <unordered_map>
#include <span>

namespace lucc
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
		Union
	};

	class Type
	{
	public:
		constexpr bool IsComplete() const { return is_complete; }
		constexpr size_t GetSize() const { return size; }
		constexpr size_t GetAlign() const { return align; }
		constexpr void SetAlign(size_t _align) { align = _align; }

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
		mutable bool is_complete;

	protected:
		constexpr Type(TypeKind kind, bool is_complete,
			size_t size = 0, size_t align = 0)
			: kind( kind ), is_complete( is_complete ), size( size ), align( align ) {}
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

		explicit QualifiedType(Qualifiers qualifiers = QualifierNone) : qualifiers( qualifiers ) {}
		QualifiedType(Type const& _type, Qualifiers qualifiers = QualifierNone)  : type(_type), qualifiers(qualifiers) {}
		Qualifiers GetQualifiers() const { return qualifiers; }
		bool IsConst() const { return qualifiers & QualifierConst; }
		bool IsVolatile() const { return qualifiers & QualifierVolatile; }
		void AddConst() { qualifiers |= QualifierConst; }
		void AddVolatile() { qualifiers |= QualifierVolatile; }

		void RmConst() { qualifiers &= ~QualifierConst; }
		void RmVolatile() { qualifiers &= ~QualifierVolatile; }
		void RemoveQualifiers() { qualifiers = QualifierNone; }

		bool HasRawType() const { return type.has_value(); }
		void SetRawType(Type const& _type) { type = _type; }
		void ResetRawType() { type = std::nullopt; }
		Type const& RawType() const { return *type; }

		std::optional<Type> const& operator->() const {
			return type;
		}
		Type const& operator*() const { return RawType(); }
		operator Type const& () const { return RawType(); }

	private:
		std::optional<Type> type = std::nullopt;
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

		QualifiedType PointeeQualfiedType() const { return pointee_qtype; }

	private:
		QualifiedType pointee_qtype;
	};

	class ArrayType : public Type
	{
	public:
		explicit ArrayType(QualifiedType const& base_qtype)
			: Type( TypeKind::Array, false, 0, (*base_qtype).GetAlign() ),
			base_qtype( base_qtype ) {}

		ArrayType(QualifiedType const& base_qtype, size_t arr_size)
			: Type(TypeKind::Array, true, (*base_qtype).GetSize()* arr_size, (*base_qtype).GetAlign()),
			base_qtype(base_qtype ), arr_size(arr_size ) {}

		QualifiedType BaseQualifiedType() const { return base_qtype; }
		size_t ArrSize() const { return arr_size; }
		void SetArrSize(size_t _arr_size)
		{
			arr_size = _arr_size;
			SetSize(arr_size * (*base_qtype).GetSize());
			SetComplete();
		}

	private:
		QualifiedType base_qtype;
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
			: Type(TypeKind::Arithmetic, true), flags(flags) 
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

	enum class FunctionSpecifier : bool
	{
		None,
		Inline
	};
	struct FunctionParameter
	{
		std::string name = "";
		QualifiedType qtype{};
	};
	class FuncType : public Type 
	{
	public:
		
		FuncType(QualifiedType const& return_qtype, std::span<FunctionParameter> param_types = {}, bool is_variadic = false)
			: Type( TypeKind::Function, false ),
			return_qtype( return_qtype ), param_types(param_types.begin(), param_types.end()), is_variadic(is_variadic), has_prototype( false ){}
		
		bool IsInline() const { return specifier == FunctionSpecifier::Inline; }
		void SetInline() { specifier = FunctionSpecifier::Inline; }
		
		QualifiedType GetReturnType() const { return return_qtype; }
		std::span<FunctionParameter const> GetParamTypes() const { return param_types; }

		void UpdateParamTypes(std::span<FunctionParameter> _param_types) { param_types.assign(_param_types.begin(),_param_types.end()); }
		
		void EncounteredDefinition() const { SetComplete(); }
		bool HasDefinition() const { return IsComplete(); }
		void EncounterPrototype() const { has_prototype = true; }
		bool HasPrototype() const { return has_prototype; }
		
	private:
		QualifiedType return_qtype;
		std::vector<FunctionParameter> param_types;
		bool is_variadic = false;
		mutable bool has_prototype = false;
		FunctionSpecifier specifier = FunctionSpecifier::None;
	};
	
	// Struct member
	struct RecordField 
	{
		std::string name = "";
		QualifiedType qtype{};
		size_t idx = 0;
		size_t align = 0;
		size_t offset = 0;
		// Bitfield
		bool is_bitfield = false;
		size_t bit_offset = 0;
		size_t bit_width = 0;
	};
	class RecordType : public Type 
	{
	public:
		explicit RecordType(std::string_view tag_name, bool is_union = false)
			: Type( is_union ? TypeKind::Union : TypeKind::Struct, false ), tag_name( tag_name ) {}

		RecordType(std::span<RecordField> _fields, std::string_view tag_name = "", bool is_union = false)
			: RecordType(tag_name, is_union) 
		{
			fields.assign(_fields.begin(), _fields.end());
		}
		
		std::string_view GetTagName() const { return tag_name; }
		std::span<RecordField const> GetFields() const { return fields; }
		void EncounterDefinition(std::span<RecordField> _fields)
		{
			SetComplete();
			fields.assign(_fields.begin(), _fields.end());
		}
		
		bool HasField(std::string_view name) const
		{
			return members_map.find(name) != members_map.cend();
		}
		RecordField& GetField(std::string_view name)
		{
			return members_map[name];
		}
		bool IsModifiableLValue() const { return is_modifiable_lvalue; }
	private:

		std::string tag_name;
		std::vector<RecordField> fields{};
		std::unordered_map<std::string_view, RecordField> members_map{};
		bool is_modifiable_lvalue = false;
	};

	template<typename T>
	T& TypeCast(Type& t)
	{
		return static_cast<T&>(t);
	}
	template<typename T>
	T const& TypeCast(Type const& t)
	{
		return static_cast<T const&>(t);
	}

}