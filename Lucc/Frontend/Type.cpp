#include "Type.h"
#include "Diagnostics.h"

namespace lucc
{

	bool ArithmeticType::IsCompatible(Type const& other) const
	{
		return IsArithmeticType(other) ? true : false;
	}

	bool FunctionType::IsCompatible(Type const& other) const
	{
		return true;
	}

	// C11 6.3.1.1p2: If an int can represent all values of the original type (as
	// restricted by the width, for a bit-field), the value is converted to an int;
	// otherwise, it is converted to an unsigned int. These are called the integer
	// promotions. All other types are unchanged by the integer promotions.
	QualifiedType IntegerPromotion(QualifiedType const& qtype)
	{
		LU_ASSERT(IsIntegerType(qtype));
		auto const& arithmetic_type = TypeCast<ArithmeticType>(qtype);
		if (arithmetic_type.ConversionRank() < builtin_types::Int.ConversionRank()) return builtin_types::Int;
		else return builtin_types::UnsignedInt;
	}
	QualifiedType TryIntegerPromotion(QualifiedType const& type)
	{
		if (!IsIntegerType(type)) return ValueTransformation(type);
		return IntegerPromotion(type);
	}

	/*Value transformations
	1. Lvalue conversion
	------------------------------------------------------------------------------------------------
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
	------------------------------------------------------------------------------------------------
	2. Array to pointer conversion
	------------------------------------------------------------------------------------------------
	Any lvalue expression of array type, when used in any context other than

	as the operand of the address-of operator
	as the operand of sizeof
	as the string literal used for array initialization
	undergoes a conversion to the non-lvalue pointer to its first element.

	If the array was declared register, the behavior is undefined.
	------------------------------------------------------------------------------------------------
	3. Function to pointer conversion
	------------------------------------------------------------------------------------------------
	Any function designator expression, when used in any context other than

	as the operand of the address-of operator
	as the operand of sizeof
	undergoes a conversion to the non-lvalue pointer to the function designated by the expression.
	------------------------------------------------------------------------------------------------
	*/
	QualifiedType ValueTransformation(QualifiedType const& qtype)
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
	QualifiedType AsIfByAssignment(QualifiedType const& src_type, QualifiedType const& dst_type)
	{
		QualifiedType expr_type = ValueTransformation(src_type);

		LU_ASSERT(!IsArrayType(dst_type) && !IsFunctionType(dst_type));
		QualifiedType ret_type = RemoveQualifiers(dst_type);
		if (expr_type->IsCompatible(dst_type)) return ret_type;

		if (IsIntegerType(expr_type) && IsPointerType(dst_type) && !IsBoolType(expr_type))
		{
			Report(diag::incompatible_integer_to_pointer_conversion);
		}
		else if (IsIntegerType(dst_type) && IsPointerType(expr_type) && !IsBoolType(dst_type))
		{
			Report(diag::incompatible_integer_to_pointer_conversion);
		}
		else if (IsPointerType(expr_type) && IsPointerType(dst_type))
		{
			QualifiedType expr_pte_qty = TypeCast<PointerType>(expr_type).PointeeType();
			QualifiedType dst_pte_qty = TypeCast<PointerType>(dst_type).PointeeType();
			if ((IsVoidType(expr_pte_qty) && IsObjectType(dst_pte_qty)) ||
				(IsVoidType(dst_pte_qty) && IsObjectType(expr_pte_qty)) ||
				expr_pte_qty->IsCompatible(dst_pte_qty))
			{
				if ((!dst_pte_qty.IsConst() && expr_pte_qty.IsConst()) ||
					(!dst_pte_qty.IsVolatile() && expr_pte_qty.IsVolatile()))
				{
					Report(diag::incompatible_pointer_types_conversion_discards_qualifiers);
				}
			}
			else if ((IsObjectType(expr_pte_qty) && IsObjectType(dst_pte_qty)) ||
					 (IsFunctionType(expr_pte_qty) && IsFunctionType(dst_pte_qty)))
			{
				Report(diag::incompatible_pointer_types_conversion);
			}
		}
		return ret_type;
	}

	// C11 6.5.6 Additive operators
	QualifiedType AdditiveOperatorType(QualifiedType const& lhs_type, QualifiedType const& rhs_type, bool subtract)
	{
		QualifiedType const& lhs_qtype = ValueTransformation(lhs_type);
		QualifiedType const& rhs_qtype = ValueTransformation(rhs_type);
		if (IsArithmeticType(lhs_qtype) && IsArithmeticType(rhs_qtype)) { return UsualArithmeticConversion(lhs_qtype, rhs_qtype); }
		else if (IsPointerType(lhs_qtype) || IsPointerType(rhs_qtype))
		{
			auto IsComplete = [](QualifiedType const& ptr_qtype)
			{
				if (!IsVoidPointerType(ptr_qtype) && !IsFunctionPointerType(ptr_qtype) && !TypeCast<PointerType>(ptr_qtype).PointeeType()->IsComplete())
				{
					Report(diag::arithmetic_on_incomplete_object_type);
					return false;
				}
				else
				{
					if (IsVoidPointerType(ptr_qtype))
					{
						Report(diag::arithmetic_on_void_pointer_type);
					}
					else if (IsFunctionPointerType(ptr_qtype))
					{
						Report(diag::arithmetic_on_function_pointer_type);
					}
					return true;
				}
				return true;
			};

			if (IsPointerType(lhs_qtype) && IsPointerType(rhs_qtype) && subtract)
			{
				QualifiedType lhs_pte_qty = TypeCast<PointerType>(lhs_qtype).PointeeType();
				QualifiedType rhs_pte_qty = TypeCast<PointerType>(rhs_qtype).PointeeType();
				if (!lhs_pte_qty->IsCompatible(rhs_pte_qty))
				{
					Report(diag::arithmetic_on_incompatible_pointers);
				}
				else if (IsComplete(lhs_qtype))
				{
					// C11 6.5.6p9: The size of the result is implementation-defined,
					// and its type (a signed integer type) is ptrdiff_t defined in
					// the <stddef.h> header.
					return builtin_types::LongLong;
				}
			}
			else if (!IsIntegerType(lhs_qtype) && !IsIntegerType(rhs_qtype))
			{
				Report(diag::additive_operator_invalid_operands); //ErrInExpr("invalid operands to additive operators");
			}
			else
			{
				QualifiedType ptr_qtype = IsIntegerType(lhs_qtype) ? rhs_qtype : lhs_qtype;
				if (IsComplete(ptr_qtype))  return ptr_qtype;
			}
		}
		else
		{
			Report(diag::additive_operator_invalid_operands);
		}
		return QualifiedType{};
	}
	// C11 6.5.5p2: Each of the operands shall have arithmetic type. The operands of the % operator shall have integer type.
	QualifiedType MultiplicativeOperatorType(QualifiedType const& lhs_type, QualifiedType const& rhs_type, bool modulo)
	{
		if (modulo && (!IsIntegerType(lhs_type) || !IsIntegerType(rhs_type))) Report(diag::modulo_operands_invalid);
		else if (!IsArithmeticType(lhs_type) || !IsArithmeticType(rhs_type)) Report(diag::multiplicative_operator_invalid_operands);
		return UsualArithmeticConversion(lhs_type, rhs_type);
	}

	// C11 6.5.7p2: Each of the operands shall have integer type.
	QualifiedType ShiftOperatorType(QualifiedType const& lhs_type, QualifiedType const& rhs_type)
	{
		if (!IsIntegerType(lhs_type) || !IsIntegerType(rhs_type)) Report(diag::shift_operator_invalid_operands);
		return IntegerPromotion(lhs_type);
	}
	// C11 6.5.13p2 & 6.5.14p2: Each of the operands shall have scalar type.
	QualifiedType LogicOperatorType(QualifiedType const& lhs_type, QualifiedType const& rhs_type)
	{
		QualifiedType lhs_qtype = ValueTransformation(lhs_type);
		QualifiedType rhs_qtype = ValueTransformation(rhs_type);
		if (!IsScalarType(lhs_qtype) || !IsScalarType(rhs_qtype)) Report(diag::logic_operator_invalid_operands);
		return builtin_types::Int;
	}
	// C11 6.5.10p2 & 6.5.11p2 & 6.5.12p2 : Each of the operands shall have integer type
	QualifiedType BitLogicOperatorType(QualifiedType const& lhs_type, QualifiedType const& rhs_type)
	{
		if (!IsIntegerType(lhs_type) || !IsIntegerType(rhs_type)) Report(diag::bit_logic_operator_invalid_operands);
		return UsualArithmeticConversion(lhs_type, rhs_type);
	}
	// C11 6.5.9 Equality operators
	QualifiedType EqualityOperatorType(QualifiedType const& lhs_type, QualifiedType const& rhs_type)
	{
		QualifiedType lhs_qtype = ValueTransformation(lhs_type);
		QualifiedType rhs_qtype = ValueTransformation(rhs_type);
		if (IsPointerType(lhs_qtype) && IsPointerType(rhs_qtype)) 
		{
			QualifiedType lhs_pte_qty = TypeCast<PointerType>(lhs_qtype).PointeeType();
			QualifiedType rhs_pte_qty = TypeCast<PointerType>(rhs_qtype).PointeeType();
			if (!lhs_pte_qty->IsCompatible(rhs_pte_qty)) 
			{
				if (!IsVoidType(lhs_pte_qty) && !IsVoidType(rhs_pte_qty)) Report(diag::comparison_between_incompatible_pointers);
				else if (IsFunctionType(lhs_pte_qty) || IsFunctionType(rhs_pte_qty))  Report(diag::pointer_comparison_between_func_and_void_pointer);
			}
		}
		else if(!IsArithmeticType(lhs_qtype) || !IsArithmeticType(rhs_qtype)) Report(diag::equality_operator_invalid_operands);
		return builtin_types::Int;
	}
	// C11 6.5.8 Relational operators
	QualifiedType RelationOperatorType(QualifiedType const& lhs_type, QualifiedType const& rhs_type)
	{
		return EqualityOperatorType(lhs_type, rhs_type);
	}

	// C11 6.3.1.8 Usual arithmetic conversions
	QualifiedType UsualArithmeticConversion(QualifiedType const& lhs, QualifiedType const& rhs)
	{
		LU_ASSERT(IsArithmeticType(lhs) && IsArithmeticType(rhs));
		auto const& lhs_type = TypeCast<ArithmeticType>(lhs);
		auto const& rhs_type = TypeCast<ArithmeticType>(rhs);
		QualifiedType common_type = lhs_type.ConversionRank() < rhs_type.ConversionRank() ? rhs : lhs;
		common_type = TryIntegerPromotion(common_type);
		return common_type;
	}

	// C11 6.5.2.4p1 & 6.5.3.1p1: The operand of the postfix/prefix increment or
	// decrement operator shall have atomic, qualified, or unqualified real or
	// pointer type, and shall be a modifiable lvalue.
	QualifiedType IncDecOperatorType(QualifiedType const& op_type)
	{
		if (op_type.IsConst())				Report(diag::inc_dec_operand_cannot_be_const);
		else if (!IsScalarType(op_type))	Report(diag::inc_dec_expected_scalar_operand);
		return RemoveQualifiers(op_type);
	}

	QualifiedType PlusMinusOperatorType(QualifiedType const& op_type)
	{
		if (!IsArithmeticType(op_type)) Report(diag::plus_minus_expected_arithmetic_operand);
		return TryIntegerPromotion(op_type);
	}

	QualifiedType BitNotOperatorType(QualifiedType const& op_type)
	{
		if (!IsIntegerType(op_type)) Report(diag::bit_not_expected_integer_operand);
		return TryIntegerPromotion(op_type);
	}

	QualifiedType LogicalNotOperatorType(QualifiedType const& op_type)
	{
		QualifiedType operand_qty = ValueTransformation(op_type);
		if (!IsScalarType(operand_qty)) Report(diag::logical_not_expected_scalar_operand);
		return builtin_types::Int;
	}

	QualifiedType DereferenceOperatorType(QualifiedType const& op_type)
	{
		QualifiedType operand_qty = ValueTransformation(op_type);
		if (!IsPointerType(operand_qty)) Report(diag::dereference_expected_pointer_operand);
		return TypeCast<PointerType>(operand_qty).PointeeType();
	}
	// C11 6.5.3.2p1: The operand of the unary & operator shall be either a function
	// designator, the result of a [] or unary * operator, or an lvalue that
	// designates an object that is not a bit-field and is not declared with the
	// register storage-class specifier.
	QualifiedType AddressOfOperatorType(QualifiedType const& op_type)
	{
		return PointerType(op_type);
	}

}

