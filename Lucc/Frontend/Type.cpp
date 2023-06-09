#include "Type.h"
#include "Diagnostics.h"

namespace lucc
{

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

	QualifiedType IntPromote(QualifiedType const& type)
	{
		LU_ASSERT(IsIntegerType(type));
		auto const& arithmetic_type = TypeCast<ArithmeticType>(type);
		if (arithmetic_type.ConversionRank() < builtin_types::Int.ConversionRank()) return builtin_types::Int;
		return RemoveQualifiers(type);
	}

	QualifiedType TryIntPromote(QualifiedType const& type)
	{
		if (!IsIntegerType(type)) return ValueTransformation(type);
		return IntPromote(type);
	}

	QualifiedType UsualArithmeticConversion(QualifiedType const& lhs, QualifiedType const& rhs)
	{
		LU_ASSERT(IsArithmeticType(lhs) && IsArithmeticType(rhs));
		auto const& lhs_type = TypeCast<ArithmeticType>(lhs);
		auto const& rhs_type = TypeCast<ArithmeticType>(rhs);
		QualifiedType common_type = lhs_type.ConversionRank() < rhs_type.ConversionRank() ? rhs : lhs;
		common_type = TryIntPromote(common_type);
		return common_type;
	}

}

