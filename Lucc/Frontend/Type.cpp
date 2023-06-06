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

