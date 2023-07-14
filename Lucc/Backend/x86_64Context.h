#pragma once
#include <array>
#include <string>
#include "x86_64.h"
#include "x86_64CodeGenerator.h"
#include "Core/Defines.h"

namespace lucc
{
	class x86_64Context
	{
	public:
		explicit x86_64Context(OutputBuffer& output_buffer);

		Register AllocateRegister();
		Register GetCallRegister(uint32 arg_index) const;
		void FreeRegister(Register reg);

		void Add(ResultRef lhs, ResultRef rhs, BitCount bitcount);
		void Sub(ResultRef lhs, ResultRef rhs, BitCount bitcount);
		void Imul(Register lhs, ResultRef rhs,  BitCount bitcount);
		void Imul(Register lhs, ResultRef rhs, int32 imm, BitCount bitcount);
		void Idiv(Register lhs, ResultRef rhs, BitCount bitcount);
		void Neg(ResultRef op, BitCount bitcount);
		void Inc(ResultRef op, BitCount bitcount);
		void Dec(ResultRef op, BitCount bitcount);

		void And(ResultRef lhs, ResultRef rhs, BitCount bitcount);
		void Or(ResultRef  lhs, ResultRef rhs, BitCount bitcount);
		void Xor(ResultRef lhs, ResultRef rhs, BitCount bitcount);
		void Not(ResultRef op, BitCount bitcount);

		void Push(ResultRef op);
		void Pop(ResultRef op);

		uint64 GenerateLabelId();
		void Label(char const* label);
		void Label(char const* label, uint64 label_id);
		void Cmp(ResultRef lhs, ResultRef rhs, BitCount bitcount);
		void Set(ResultRef op, ConditionCode cc);
		void Jmp(char const* label, ConditionCode cc = ConditionCode::None);
		void Jmp(char const* label, uint64 label_id, ConditionCode cc = ConditionCode::None);

		void Mov(ResultRef lhs, ResultRef rhs, BitCount bitcount);
		void MovOffset(Register lhs, char const* global);
		void Movabs(Register lhs, int64 value);
		void Movzx(Register lhs, ResultRef rhs, BitCount bitcount, bool rhs8bit = false);
		void Movsx(Register lhs, ResultRef rhs, BitCount bitcount, bool rhs8bit = false);
		void Movsxd(Register lhs, ResultRef rhs);
		void Lea(Register reg, ResultRef op);

		void DeclareVariable(VarDeclCG const& var_decl);
		void DeclareArray(ArrayDeclCG const& array_decl);
		void DeclareFunction(FunctionDeclCG const& func_decl);

		void Call(char const* func_name);
		void ReserveStack(uint32 stack);
		void JumpToReturn();
		void Return();

	private:
		OutputBuffer& output_buffer;
		std::array<bool, RegisterCount> register_mask;

		std::string current_function; 
		bool stack_reg_saved;
		uint32 stack_used;
	private:
		template<SegmentType segment, typename... Ts>
		void Emit(std::string_view fmt, Ts&&... args);

		static std::string ConvertOperand(ResultRef op, BitCount bitcount);
	};
}