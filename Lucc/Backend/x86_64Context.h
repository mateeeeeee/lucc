#pragma once
#include <array>
#include <string>
#include "x86_64.h"
#include "x86_64CodeGenerator.h"
#include "Core/Defines.h"

namespace lucc
{

	class x86_64CodeGenerator::Context
	{
	public:
		explicit Context(OutputBuffer& output_buffer);

		Register AllocateRegister();
		Register GetCallRegister(uint32 arg_index) const;
		void FreeRegister(Register reg);

		void Add(OperandRef lhs, OperandRef rhs, BitCount bitcount);
		void Sub(OperandRef lhs, OperandRef rhs, BitCount bitcount);
		void Imul(Register  lhs, OperandRef rhs,  BitCount bitcount);
		void Imul(Register  lhs, OperandRef rhs, int32 imm, BitCount bitcount);
		void Idiv(Register  lhs, OperandRef rhs, BitCount bitcount);
		void Neg(OperandRef op, BitCount bitcount);
		void Inc(OperandRef op, BitCount bitcount);
		void Dec(OperandRef op, BitCount bitcount);

		void And(OperandRef lhs, OperandRef rhs, BitCount bitcount);
		void Or(OperandRef  lhs, OperandRef rhs, BitCount bitcount);
		void Xor(OperandRef lhs, OperandRef rhs, BitCount bitcount);
		void Not(OperandRef op, BitCount bitcount);

		void Push(OperandRef op);
		void Pop(OperandRef op);

		uint64 GenerateLabelId();
		void Label(char const* label);
		void Label(char const* label, uint64 label_id);
		void Cmp(OperandRef lhs, OperandRef rhs, BitCount bitcount);
		void Set(OperandRef op, ConditionCode cc);
		void Jmp(char const* label, ConditionCode cc = ConditionCode::None);
		void Jmp(char const* label, uint64 label_id, ConditionCode cc = ConditionCode::None);

		void Mov(OperandRef lhs, OperandRef rhs, BitCount bitcount);
		void MovOffset(Register lhs, char const* global);
		void Movabs(Register lhs, int64 value);
		void Movzx(Register lhs, OperandRef rhs, BitCount bitcount, bool rhs8bit = false);
		void Movsx(Register lhs, OperandRef rhs, BitCount bitcount, bool rhs8bit = false);
		void Movsxd(Register lhs, OperandRef rhs);
		void Lea(Register reg, OperandRef op);

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

		static std::string ConvertOperand(OperandRef op, BitCount bitcount);
	};
}