#pragma once
#include <array>
#include <string>
#include "x86_64.h"
#include "x86_64CodeGenerator.h"


namespace lucc
{
	class x86_64Context
	{
	public:
		explicit x86_64Context(OutputBuffer& output_buffer);

		
		Register AllocateRegister();
		Register GetCallRegister(uint32 arg_index);
		Register GetReturnRegister();
		void FreeRegister(Register reg);
		void FreeRegister(ResultRef reg);
		uint32 SaveVolatileRegisters();
		void RestoreVolatileRegisters();

		void Add(ResultRef lhs, ResultRef rhs, BitCount bitcount);
		void Sub(ResultRef lhs, ResultRef rhs, BitCount bitcount);
		void Imul(Register lhs, ResultRef rhs,  BitCount bitcount);
		void Imul(Register lhs, ResultRef rhs, int32 imm, BitCount bitcount);
		void Idiv(Register lhs, ResultRef rhs, BitCount bitcount);
		void Neg(ResultRef op, BitCount bitcount);
		void Inc(ResultRef op, BitCount bitcount);
		void Dec(ResultRef op, BitCount bitcount);

		//shifts
		void Shl(ResultRef lhs, ResultRef rhs, BitCount bitcount);
		void Shr(ResultRef lhs, ResultRef rhs, BitCount bitcount);
		void Sar(ResultRef lhs, ResultRef rhs, BitCount bitcount);

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
		std::string DeclareString(char const* str);


		void Call(char const* func_name);
		void Call(Register reg);
		void SaveFrameRegister();
		void AllocateStack(uint32 size);
		void FreeStack(uint32 size);
		void JumpToReturn();
		void Return();

	private:
		OutputBuffer& output_buffer;
		std::array<bool, RegisterCount> registers_available;
		std::array<bool, RegisterCount> registers_pushed;

		std::vector<Register> spilled_scratched_registers;
		size_t last_spilled_reg_index = 0;

		std::string current_function;
		bool frame_reg_saved = false;
		uint32 stack_allocated = 0;

	private:
		template<SegmentType segment, typename... Ts>
		void Emit(std::string_view fmt, Ts&&... args);

		static std::string ConvertOperand(ResultRef op, BitCount bitcount);
	};
}