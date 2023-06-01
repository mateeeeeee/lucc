#pragma once
#include <array>
#include <string>
#include "Core/Defines.h"
#include "x86_64CodeGenerator.h"

namespace lucc
{

	class x86_64CodeGenerator::Context : public ICodegenContext
	{
		static constexpr size_t REG_COUNT = 9;
		static constexpr size_t FUNC_ARGS_COUNT_IN_REGISTERS = 4;
		static constexpr size_t RETURN_REGISTER_INDEX = 8;

		static constexpr char const* qword_registers[REG_COUNT] = { "r10" , "r11" ,"r8" , "r9" , "rcx", "rdx", "rdi", "rsi", "rax" };
		static constexpr char const* dword_registers[REG_COUNT] = { "r10d", "r11d","r8d", "r9d", "ecx", "edx", "edi", "esi", "eax" };
		static constexpr char const* word_registers[REG_COUNT] = { "r10w", "r11w","r8w", "r9w", "cx" , "dx" , "di" , "si" , "ax" };
		static constexpr char const* byte_registers[REG_COUNT] = { "r10b", "r11b","r8b", "r9b", "cl" , "dl" , "dil", "sil", "al " };

		enum SegmentType : uint16
		{
			None,
			Data,
			Text
		};

	public:
		explicit Context(OutputBuffer& output_buffer);

		virtual register_t AllocateRegister() override;
		virtual register_t AllocateRegisterForReturn() override;
		virtual register_t AllocateRegisterForFunctionArg(size_t arg_index) override;
		virtual void FreeRegister(register_t reg) override;
		virtual void FreeAllRegisters() override;

		virtual void Move(char const* sym_name, register_t reg) override;
		virtual void Move(register_t reg, char const* sym_name) override;
		virtual void Move(char const* sym_name, int64 val) override;
		virtual void Move(register_t reg, int64 val) override;

		virtual void Inc(register_t reg) override;
		virtual void Inc(char const* sym_name) override;
		virtual void Dec(register_t reg) override;
		virtual void Dec(char const* sym_name) override;

		virtual void Add(register_t reg1, register_t reg2) override;
		virtual void AddImm(register_t reg1, int64 val) override;
		virtual void Sub(register_t reg1, register_t reg2) override;
		virtual void SubImm(register_t reg1, int64 val) override;
		virtual void Neg(register_t reg) override;

		virtual void GenerateLabelId() override;
		virtual void Label(char const* label) override;
		virtual void Compare(register_t reg, int64 val = 0) override;
		virtual void Compare(register_t reg1, register_t reg2) override;
		virtual void Set(register_t reg, Condition cond) override;
		virtual void Jump(char const* label, Condition cond) override;

		virtual void DeclareVariable(char const* sym_name, bool is_static) override;
		virtual void DeclareExternVariable(char const* sym_name) override;
		virtual void DeclareFunction(char const* sym_name, bool is_static) override;
		virtual void DeclareExternFunction(char const* sym_name) override;

		virtual void CallFunction(char const* sym_name) override;
		virtual void JumpToFunctionEnd() override;
		virtual void ReturnFromFunction() override;

	private:
		OutputBuffer& output_buffer;
		size_t label_id;
		char const* current_func_name;
		std::array<bool, REG_COUNT> free_registers;

	private:
		template<SegmentType segment, typename... Ts>
		void Emit(std::string_view fmt, Ts&&... args);

		static size_t GenerateUniqueInteger();
	};

}