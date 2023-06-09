#pragma once
#include <array>
#include <string>
#include "Core/Defines.h"
#include "x86_64CodeGenerator.h"
#include "ICodegenContext.h"

namespace lucc
{

	class x86_64CodeGenerator::Context : public ICodegenContext
	{
		static constexpr uint64 GP_REG_COUNT = 9;
		static constexpr uint64 FUNC_ARGS_COUNT_IN_REGISTERS = 4;
		static constexpr uint64 RETURN_REGISTER_INDEX = GP_REG_COUNT - 1;
		static constexpr uint64 STACK_FRAME_REGISTER_INDEX = GP_REG_COUNT;
		static constexpr size_t FUNC_ARG_REG_MAPPING[FUNC_ARGS_COUNT_IN_REGISTERS] = { 4, 5, 2, 3 };

		static constexpr char const* registers[GP_REG_COUNT + 1][BitMode_Count] = {
			{"r10b", "r10w", "r10d", "r10"},
			{"r11b", "r11w", "r11d", "r11"},
			{"r8b" , "r8w" , "r8d" , "r8" },
			{"r9b" , "r9w" , "r9d" , "r9" },
			{"cl"  , "cx"  , "ecx" , "rcx"},
			{"dl"  , "dx"  , "edx" , "rdx"},
			{"dil" , "di"  , "edi" , "rdi"},
			{"sil" , "si"  , "esi" , "rsi"},
			{"al"  , "ax"  , "eax" , "rax"},
			{"bpl" , "bp"  , "ebp" , "rbp"}};

		enum SegmentType : uint16
		{
			None,
			Data,
			Text
		};

	public:
		explicit Context(OutputBuffer& output_buffer);

		//registers
		virtual register_t AllocateRegister() override;
		virtual register_t AllocateReturnRegister() override;
		virtual register_t AllocateFunctionArgumentRegister(size_t arg_index) override;
		virtual register_t GetFunctionArgumentRegister(size_t arg_index) override;
		virtual register_t GetStackFrameRegister() override;
		virtual void FreeRegister(register_t reg) override;
		virtual void FreeAllRegisters() override;

		//arithmetic
		virtual void Add(register_t dst, int32 value, BitMode bitmode) override;
		virtual void Add(register_t dst, register_t src, BitMode bitmode) override;
		virtual void Add(register_t dst, char const* mem, BitMode bitmode) override;
		virtual void Add(char const* mem, register_t src, BitMode bitmode) override;
		virtual void Add(char const* mem, int64 value, BitMode bitmode) override;

		virtual void Sub(register_t dst, int32 value, BitMode bitmode) override;
		virtual void Sub(register_t dst, register_t src, BitMode  bitmode) override;
		virtual void Sub(register_t dst, char const* mem, BitMode bitmode) override;
		virtual void Sub(char const* mem, register_t src, BitMode bitmode) override;
		virtual void Sub(char const* mem, int64 value, BitMode bitmode) override;

		virtual void Imul(register_t dst, register_t src, BitMode bitmode) override;
		virtual void Imul(register_t dst, char const* mem, BitMode bitmode) override;
		virtual void Imul(register_t dst, register_t src, int32 value, BitMode bitmode) override;
		virtual void Imul(register_t dst, char const* mem, int32 value, BitMode bitmode) override;


		virtual void Neg(register_t reg, BitMode bitmode) override;
		virtual void Neg(char const* mem, BitMode bitmode) override;

		virtual void Inc(char const* mem, BitMode bitmode) override;
		virtual void Inc(register_t reg, BitMode bitmode) override;
		virtual void Dec(char const* mem, BitMode bitmode) override;
		virtual void Dec(register_t reg, BitMode bitmode) override;

		virtual void Not(register_t reg, BitMode bitmode) override;
		virtual void Not(char const* mem, BitMode bitmode) override;
		virtual void Not(mem_ref_t const& mem_ref, BitMode bitmode) override;

		//stack
		virtual void Push(register_t reg, BitMode bitmode) override;
		virtual void Push(char const* mem, BitMode bitmode) override;
		virtual void Push(mem_ref_t const& mem_ref, BitMode bitmode) override;
		virtual void Push(int32 value, BitMode bitmode) override;

		virtual void Pop(register_t reg, BitMode bitmode) override;
		virtual void Pop(char const* mem, BitMode bitmode) override;
		virtual void Pop(mem_ref_t const& mem_ref, BitMode bitmode) override;

		//control
		virtual void GenerateLabelId() override;
		virtual void Label(char const* lbl) override;
		virtual void Cmp(register_t reg, int64 value, BitMode bitmode) override;
		virtual void Cmp(char const* mem, int64 value, BitMode bitmode) override;
		virtual void Cmp(register_t reg1, register_t reg2, BitMode bitmode) override;
		virtual void Cmp(char const* mem, register_t reg2, BitMode bitmode) override;
		virtual void Cmp(register_t reg1, char const* mem, BitMode bitmode) override;
		virtual void Set(register_t reg, Condition cond) override;
		virtual void Set(char const* mem, Condition cond) override;
		virtual void Jmp(char const* label, Condition cond = Condition::Unconditional) override;

		//transfer
		virtual void Mov(register_t reg, int64 value, BitMode bitmode) override;
		virtual void Mov(char const* mem, int32 value, BitMode bitmode) override;
		virtual void Mov(mem_ref_t const& mem_ref, int32 value, BitMode bitmode) override;

		virtual void Mov(register_t dst, register_t src, BitMode bitmode) override;
		virtual void Mov(register_t dst, char const* mem, BitMode bitmode, bool address = false) override;
		virtual void Mov(register_t dst, mem_ref_t const& mem_ref, BitMode bitmode) override;
		virtual void Mov(char const* mem, register_t src, BitMode bitmode) override;
		virtual void Mov(mem_ref_t const& mem_ref, register_t src, BitMode bitmode) override;

		virtual void Lea(register_t reg, char const* mem) override;
		virtual void Lea(register_t reg, mem_ref_t const& mem_ref) override;


		//declarations
		virtual void DeclareVariable(char const* sym_name, bool is_static, BitMode bitmode, int64* init = nullptr) override;
		virtual void DeclareArray(char const* sym_name, size_t size, bool is_static, BitMode bitmode, int64 init_arr[] = nullptr, size_t init_size = 0) override;
		virtual void DeclareExternVariable(char const* sym_name, BitMode bitmode) override;
		virtual void DeclareFunction(char const* sym_name, bool is_static) override;
		virtual void DeclareExternFunction(char const* sym_name) override;

		//functions
		virtual uint64 GetFunctionArgsInRegisters() const override { return FUNC_ARGS_COUNT_IN_REGISTERS; }
		virtual void SaveStackPointer() override;
		virtual void ReserveStackSpace(uint32 stack_space) override;
		virtual void CallFunction(char const* sym_name) override;
		virtual void JumpToFunctionEnd() override;
		virtual void Return() override;
		
	private:
		OutputBuffer& output_buffer;
		std::array<bool, GP_REG_COUNT> free_registers;

		size_t label_id;
		char const* current_func_name;
		bool saved_stack_pointer;
		bool reserved_stack;

	private:
		template<SegmentType segment, typename... Ts>
		void Emit(std::string_view fmt, Ts&&... args);

		static size_t GenerateUniqueInteger();
		static std::string ConvertMemRef(mem_ref_t const& args, BitMode mode);
		static std::string ConvertToType(BitMode mode);
		static std::string ConvertToCast(BitMode mode);
	};

}