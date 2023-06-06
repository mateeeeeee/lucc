#pragma once
#include <array>
#include <string>
#include "Core/Defines.h"
#include "x86_64CodeGenerator.h"

namespace lucc
{

	class x86_64CodeGenerator::Context : public ICodegenContext
	{
		static constexpr uint64 REG_COUNT = 9;
		static constexpr uint64 FUNC_ARGS_COUNT_IN_REGISTERS = 4;
		static constexpr uint64 RETURN_REGISTER_INDEX = REG_COUNT - 1;

		static constexpr char const* registers[REG_COUNT][BitMode_Count] = {
			{"r10b", "r10w", "r10d", "r10"},
			{"r11b", "r11w", "r11d", "r11"},
			{"r8b" , "r8w" , "r8d" , "r8" },
			{"r9b" , "r9w" , "r9d" , "r9" },
			{"cl"  , "cx"  , "ecx" , "rcx"},
			{"dl"  , "dx"  , "edx" , "rdx"},
			{"dil" , "di"  , "edi" , "rdi"},
			{"sil" , "si"  , "esi" , "rsi"},
			{"al"  , "ax"  , "eax" , "rax"}};

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
		virtual register_t AllocateRegisterForReturn() override;
		virtual register_t AllocateRegisterForFunctionArg(size_t arg_index) override;
		virtual void FreeRegister(register_t reg) override;
		virtual void FreeAllRegisters() override;

		//arithmetic
		virtual void Add(register_t dst, int32 value, BitMode bitmode = BitMode_64) override;
		virtual void Add(register_t dst, register_t src, BitMode bitmode = BitMode_64) override;
		virtual void Add(register_t dst, char const* mem, BitMode bitmode = BitMode_64) override;
		virtual void Add(char const* mem, register_t src, BitMode bitmode = BitMode_64) override;
		virtual void Add(char const* mem, int64 value, BitMode bitmode = BitMode_64) override;

		virtual void Sub(register_t dst, int32 value, BitMode bitmode = BitMode_64) override;
		virtual void Sub(register_t dst, register_t src, BitMode  bitmode = BitMode_64) override;
		virtual void Sub(register_t dst, char const* mem, BitMode bitmode = BitMode_64) override;
		virtual void Sub(char const* mem, register_t src, BitMode bitmode = BitMode_64) override;
		virtual void Sub(char const* mem, int64 value, BitMode bitmode = BitMode_64) override;

		virtual void Imul(register_t dst, register_t src, BitMode bitmode = BitMode_64) override;
		virtual void Imul(register_t dst, char const* mem, BitMode bitmode = BitMode_64) override;
		virtual void Imul(register_t dst, register_t src, int32 value, BitMode bitmode = BitMode_64) override;
		virtual void Imul(register_t dst, char const* mem, int32 value, BitMode bitmode = BitMode_64) override;


		virtual void Neg(register_t reg, BitMode bitmode = BitMode_64) override;
		virtual void Neg(char const* mem) override;

		virtual void Inc(char const* mem, BitMode bitmode = BitMode_64) override;
		virtual void Inc(register_t reg, BitMode bitmode = BitMode_64) override;
		virtual void Dec(char const* mem, BitMode bitmode = BitMode_64) override;
		virtual void Dec(register_t reg, BitMode bitmode = BitMode_64) override;

		//control
		virtual void GenerateLabelId() override;
		virtual void Label(char const* lbl) override;
		virtual void Cmp(register_t reg, int64 value, BitMode bitmode = BitMode_64) override;
		virtual void Cmp(char const* mem, int64 value, BitMode bitmode = BitMode_64) override;
		virtual void Cmp(register_t reg1, register_t reg2, BitMode bitmode = BitMode_64) override;
		virtual void Cmp(char const* mem, register_t reg2, BitMode bitmode = BitMode_64) override;
		virtual void Cmp(register_t reg1, char const* mem, BitMode bitmode = BitMode_64) override;
		virtual void Set(register_t reg, Condition cond) override;
		virtual void Set(char const* mem, Condition cond) override;
		virtual void Jmp(char const* label, Condition cond = Condition::Unconditional) override;

		//transfer
		virtual void Mov(register_t reg, int64 value, BitMode bitmode = BitMode_64) override;
		virtual void Mov(char const* mem, int32 value, BitMode bitmode = BitMode_64) override;
		virtual void Mov(mem_ref_t const& mem_ref, int32 value, BitMode bitmode = BitMode_64) override;

		virtual void Mov(register_t dst, register_t src, BitMode bitmode = BitMode_64) override;
		virtual void Mov(register_t dst, char const* mem, BitMode bitmode = BitMode_64, bool address = false) override;
		virtual void Mov(register_t dst, mem_ref_t const& mem_ref, BitMode bitmode = BitMode_64) override;
		virtual void Mov(char const* mem, register_t src, BitMode bitmode = BitMode_64) override;
		virtual void Mov(mem_ref_t const& mem_ref, register_t src, BitMode bitmode = BitMode_64) override;

		virtual void Lea(register_t reg, char const* mem) override;
		virtual void Lea(register_t reg, mem_ref_t const& mem_ref) override;


		//declarations
		virtual void DeclareVariable(char const* sym_name, bool is_static, BitMode bitmode = BitMode_64) override;
		virtual void DeclareArray(char const* sym_name, size_t size, bool is_static, BitMode bitmode = BitMode_64) override;
		virtual void DeclareExternVariable(char const* sym_name, BitMode bitmode = BitMode_64) override;
		virtual void DeclareFunction(char const* sym_name, bool is_static) override;
		virtual void DeclareExternFunction(char const* sym_name) override;

		//functions
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
		static std::string ConvertMemRef(mem_ref_t const& args, BitMode mode);
		static std::string ConvertToType(BitMode mode);
		static std::string ConvertToCast(BitMode mode);
	};

}