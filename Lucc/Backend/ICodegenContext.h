#pragma once

namespace lucc
{
	struct register_t
	{
		uint16 id;
	};
	inline constexpr register_t INVALID_REG = register_t{ .id = uint16(-1) };
	inline constexpr bool operator==(register_t r1, register_t r2)
	{
		return r1.id == r2.id;
	}
	inline constexpr bool operator!=(register_t r1, register_t r2)
	{
		return r1.id != r2.id;
	}

	struct mem_ref_t
	{
		enum Scale
		{
			Scale_None = 0x0,
			Scale_x1 = 0x1,
			Scale_x2 = 0x2,
			Scale_x4 = 0x4,
			Scale_x8 = 0x8,
		};
		register_t base_reg = INVALID_REG;
		register_t index_reg = INVALID_REG;
		Scale scale = Scale_None;
		int32 displacement = 0;
	};

	enum class Condition
	{
		Unconditional,
		Equal,
		NotEqual,
		Greater,
		GreaterEqual,
		Less,
		LessEqual
	};
	enum BitMode
	{
		BitMode_8,
		BitMode_16,
		BitMode_32,
		BitMode_64,
		BitMode_Count
	};

	class ICodegenContext
	{
	public:
		virtual ~ICodegenContext() = default;

		//registers
		virtual void		FreeAllRegisters() = 0;
		virtual register_t	AllocateRegister() = 0;
		virtual register_t	AllocateReturnRegister() = 0;
		virtual register_t	AllocateFunctionArgumentRegister(uint16 arg_index) = 0;
		virtual register_t  GetFunctionArgumentRegister(uint16 arg_index) = 0;
		virtual register_t  GetStackFrameRegister() = 0;
		virtual void		FreeRegister(register_t reg) = 0;

		//arithmetic
		virtual void Add(register_t dst, int32 value, BitMode bitmode) = 0;
		virtual void Add(register_t dst, register_t src, BitMode bitmode) = 0;
		virtual void Add(register_t dst, char const* mem, BitMode bitmode) = 0;
		virtual void Add(char const* mem, register_t src, BitMode bitmode) = 0;
		virtual void Add(char const* mem, int64 value, BitMode bitmode) = 0;

		virtual void Sub(register_t dst, int32 value, BitMode bitmode) = 0;
		virtual void Sub(register_t dst, register_t src, BitMode  bitmode) = 0;
		virtual void Sub(register_t dst, char const* mem, BitMode bitmode) = 0;
		virtual void Sub(char const* mem, register_t src, BitMode bitmode) = 0;
		virtual void Sub(char const* mem, int64 value, BitMode bitmode) = 0;

		//signed multiply
		virtual void Imul(register_t dst, register_t src, BitMode bitmode) = 0;
		virtual void Imul(register_t dst, char const* mem, BitMode bitmode) = 0;
		virtual void Imul(register_t dst, register_t src, int32 value, BitMode bitmode) = 0;
		virtual void Imul(register_t dst, char const* mem, int32 value, BitMode bitmode) = 0;

		//Signed divide
		virtual void Idiv(register_t dividend, register_t divisor, BitMode bitmode) = 0;
		virtual void Idiv(register_t dividend, char const* divisor, BitMode bitmode) = 0;
		//Mul, Div missing (unsigned versions)

		virtual void Neg(register_t reg, BitMode bitmode) = 0;
		virtual void Neg(char const* mem, BitMode bitmode) = 0;

		virtual void Inc(char const* mem, BitMode bitmode) = 0;
		virtual void Inc(mem_ref_t const& mem_ref, BitMode bitmode) = 0;
		virtual void Inc(register_t reg, BitMode bitmode) = 0;
		virtual void Dec(char const* mem, BitMode bitmode) = 0;
		virtual void Dec(mem_ref_t const& mem_ref, BitMode bitmode) = 0;
		virtual void Dec(register_t reg, BitMode bitmode) = 0;

		//shifts
		virtual void Shl(register_t dst, uint8 value, BitMode bitmode) = 0;
		virtual void Shl(register_t dst, register_t shift_reg, BitMode bitmode) = 0;
		virtual void Shl(char const* mem, uint8 value, BitMode bitmode) = 0;
		virtual void Shl(char const* mem, register_t shift_reg, BitMode bitmode) = 0;
		virtual void Shl(mem_ref_t const& mem_ref, uint8 value, BitMode bitmode) = 0;
		virtual void Shl(mem_ref_t const& mem_ref, register_t shift_reg, BitMode bitmode) = 0;

		virtual void Shr(register_t dst, uint8 value, BitMode bitmode) = 0;
		virtual void Shr(register_t dst, register_t shift_reg, BitMode bitmode) = 0;
		virtual void Shr(char const* mem, uint8 value, BitMode bitmode) = 0;
		virtual void Shr(char const* mem, register_t shift_reg, BitMode bitmode) = 0;
		virtual void Shr(mem_ref_t const& mem_ref, uint8 value, BitMode bitmode) = 0;
		virtual void Shr(mem_ref_t const& mem_ref, register_t shift_reg, BitMode bitmode) = 0;

		virtual void Sar(register_t dst, uint8 value, BitMode bitmode) = 0;
		virtual void Sar(register_t dst, register_t shift_reg, BitMode bitmode) = 0;
		virtual void Sar(char const* mem, uint8 value, BitMode bitmode) = 0;
		virtual void Sar(char const* mem, register_t shift_reg, BitMode bitmode) = 0;
		virtual void Sar(mem_ref_t const& mem_ref, uint8 value, BitMode bitmode) = 0;
		virtual void Sar(mem_ref_t const& mem_ref, register_t shift_reg, BitMode bitmode) = 0;

		//logical
		virtual void Not(register_t reg, BitMode bitmode) = 0;
		virtual void Not(char const* mem, BitMode bitmode) = 0;
		virtual void Not(mem_ref_t const& mem_ref, BitMode bitmode) = 0;

		virtual void And(register_t dst, register_t src, BitMode bitmode) = 0;
		virtual void And(register_t dst, char const* mem, BitMode bitmode) = 0;
		virtual void And(register_t dst, mem_ref_t const& mem_ref, BitMode bitmode) = 0;
		virtual void And(char const* mem, register_t src, BitMode bitmode) = 0;
		virtual void And(mem_ref_t const& mem_ref, register_t src, BitMode bitmode) = 0;
		virtual void And(mem_ref_t const& mem_ref, int32 value, BitMode bitmode) = 0;
		virtual void And(char const* mem, int32 value, BitMode bitmode) = 0;
		virtual void And(register_t dst, int32 value, BitMode bitmode) = 0;

		virtual void Or(register_t dst, register_t src, BitMode bitmode) = 0;
		virtual void Or(register_t dst, char const* mem, BitMode bitmode) = 0;
		virtual void Or(register_t dst, mem_ref_t const& mem_ref, BitMode bitmode) = 0;
		virtual void Or(char const* mem, register_t src, BitMode bitmode) = 0;
		virtual void Or(mem_ref_t const& mem_ref, register_t src, BitMode bitmode) = 0;
		virtual void Or(mem_ref_t const& mem_ref, int32 value, BitMode bitmode) = 0;
		virtual void Or(char const* mem, int32 value, BitMode bitmode) = 0;
		virtual void Or(register_t dst, int32 value, BitMode bitmode) = 0;

		virtual void Xor(register_t dst, register_t src, BitMode bitmode) = 0;
		virtual void Xor(register_t dst, char const* mem, BitMode bitmode) = 0;
		virtual void Xor(register_t dst, mem_ref_t const& mem_ref, BitMode bitmode) = 0;
		virtual void Xor(char const* mem, register_t src, BitMode bitmode) = 0;
		virtual void Xor(mem_ref_t const& mem_ref, register_t src, BitMode bitmode) = 0;
		virtual void Xor(mem_ref_t const& mem_ref, int32 value, BitMode bitmode) = 0;
		virtual void Xor(char const* mem, int32 value, BitMode bitmode) = 0;
		virtual void Xor(register_t dst, int32 value, BitMode bitmode) = 0;

		//stack
		virtual void Push(register_t reg, BitMode bitmode) = 0;
		virtual void Push(char const* mem, BitMode bitmode) = 0;
		virtual void Push(mem_ref_t const& mem_ref, BitMode bitmode) = 0;
		virtual void Push(int32 value, BitMode bitmode) = 0;

		virtual void Pop(register_t reg, BitMode bitmode) = 0;
		virtual void Pop(char const* mem, BitMode bitmode) = 0;
		virtual void Pop(mem_ref_t const& mem_ref, BitMode bitmode) = 0;
		//control
		virtual uint64 GenerateLabelId() = 0;
		virtual void Label(char const* label, uint64 label_id) = 0;
		virtual void Cmp(register_t reg, int64 value, BitMode bitmode) = 0;
		virtual void Cmp(char const* mem, int64 value, BitMode bitmode) = 0;
		virtual void Cmp(register_t reg1, register_t reg2, BitMode bitmode) = 0;
		virtual void Cmp(char const* mem, register_t reg2, BitMode bitmode) = 0;
		virtual void Cmp(register_t reg1, char const* mem, BitMode bitmode) = 0;
		virtual void Set(register_t reg, Condition cond) = 0;
		virtual void Set(char const* mem, Condition cond) = 0;
		virtual void Jmp(char const* label, uint64 label_id, Condition cond = Condition::Unconditional) = 0;

		//transfer
		virtual void Mov(register_t reg, int64 value, BitMode bitmode) = 0;
		virtual void Mov(char const* mem, int32 value, BitMode bitmode) = 0;
		virtual void Mov(mem_ref_t const& mem_ref, int32 value, BitMode bitmode) = 0;
		virtual void Mov(register_t dst, register_t src, BitMode bitmode) = 0;
		virtual void Mov(register_t dst, char const* mem, BitMode bitmode, bool address = false) = 0;
		virtual void Mov(register_t dst, mem_ref_t const& mem_ref, BitMode bitmode) = 0;
		virtual void Mov(char const* mem, register_t src, BitMode bitmode) = 0;
		virtual void Mov(mem_ref_t const& mem_ref, register_t src, BitMode bitmode) = 0;

		virtual void Movzx(register_t dst, register_t src, BitMode bitmode, bool src_8bit = false) = 0;
		virtual void Movzx(register_t dst, char const* mem, BitMode bitmode, bool src_8bit = false) = 0;
		virtual void Movzx(register_t dst, mem_ref_t const& mem_ref, BitMode bitmode, bool src_8bit = false) = 0;

		virtual void Lea(register_t reg, char const* mem) = 0;
		virtual void Lea(register_t reg, mem_ref_t const& mem_ref) = 0;

		//declarations
		virtual void DeclareVariable(char const* sym_name, bool is_static, BitMode bitmode, int64* init = nullptr) = 0;
		virtual void DeclareArray(char const* sym_name, size_t size, bool is_static, BitMode bitmode, int64 init_arr[] = nullptr, size_t init_size = 0) = 0;
		virtual void DeclareExternVariable(char const* sym_name, BitMode bitmode) = 0;
		virtual void DeclareFunction(char const* sym_name, bool is_static) = 0;
		virtual void DeclareExternFunction(char const* sym_name) = 0;

		//functions
		virtual uint64 GetFunctionArgsInRegisters() const = 0;
		virtual void ReserveStackSpace(uint32 stack_space) = 0;
		virtual void CallFunction(char const* func_lbl) = 0;
		virtual void JumpToFunctionEnd() = 0;
		virtual void Return() = 0;
	};
}