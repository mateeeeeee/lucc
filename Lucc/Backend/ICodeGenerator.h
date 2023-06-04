#pragma once

namespace lucc
{
	struct AST;

	class ICodeGenerator
	{
	public:
		virtual ~ICodeGenerator() = default;
		virtual void Generate(AST*) = 0;
	};

	struct register_t
	{
		size_t id;
	};
	inline constexpr register_t INVALID_REG = register_t{ .id = size_t(-1) };
	inline constexpr bool operator==(register_t r1, register_t r2)
	{
		return r1.id == r2.id;
	}
	inline constexpr bool operator!=(register_t r1, register_t r2)
	{
		return r1.id != r2.id;
	}

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


	struct IndirectArgs
	{
		enum Scale
		{
			Scale_None = 0x0,
			Scale_x1   = 0x1,
			Scale_x2   = 0x2,
			Scale_x4   = 0x4,
			Scale_x8   = 0x8,
		};
		register_t base_reg = INVALID_REG;
		register_t index_reg = INVALID_REG;
		Scale scale = Scale_None;
		char const* label_displacement;
		size_t displacement = 0;
	};

	class ICodegenContext
	{
	public:
		virtual ~ICodegenContext() = default;

		virtual void		FreeAllRegisters() = 0;
		virtual register_t	AllocateRegister() = 0;
		virtual register_t	AllocateRegisterForReturn() = 0;
		virtual register_t	AllocateRegisterForFunctionArg(size_t arg_index) = 0;
		virtual void		FreeRegister(register_t reg) = 0;

		virtual void Move(char const* sym, register_t src) = 0;
		virtual void Move(register_t  dst, char const* sym) = 0;
		virtual void Move(register_t  dst, char const* sym, size_t offset) = 0;
		virtual void Move(char const* sym, int64 val) = 0;
		virtual void Move(register_t  dst, int64 val) = 0;

		virtual void MoveIndirect(register_t dst, register_t src) = 0;
		virtual void MoveIndirect(register_t dst, IndirectArgs const& src_indirect_args) = 0;
		virtual void MoveIndirect(IndirectArgs const& dst_indirect_args, register_t src) = 0;
		virtual void MoveIndirect(IndirectArgs const& dst_indirect_args, int64 val) = 0;

		virtual void LoadEffectiveAddress(register_t reg, char const* sym_name) = 0;

		virtual void Inc(register_t reg) = 0;
		virtual void Inc(char const* sym_name) = 0;
		virtual void Dec(register_t reg) = 0;
		virtual void Dec(char const* sym_name) = 0;

		virtual void Add(register_t reg1, register_t reg2) = 0;
		virtual void AddImm(register_t reg1, int64 v) = 0;
		virtual void Sub(register_t reg1, register_t reg2) = 0;
		virtual void SubImm(register_t reg1, int64 v) = 0;
		virtual void Neg(register_t reg) = 0;
		virtual void Neg(char const* sym_name) = 0;

		virtual void GenerateLabelId() = 0;
		virtual void Label(char const* label) = 0;
		virtual void Compare(register_t reg, int64 value = 0) = 0;
		virtual void Compare(register_t reg1, register_t reg2) = 0;
		virtual void Set(register_t reg, Condition cond) = 0;
		virtual void Jump(char const* label, Condition cond = Condition::Unconditional) = 0;

		virtual void DeclareVariable(char const* sym_name, bool is_static) = 0;
		virtual void DeclareArray(char const* sym_name, size_t size, bool is_static) = 0;
		virtual void DeclareExternVariable(char const* sym_name) = 0;
		virtual void DeclareFunction(char const* sym_name, bool is_static) = 0;
		virtual void DeclareExternFunction(char const* sym_name) = 0;

		virtual void CallFunction(char const* func_name) = 0;
		virtual void JumpToFunctionEnd() = 0;
		virtual void ReturnFromFunction() = 0;
	};


}