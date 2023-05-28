#pragma once

namespace lucc
{
	class ICodeGenerator
	{
	public:
		virtual ~ICodeGenerator() = default;
		virtual void Generate() = 0;
	};

	using register_t = size_t;
	inline constexpr register_t INVALID_REG = -1;

	class ICodegenContext
	{
	public:
		virtual ~ICodegenContext() = default;

		virtual void		FreeAllRegisters() = 0;
		virtual register_t	AllocateRegister() = 0;
		virtual void		FreeRegister(register_t reg) = 0;
		
		virtual void Mov(register_t reg, int64 v) = 0;
		virtual void Add(register_t reg1, register_t reg2) = 0;

		virtual void GenerateLabelId() = 0;
		virtual void Label(char const* label) = 0;
		virtual void Compare(register_t reg, int64 value = 0) = 0;
		virtual void Jump(char const* label) = 0;
		virtual void JumpZero(char const* label) = 0;
		
		virtual void StoreReg(char const* sym_name, register_t reg) = 0;
		virtual void LoadReg(char const* sym_name, register_t reg) = 0;
		virtual void StoreImm(char const* sym_name, int64 val) = 0;

		virtual void DeclareStaticVariable(char const* sym_name) = 0;
		virtual void DeclareGlobalVariable(char const* sym_name) = 0;

		virtual void DeclareStaticFunction(char const* sym_name) = 0;
		virtual void DeclareGlobalFunction(char const* sym_name) = 0;
		virtual void ReturnFromFunction(char const* sym_name) = 0;
	};

	

}