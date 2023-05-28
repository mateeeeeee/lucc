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

		virtual void Mov(int64 v, register_t reg) = 0;
		virtual void Add(register_t reg1, register_t reg2) = 0;
		
		virtual void Store(char const* sym_name, register_t reg) = 0;
		virtual void Load(char const* sym_name, register_t reg) = 0;
		
		virtual void DeclareStaticVariable(char const* sym_name) = 0;
		virtual void DeclareGlobalVariable(char const* sym_name) = 0;

		virtual void DeclareStaticFunction(char const* sym_name) = 0;
		virtual void DeclareGlobalFunction(char const* sym_name) = 0;
		virtual void ReturnFromFunction(char const* sym_name) = 0;
	};

	

}