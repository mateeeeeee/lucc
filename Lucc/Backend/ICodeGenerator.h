#pragma once

namespace lucc
{
	class ICodeGenerator
	{
	public:
		virtual ~ICodeGenerator() = default;
		virtual void Generate() = 0;
	};

	class ICodegenContext
	{
	public:
		virtual ~ICodegenContext() = default;

		virtual void FreeAllRegisters() = 0;
		virtual size_t AllocateRegister() = 0;
		virtual void FreeRegister(size_t reg) = 0;

		virtual void Movq(int64 v, size_t reg) = 0;
		virtual void Add(size_t reg1, size_t reg2) = 0;
		//virtual void Push() = 0;
		//virtual void Pop() = 0;
		//
		//virtual void Load() = 0;
		//virtual void Store() = 0;
		virtual void Store(char const* sym_name, size_t reg) = 0;
		virtual void Load(char const* sym_name, size_t reg) = 0;
		
		virtual void DeclareGlobalSymbol(char const* sym_name) = 0;
	};

	inline constexpr size_t INVALID_REG(-1);

}