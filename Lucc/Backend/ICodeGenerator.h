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
		
		virtual void EmitGlobal(char const* sym, bool is_static) = 0;

		virtual void GenerateAddress(char const* name) = 0;
		virtual void GenerateInt64Literal(int64 value) = 0;

		// Store %rax to an address that the stack top is pointing to.
		virtual void Store(size_t type_size) = 0;
		virtual void Push() = 0;
	};

}