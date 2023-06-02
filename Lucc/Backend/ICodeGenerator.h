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

	class ICodegenContext
	{
	public:
		virtual ~ICodegenContext() = default;

		virtual void		FreeAllRegisters() = 0;
		virtual register_t	AllocateRegister() = 0;
		virtual register_t	AllocateRegisterForReturn() = 0;
		virtual register_t	AllocateRegisterForFunctionArg(size_t arg_index) = 0;
		virtual void		FreeRegister(register_t reg) = 0;

		virtual void Move(char const* sym_name, register_t reg) = 0;
		virtual void Move(register_t reg, char const* sym_name) = 0;
		virtual void Move(char const* sym_name, int64 val) = 0;
		virtual void Move(register_t reg, int64 v) = 0;

		virtual void Inc(register_t reg) = 0;
		virtual void Inc(char const* sym_name) = 0;
		virtual void Dec(register_t reg) = 0;
		virtual void Dec(char const* sym_name) = 0;

		virtual void Add(register_t reg1, register_t reg2) = 0;
		virtual void AddImm(register_t reg1, int64 v) = 0;
		virtual void Sub(register_t reg1, register_t reg2) = 0;
		virtual void SubImm(register_t reg1, int64 v) = 0;
		virtual void Neg(register_t reg) = 0;


		virtual void GenerateLabelId() = 0;
		virtual void Label(char const* label) = 0;
		virtual void Compare(register_t reg, int64 value = 0) = 0;
		virtual void Compare(register_t reg1, register_t reg2) = 0;
		virtual void Set(register_t reg, Condition cond) = 0;
		virtual void Jump(char const* label, Condition cond = Condition::Unconditional) = 0;

		virtual void DeclareVariable(char const* sym_name, bool is_static) = 0;
		virtual void DeclareExternVariable(char const* sym_name) = 0;
		virtual void DeclareFunction(char const* sym_name, bool is_static) = 0;
		virtual void DeclareExternFunction(char const* sym_name) = 0;

		virtual void CallFunction(char const* func_name) = 0;
		virtual void JumpToFunctionEnd() = 0;
		virtual void ReturnFromFunction() = 0;
	};


}