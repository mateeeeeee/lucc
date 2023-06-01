#include <format>
#include "x86_64Context.h"

namespace lucc
{

	x86_64CodeGenerator::Context::Context(OutputBuffer& output_buffer) : output_buffer(output_buffer)
	{
		free_registers.fill(true);
		Emit<Data>(".data");
		Emit<Text>(".code");
	}

	register_t x86_64CodeGenerator::Context::AllocateRegisterForFunctionArg(size_t arg_index)
	{
		LU_ASSERT_MSG(arg_index < FUNC_ARGS_COUNT_IN_REGISTERS, "Maximum of 4 parameters are passed in registers!");
		static size_t mapping[FUNC_ARGS_COUNT_IN_REGISTERS] = { 4, 5, 2, 3 };
		size_t const i = mapping[arg_index];
		if (free_registers[i])
		{
			free_registers[i] = false;
			return register_t(i);
		}
		else LU_ASSERT(false);
		return INVALID_REG;
	}
	register_t x86_64CodeGenerator::Context::AllocateRegisterForReturn()
	{
		if (free_registers[RETURN_REGISTER_INDEX])
		{
			free_registers[RETURN_REGISTER_INDEX] = false;
			return register_t(RETURN_REGISTER_INDEX);
		}
		else LU_ASSERT(false);
		return INVALID_REG;
	}
	register_t x86_64CodeGenerator::Context::AllocateRegister()
	{
		for (size_t i = 0; i < REG_COUNT; ++i)
		{
			if (free_registers[i])
			{
				free_registers[i] = false;
				return register_t(i);
			}
		}
		LU_ASSERT_MSG(false, "Register spilling not implemented yet");
		return INVALID_REG;
	}
	void x86_64CodeGenerator::Context::FreeAllRegisters()
	{
		free_registers.fill(true);
	}
	void x86_64CodeGenerator::Context::FreeRegister(register_t reg)
	{
		free_registers[reg.id] = true;
	}

	void x86_64CodeGenerator::Context::SubImm(register_t reg1, int64 val)
	{
		Emit<Text>("sub\t{}, {}", dword_registers[reg1.id], val);
	}
	void x86_64CodeGenerator::Context::Sub(register_t reg1, register_t reg2)
	{
		Emit<Text>("sub\t{}, {}", dword_registers[reg1.id], dword_registers[reg2.id]);
	}
	void x86_64CodeGenerator::Context::AddImm(register_t reg1, int64 val)
	{
		Emit<Text>("add\t{}, {}", dword_registers[reg1.id], val);
	}
	void x86_64CodeGenerator::Context::Add(register_t reg1, register_t reg2)
	{
		Emit<Text>("add\t{}, {}", dword_registers[reg1.id], dword_registers[reg2.id]);
	}
	void x86_64CodeGenerator::Context::Neg(register_t reg)
	{
		Emit<Text>("neg\t{}", dword_registers[reg.id]);
	}

	void x86_64CodeGenerator::Context::Dec(char const* sym_name)
	{
		Emit<Text>("dec\t{}", sym_name);
	}
	void x86_64CodeGenerator::Context::Dec(register_t reg)
	{
		Emit<Text>("dec\t{}", dword_registers[reg.id]);
	}
	void x86_64CodeGenerator::Context::Inc(char const* sym_name)
	{
		Emit<Text>("inc\t{}", sym_name);
	}
	void x86_64CodeGenerator::Context::Inc(register_t reg)
	{
		Emit<Text>("inc\t{}", dword_registers[reg.id]);
	}

	void x86_64CodeGenerator::Context::Move(register_t reg, int64 val)
	{
		Emit<Text>("mov\t{}, {}", dword_registers[reg.id], val);
	}
	void x86_64CodeGenerator::Context::Move(char const* sym_name, int64 val)
	{
		Emit<Text>("mov\t{}, {}", sym_name, val);
	}
	void x86_64CodeGenerator::Context::Move(char const* sym_name, register_t reg)
	{
		Emit<Text>("mov\t{}, {}", sym_name, dword_registers[reg.id]);
	}
	void x86_64CodeGenerator::Context::Move(register_t reg, char const* sym_name)
	{
		Emit<Text>("mov\t{}, {}", dword_registers[reg.id], sym_name);
	}

	void x86_64CodeGenerator::Context::Jump(char const* label, Condition cond)
	{
		switch (cond)
		{
		case Condition::Unconditional: Emit<Text>("jmp\t{}{}", label, label_id); break;
		case Condition::Equal:		   Emit<Text>("je\t{}{}", label, label_id);  break;
		case Condition::NotEqual:	   Emit<Text>("jne\t{}{}", label, label_id);  break;
		case Condition::Greater:	   Emit<Text>("jg\t{}{}", label, label_id);  break;
		case Condition::GreaterEqual:  Emit<Text>("jge\t{}{}", label, label_id);  break;
		case Condition::Less:		   Emit<Text>("jl\t{}{}", label, label_id);  break;
		case Condition::LessEqual:	   Emit<Text>("jle\t{}{}", label, label_id);  break;
		}
	}
	void x86_64CodeGenerator::Context::Set(register_t reg, Condition cond)
	{
		char const* reg_name = byte_registers[reg.id];
		switch (cond)
		{
		case Condition::Unconditional: LU_ASSERT(false); break;
		case Condition::Equal:		   Emit<Text>("sete\t{}", reg_name);  break;
		case Condition::NotEqual:	   Emit<Text>("setne\t{}", reg_name);  break;
		case Condition::Greater:	   Emit<Text>("setg\t{}", reg_name);  break;
		case Condition::GreaterEqual:  Emit<Text>("setge\t{}", reg_name);  break;
		case Condition::Less:		   Emit<Text>("setl\t{}", reg_name);  break;
		case Condition::LessEqual:	   Emit<Text>("setle\t{}", reg_name);  break;
		}
	}
	void x86_64CodeGenerator::Context::Compare(register_t reg1, register_t reg2)
	{
		Emit<Text>("cmp\t{}, {}", dword_registers[reg1.id], dword_registers[reg2.id]);
	}
	void x86_64CodeGenerator::Context::Compare(register_t reg, int64 val /*= 0*/)
	{
		Emit<Text>("cmp\t{}, {}", dword_registers[reg.id], val);
	}
	void x86_64CodeGenerator::Context::Label(char const* label)
	{
		Emit<Text>("{}{}: ", label, label_id);
	}
	void x86_64CodeGenerator::Context::GenerateLabelId()
	{
		label_id = GenerateUniqueInteger();
	}

	void x86_64CodeGenerator::Context::DeclareVariable(char const* sym_name, bool is_static)
	{
		if (!is_static) Emit<None>("public {}", sym_name);
		Emit<Data>("{}\tdword ?", sym_name);
	}
	void x86_64CodeGenerator::Context::DeclareExternVariable(char const* sym_name)
	{
		Emit<None>("extern {} : dword", sym_name);
	}
	void x86_64CodeGenerator::Context::DeclareFunction(char const* sym_name, bool is_static)
	{
		current_func_name = sym_name;
		Emit<Text>("\n{} proc {}", sym_name, is_static ? "private" : "");
	}
	void x86_64CodeGenerator::Context::DeclareExternFunction(char const* sym_name)
	{
		Emit<None>("extern {} : proc", sym_name);
	}

	void x86_64CodeGenerator::Context::CallFunction(char const* sym_name)
	{
		Emit<Text>("call {}", sym_name);
	}
	void x86_64CodeGenerator::Context::JumpToFunctionEnd()
	{
		Emit<Text>("jmp {}_end", current_func_name);
	}
	void x86_64CodeGenerator::Context::ReturnFromFunction()
	{
		Emit<Text>("{}_end:", current_func_name);
		Emit<Text>("ret");
		Emit<Text>("{} endp", current_func_name);
	}

	size_t x86_64CodeGenerator::Context::GenerateUniqueInteger()
	{
		static size_t i = 0;
		return ++i;
	}

	template<x86_64CodeGenerator::Context::SegmentType segment, typename... Ts>
	void x86_64CodeGenerator::Context::Emit(std::string_view fmt, Ts&&... args)
	{
		std::string output = std::vformat(fmt, std::make_format_args(std::forward<Ts>(args)...));
		output += "\n";
		if		constexpr (segment == x86_64CodeGenerator::Context::SegmentType::None)	output_buffer.preamble += output;
		else if constexpr (segment == x86_64CodeGenerator::Context::SegmentType::Data)	 output_buffer.data_segment += output;
		else if constexpr (segment == x86_64CodeGenerator::Context::SegmentType::Text)	 output_buffer.text_segment += output;
	}
}

