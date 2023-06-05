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

	//registers
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

	//arithmetic
	void x86_64CodeGenerator::Context::Add(register_t dst, int32 value, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("add\t{}, {}", registers[dst.id][bitmode], value);
	}
	void x86_64CodeGenerator::Context::Add(register_t dst, register_t src, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("add\t{}, {}", registers[dst.id][bitmode], registers[src.id][bitmode]);
	}
	void x86_64CodeGenerator::Context::Add(register_t dst, char const* mem, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("add\t{}, {} {}", registers[dst.id][bitmode], ConvertToCast(bitmode), mem);
	}
	void x86_64CodeGenerator::Context::Add(char const* mem, register_t src, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("add\t{} {}, {}", ConvertToCast(bitmode), mem, registers[src.id][bitmode]);
	}
	void x86_64CodeGenerator::Context::Add(char const* mem, int64 value, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("add\t{}, {}", mem, value);
	}

	void x86_64CodeGenerator::Context::Sub(register_t dst, int32 value, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("sub\t{}, {}", registers[dst.id][bitmode], value);
	}
	void x86_64CodeGenerator::Context::Sub(register_t dst, register_t src, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("sub\t{}, {}", registers[dst.id][bitmode], registers[src.id][bitmode]);
	}
	void x86_64CodeGenerator::Context::Sub(register_t dst, char const* mem, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("sub\t{}, {}", registers[dst.id][bitmode], mem);
	}
	void x86_64CodeGenerator::Context::Sub(char const* mem, register_t src, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("sub\t{}, {}", mem, registers[src.id][bitmode]);
	}
	void x86_64CodeGenerator::Context::Sub(char const* mem, int64 value, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("sub\t{}, {}", mem, value);
	}

	void x86_64CodeGenerator::Context::Imul(register_t dst, register_t src, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("imul\t{}, {}", registers[dst.id][bitmode], registers[src.id][bitmode]);
	}
	void x86_64CodeGenerator::Context::Imul(register_t dst, char const* mem, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("imul\t{}, {} {}", registers[dst.id][bitmode],ConvertToCast(bitmode), mem);
	}
	void x86_64CodeGenerator::Context::Imul(register_t dst, register_t src, int32 value, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("imul\t{}, {}, {}", registers[dst.id][bitmode], registers[src.id][bitmode], value);
	}
	void x86_64CodeGenerator::Context::Imul(register_t dst, char const* mem, int32 value, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("imul\t{}, {} {}, {}", registers[dst.id][bitmode], ConvertToCast(bitmode), mem, value);
	}

	void x86_64CodeGenerator::Context::Neg(register_t reg, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("neg\t{}, {}", registers[reg.id][bitmode]);
	}
	void x86_64CodeGenerator::Context::Neg(char const* mem)
	{
		Emit<Text>("neg\t{}, {}", mem);
	}
	void x86_64CodeGenerator::Context::Inc(char const* mem, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("inc\t{}", mem);
	}
	void x86_64CodeGenerator::Context::Inc(register_t reg, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("inc\t{}", registers[reg.id][bitmode]);
	}
	void x86_64CodeGenerator::Context::Dec(char const* mem, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("dec\t{}", mem);
	}
	void x86_64CodeGenerator::Context::Dec(register_t reg, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("dec\t{}", registers[reg.id][bitmode]);
	}

	//transfer
	void x86_64CodeGenerator::Context::Mov(register_t reg, int64 value, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("mov\t{}, {}", registers[reg.id][bitmode], value);
	}
	void x86_64CodeGenerator::Context::Mov(char const* mem, int32 value, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("mov\t{} {}, {}", ConvertToCast(bitmode), mem, value);
	}
	void x86_64CodeGenerator::Context::Mov(mem_ref_t const& mem_ref, int32 value, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("mov\t{}, {}", ConvertMemRef(mem_ref, bitmode), value);
	}
	void x86_64CodeGenerator::Context::Mov(register_t dst, register_t src, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("mov\t{}, {}", registers[dst.id][bitmode], registers[src.id][bitmode]);
	}
	void x86_64CodeGenerator::Context::Mov(register_t dst, char const* mem, BitMode bitmode /*= BitMode_64*/, bool address /*= false*/)
	{
		if(address)  Emit<Text>("mov\t{}, offset {}", registers[dst.id][BitMode_64], mem);
		else		 Emit<Text>("mov\t{}, {} {}", registers[dst.id][bitmode], ConvertToCast(bitmode), mem);
	}
	void x86_64CodeGenerator::Context::Mov(register_t dst, mem_ref_t const& mem_ref, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("mov\t{}, {}", registers[dst.id][bitmode], ConvertMemRef(mem_ref, bitmode));
	}
	void x86_64CodeGenerator::Context::Mov(char const* mem, register_t src, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("mov\t{} {}, {}", ConvertToCast(bitmode), mem, registers[src.id][bitmode]);
	}
	void x86_64CodeGenerator::Context::Mov(mem_ref_t const& mem_ref, register_t src, BitMode bitmode /*= BitMode_64*/)
	{
		Emit<Text>("mov\t{}, {}", ConvertMemRef(mem_ref, bitmode), registers[src.id][bitmode]);
	}
	void x86_64CodeGenerator::Context::Lea(register_t reg, char const* mem)
	{
		Emit<Text>("lea\t{}, {}", registers[reg.id][BitMode_64], mem);
	}
	void x86_64CodeGenerator::Context::Lea(register_t reg, mem_ref_t const& mem_ref)
	{
		Emit<Text>("lea\t{}, {}", registers[reg.id][BitMode_64], ConvertMemRef(mem_ref, BitMode_64));
	}

	//control
	void x86_64CodeGenerator::Context::Label(char const* label)
	{
		Emit<Text>("{}{}: ", label, label_id);
	}
	void x86_64CodeGenerator::Context::GenerateLabelId()
	{
		label_id = GenerateUniqueInteger();
	}
	void x86_64CodeGenerator::Context::Cmp(register_t reg, int64 value, BitMode bitmode)
	{
		Emit<Text>("cmp\t{}, {}", registers[reg.id][bitmode], value);
	}
	void x86_64CodeGenerator::Context::Cmp(char const* mem, int64 value, BitMode bitmode)
	{
		Emit<Text>("cmp\t{} {}, {}",ConvertToCast(bitmode), mem, value);
	}
	void x86_64CodeGenerator::Context::Cmp(register_t reg1, register_t reg2, BitMode bitmode)
	{

	}
	void x86_64CodeGenerator::Context::Cmp(char const* mem, register_t reg2, BitMode bitmode)
	{

	}
	void x86_64CodeGenerator::Context::Cmp(register_t reg1, char const* mem, BitMode bitmode)
	{

	}

	void x86_64CodeGenerator::Context::Set(register_t reg, Condition cond)
	{
		char const* reg_name = registers[reg.id][BitMode_8];
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
	void x86_64CodeGenerator::Context::Set(char const* mem, Condition cond)
	{
		switch (cond)
		{
		case Condition::Unconditional: LU_ASSERT(false); break;
		case Condition::Equal:		   Emit<Text>("sete\t{}", mem);  break;
		case Condition::NotEqual:	   Emit<Text>("setne\t{}", mem);  break;
		case Condition::Greater:	   Emit<Text>("setg\t{}", mem);  break;
		case Condition::GreaterEqual:  Emit<Text>("setge\t{}", mem);  break;
		case Condition::Less:		   Emit<Text>("setl\t{}", mem);  break;
		case Condition::LessEqual:	   Emit<Text>("setle\t{}", mem);  break;
		}
	}
	void x86_64CodeGenerator::Context::Jmp(char const* label, Condition cond /*= Condition::Unconditional*/)
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

	//declarations
	void x86_64CodeGenerator::Context::DeclareVariable(char const* sym_name, bool is_static, BitMode bitmode)
	{
		if (!is_static) Emit<None>("public {}", sym_name);
		Emit<Data>("{}\t{} ?", sym_name, ConvertToType(bitmode));
	}
	void x86_64CodeGenerator::Context::DeclareArray(char const* sym_name, size_t size, bool is_static, BitMode bitmode)
	{
		if (!is_static) Emit<None>("public {}", sym_name);
		Emit<Data>("{}\t{} {} dup (?)", sym_name, ConvertToType(bitmode), size);
	}
	void x86_64CodeGenerator::Context::DeclareExternVariable(char const* sym_name, BitMode bitmode)
	{
		Emit<None>("extern {} : {}", sym_name, ConvertToType(bitmode));
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

	//functions
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

	//helpers
	template<x86_64CodeGenerator::Context::SegmentType segment, typename... Ts>
	void x86_64CodeGenerator::Context::Emit(std::string_view fmt, Ts&&... args)
	{
		std::string output = std::vformat(fmt, std::make_format_args(std::forward<Ts>(args)...));
		output += "\n";
		if		constexpr (segment == x86_64CodeGenerator::Context::SegmentType::None)	output_buffer.preamble += output;
		else if constexpr (segment == x86_64CodeGenerator::Context::SegmentType::Data)	 output_buffer.data_segment += output;
		else if constexpr (segment == x86_64CodeGenerator::Context::SegmentType::Text)	 output_buffer.text_segment += output;
	}
	size_t x86_64CodeGenerator::Context::GenerateUniqueInteger()
	{
		static size_t i = 0;
		return ++i;
	}
	std::string x86_64CodeGenerator::Context::ConvertMemRef(mem_ref_t const& args, BitMode mode)
	{
		std::string indirect_result = "[";
		if (args.base_reg != INVALID_REG)
		{
			indirect_result += registers[args.base_reg.id][mode];
		}
		if (args.index_reg != INVALID_REG)
		{
			if (!indirect_result.empty()) indirect_result += "+";
			indirect_result += registers[args.base_reg.id][mode];

			if (args.scale != mem_ref_t::Scale_None)
			{
				indirect_result += "*";
				switch (args.scale)
				{
				case mem_ref_t::Scale_x1: indirect_result += "1"; break;
				case mem_ref_t::Scale_x2: indirect_result += "2"; break;
				case mem_ref_t::Scale_x4: indirect_result += "4"; break;
				case mem_ref_t::Scale_x8: indirect_result += "8"; break;
				}
			}
		}
		if (args.displacement)
		{
			if (!indirect_result.empty()) indirect_result += "+";
			indirect_result += std::to_string(args.displacement);
		}
		indirect_result += "]";
		return indirect_result;
	}
	std::string x86_64CodeGenerator::Context::ConvertToType(BitMode mode)
	{
		switch (mode)
		{
		case BitMode_8:  return "byte";
		case BitMode_16: return "word";
		case BitMode_32: return "dword";
		case BitMode_64: return "qword";
		}
		return "";
	}
	std::string x86_64CodeGenerator::Context::ConvertToCast(BitMode mode)
	{
		return ConvertToType(mode) + " ptr";
	}

}

/*
Various combinations of the four (including all four) are valid. Here are the valid combinations, in roughly increasing order of complexity:
Displacement
Base
Base + Index
Base + Displacement
Base + Index + Displacement
Base + (Index * Scale)
(Index * Scale) + Displacement
Base + (Index * Scale) + Displacement
----------------------------------------------------------------------------------------------------------------------------------------------
1. Displacement
This is arguably the simplest addressing mechanism in the x86 family: the displacement field is treated as an absolute memory address.
	--------------------------------------------------------------------------------
	; store the qword at 0x00000000000000ff into rax
	mov rax, [0xff]

	--------------------------------------------------------------------------------
	extern long var;				|				f: mov     rax, rdi
									|----------->      movabs  QWORD PTR [var], rax
	void f(long x) { var = x; }		|				   ret

2. Base
Addressing via the base register adds one layer of indirection over absolute addressing: instead of an absolute address encoded into the 
instruction’s displacement field, an address is loaded from the specified general-purpose register.
	--------------------------------------------------------------------------------
	;store the immediate (not displacement) into rbx
	mov rbx, 0xacabacabacabacab
	;store the qword at the address stored in rbx into rcx
	mov rcx, [rbx]
	--------------------------------------------------------------------------------

3. Base + Index
This is just like addressing via the base register, except that we also add in the value of the index register.
	--------------------------------------------------------------------------------
	; store the qword in rcx into the memory address computed as the sum of the values in rax and rbx
	mov [rax + rbx], rcx

	--------------------------------------------------------------------------------
	int foo(char * buf, int index) { return buf[index]; } |-------> movsx   eax, byte ptr [rax + rcx] ; store buf[index] into eax

4. Base + Displacement
	--------------------------------------------------------------------------------
	;add 0xcafe to the value stored in rax, then store the qword at the computed address into rbx
	mov rbx, [rax + 0xcafe]

5. Base + Index + Displacement
	--------------------------------------------------------------------------------
	; add 0xcafe to the values stores in rax and rcx, then store the qword at the computer address into rbx
	mov rbx, [rax + rcx + 0xcafe]

6. Base + (Index * Scale)
As the name implies, the scale field is used to scale (i.e., multiply) another field. In particular, it always scales the index register — scale cannot be used without index.

	mov     rax, qword ptr [rdi + 8*rsi] ;

*/

