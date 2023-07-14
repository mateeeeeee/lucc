#include <format>
#include "x86_64Context.h"
#include "Diagnostics/Diagnostics.h"

namespace lucc
{
	namespace
	{
		uint64 GenerateUniqueInteger()
		{
			static uint64 i = 0;
			return i++;
		}
	}

	template<x86_64CodeGenerator::SegmentType segment, typename... Ts>
	void x86_64CodeGenerator::Context::Emit(std::string_view fmt, Ts&&... args)
	{
		std::string output = std::vformat(fmt, std::make_format_args(std::forward<Ts>(args)...));
		output += "\n";
		if		constexpr (segment == x86_64CodeGenerator::SegmentType::None)	 output_buffer.no_segment += output;
		else if constexpr (segment == x86_64CodeGenerator::SegmentType::BSS)	 output_buffer.bss_segment += output;
		else if constexpr (segment == x86_64CodeGenerator::SegmentType::Const)	 output_buffer.rodata_segment += output;
		else if constexpr (segment == x86_64CodeGenerator::SegmentType::Data)	 output_buffer.data_segment += output;
		else if constexpr (segment == x86_64CodeGenerator::SegmentType::Text)	 output_buffer.text_segment += output;
	}

	std::string x86_64CodeGenerator::Context::ConvertOperand(OperandRef op, BitCount bitcount)
	{
		switch (op.form)
		{
		case OperandForm::Immediate: return std::format("{}", op.immediate);
		case OperandForm::Register:  return GetRegisterName(op.reg, bitcount);
		case OperandForm::Global:    return std::format("{} {}", GetWordCast(bitcount), op.global);
		case OperandForm::SIB:
		{
			bitcount = BitCount_64; //for now
			std::string indirect_result = "[";
			if (op.sib.base_reg != InvalidRegister)
			{
				indirect_result += GetRegisterName(op.sib.base_reg, bitcount);
			}
			if (op.sib.index_reg != InvalidRegister)
			{
				if (!indirect_result.empty()) indirect_result += "+";
				indirect_result += GetRegisterName(op.sib.index_reg, bitcount);

				if (op.sib.scale != SIBScale_None)
				{
					indirect_result += "*";
					switch (op.sib.scale)
					{
					case SIBScale_x1: indirect_result += "1"; break;
					case SIBScale_x2: indirect_result += "2"; break;
					case SIBScale_x4: indirect_result += "4"; break;
					case SIBScale_x8: indirect_result += "8"; break;
					}
				}
			}
			if (op.sib.displacement)
			{
				if (!indirect_result.empty() && op.sib.displacement > 0) indirect_result += "+";
				indirect_result += std::to_string(op.sib.displacement);
			}
			indirect_result += "]";
			return std::format("{} {}", GetWordCast(bitcount), indirect_result);
		}
		}
		return "";
	}

	x86_64CodeGenerator::Context::Context(OutputBuffer& output_buffer) : output_buffer(output_buffer)
	{
		Emit<BSS>(".data?");
		Emit<Const>(".const");
		Emit<Data>(".data");
		Emit<Text>(".code");
		register_mask.fill(true);
	}

	Register x86_64CodeGenerator::Context::AllocateRegister()
	{
		for (int32 i = 0; i < std::size(scratch_registers); i++)
		{
			Register reg = scratch_registers[i];
			if (register_mask[reg])
			{
				register_mask[reg] = false;
				return reg;
			}
		}
		Report(diag::out_of_registers);
	}

	void x86_64CodeGenerator::Context::FreeRegister(Register reg)
	{
		LU_ASSERT(!register_mask[reg]);
		register_mask[reg] = true;
	}

	Register x86_64CodeGenerator::Context::GetCallRegister(uint32 arg_index) const
	{
		if (arg_index >= ARGUMENTS_PASSED_BY_REGISTERS) Report(diag::out_of_func_arg_registers);
		static Register call_registers[ARGUMENTS_PASSED_BY_REGISTERS] = { RCX, RDX, R8, R9 };
		return call_registers[arg_index];
	}

	void x86_64CodeGenerator::Context::Add(OperandRef lhs, OperandRef rhs, BitCount bitcount)
	{
		LU_ASSERT(lhs.form != OperandForm::Immediate);
		Emit<Text>("add\t{}, {}", ConvertOperand(lhs, bitcount), ConvertOperand(rhs, bitcount));
	}

	void x86_64CodeGenerator::Context::Sub(OperandRef lhs, OperandRef rhs, BitCount bitcount)
	{
		LU_ASSERT(lhs.form != OperandForm::Immediate);
		Emit<Text>("sub\t{}, {}", ConvertOperand(lhs, bitcount), ConvertOperand(rhs, bitcount));
	}

	void x86_64CodeGenerator::Context::Imul(Register lhs, OperandRef rhs, BitCount bitcount)
	{
		LU_ASSERT(rhs.form != OperandForm::Immediate);
		Emit<Text>("imul\t{}, {}", GetRegisterName(lhs, bitcount), ConvertOperand(rhs, bitcount));
	}

	void x86_64CodeGenerator::Context::Imul(Register lhs, OperandRef rhs, int32 imm, BitCount bitcount)
	{
		LU_ASSERT(rhs.form != OperandForm::Immediate);
		Emit<Text>("imul\t{}, {}, {}", GetRegisterName(lhs, bitcount), ConvertOperand(rhs, bitcount), imm);
	}

	void x86_64CodeGenerator::Context::Idiv(Register lhs, OperandRef divisor, BitCount bitcount)
	{
		LU_ASSERT(divisor.form != OperandForm::Immediate);

	}

	void x86_64CodeGenerator::Context::Neg(OperandRef op, BitCount bitcount)
	{
		LU_ASSERT(op.form != OperandForm::Immediate);
		Emit<Text>("neg\t{}", ConvertOperand(op, bitcount));
	}

	void x86_64CodeGenerator::Context::Inc(OperandRef op, BitCount bitcount)
	{
		LU_ASSERT(op.form != OperandForm::Immediate);
		Emit<Text>("inc\t{} {}", ConvertOperand(op, bitcount));
	}

	void x86_64CodeGenerator::Context::Dec(OperandRef op, BitCount bitcount)
	{
		LU_ASSERT(op.form != OperandForm::Immediate);
		Emit<Text>("dec\t{} {}", ConvertOperand(op, bitcount));
	}

	void x86_64CodeGenerator::Context::And(OperandRef lhs, OperandRef rhs, BitCount bitcount)
	{
		LU_ASSERT(lhs.form != OperandForm::Immediate);
		Emit<Text>("and\t{}, {}", ConvertOperand(lhs, bitcount), ConvertOperand(rhs, bitcount));
	}

	void x86_64CodeGenerator::Context::Or(OperandRef lhs, OperandRef rhs, BitCount bitcount)
	{
		LU_ASSERT(lhs.form != OperandForm::Immediate);
		Emit<Text>("or\t{}, {}", ConvertOperand(lhs, bitcount), ConvertOperand(rhs, bitcount));
	}

	void x86_64CodeGenerator::Context::Xor(OperandRef lhs, OperandRef rhs, BitCount bitcount)
	{
		LU_ASSERT(lhs.form != OperandForm::Immediate);
		Emit<Text>("xor\t{}, {}", ConvertOperand(lhs, bitcount), ConvertOperand(rhs, bitcount));
	}

	void x86_64CodeGenerator::Context::Not(OperandRef op, BitCount bitcount)
	{
		LU_ASSERT(op.form != OperandForm::Immediate);
		Emit<Text>("not\t{}", ConvertOperand(op, bitcount));
	}

	void x86_64CodeGenerator::Context::Push(OperandRef op)
	{
		Emit<Text>("push\t{}", ConvertOperand(op, BitCount_64));
	}

	void x86_64CodeGenerator::Context::Pop(OperandRef op)
	{
		Emit<Text>("pop\t{}", ConvertOperand(op, BitCount_64));
	}

	void x86_64CodeGenerator::Context::Label(char const* label)
	{
		Emit<Text>("{}: ", label);
	}

	void x86_64CodeGenerator::Context::Label(char const* label, uint64 label_id)
	{
		Emit<Text>("{}.{}: ", label, label_id);
	}

	uint64 x86_64CodeGenerator::Context::GenerateLabelId()
	{
		return GenerateUniqueInteger();
	}

	void x86_64CodeGenerator::Context::Cmp(OperandRef lhs, OperandRef rhs, BitCount bitcount)
	{
		Emit<Text>("cmp\t{}, {}", ConvertOperand(lhs, bitcount), ConvertOperand(rhs, bitcount));
	}

	void x86_64CodeGenerator::Context::Set(OperandRef op, ConditionCode cc)
	{
		std::string op_name = ConvertOperand(op, BitCount_8);
		switch (cc)
		{
		case ConditionCode::E:  Emit<Text>("sete {}", op_name);  break;
		case ConditionCode::NE: Emit<Text>("setne {}", op_name);  break;
		case ConditionCode::B:  Emit<Text>("setb {}", op_name);  break;
		case ConditionCode::BE: Emit<Text>("setbe {}", op_name);  break;
		case ConditionCode::A:  Emit<Text>("seta {}", op_name);  break;
		case ConditionCode::AE: Emit<Text>("setae {}", op_name);  break;
		case ConditionCode::L:  Emit<Text>("setl {}", op_name);  break;
		case ConditionCode::LE: Emit<Text>("setle {}", op_name);  break;
		case ConditionCode::G:  Emit<Text>("setg {}", op_name);  break;
		case ConditionCode::GE: Emit<Text>("setge {}", op_name);  break;
		case ConditionCode::Z:  Emit<Text>("setz {}", op_name);  break;
		case ConditionCode::NZ: Emit<Text>("setnz {}", op_name);  break;
		case ConditionCode::S:  Emit<Text>("sets {}", op_name);  break;
		case ConditionCode::NS: Emit<Text>("setns {}", op_name);  break;
		case ConditionCode::None:
		default:
			LU_ASSERT(false);
		}
	}

	void x86_64CodeGenerator::Context::Jmp(char const* label, ConditionCode cc)
	{
		switch (cc)
		{
		case ConditionCode::E:    Emit<Text>("je {}", label);  break;
		case ConditionCode::NE:   Emit<Text>("jne {}", label);  break;
		case ConditionCode::B:    Emit<Text>("jb {}", label);  break;
		case ConditionCode::BE:   Emit<Text>("jbe {}", label);  break;
		case ConditionCode::A:    Emit<Text>("ja {}", label);  break;
		case ConditionCode::AE:   Emit<Text>("jae {}", label);  break;
		case ConditionCode::L:    Emit<Text>("jl {}", label);  break;
		case ConditionCode::LE:   Emit<Text>("jle {}", label);  break;
		case ConditionCode::G:    Emit<Text>("jg {}", label);  break;
		case ConditionCode::GE:   Emit<Text>("jge {}", label);  break;
		case ConditionCode::Z:    Emit<Text>("jz {}", label);  break;
		case ConditionCode::NZ:   Emit<Text>("jnz {}", label);  break;
		case ConditionCode::S:    Emit<Text>("js {}", label);  break;
		case ConditionCode::NS:   Emit<Text>("jns {}", label);  break;
		case ConditionCode::None: Emit<Text>("jmp {}", label);  break;
		default:
			LU_ASSERT(false);
		}
	}

	void x86_64CodeGenerator::Context::Jmp(char const* label, uint64 label_id, ConditionCode cc)
	{
		std::string label_ = std::format("{}.{}", label, label_id);
		Jmp(label_.c_str(), cc);
	}

	void x86_64CodeGenerator::Context::Mov(OperandRef lhs, OperandRef rhs, BitCount bitcount)
	{
		LU_ASSERT(lhs.form != OperandForm::Immediate);
		Emit<Text>("mov\t{}, {}", ConvertOperand(lhs, bitcount), ConvertOperand(rhs, bitcount));
	}

	void x86_64CodeGenerator::Context::MovOffset(Register lhs, char const* global)
	{
		Emit<Text>("mov\t{}, offset {}", GetRegisterName(lhs, BitCount_64), global);
	}

	void x86_64CodeGenerator::Context::Movabs(Register lhs, int64 value)
	{
		Emit<Text>("mov\t{}, {}", GetRegisterName(lhs, BitCount_64), value);
	}

	void x86_64CodeGenerator::Context::Movzx(Register lhs, OperandRef rhs, BitCount bitcount, bool rhs8bit /*= false*/)
	{
		LU_ASSERT(rhs.form != OperandForm::Immediate);
		Emit<Text>("movzx\t{}, {}", GetRegisterName(lhs, bitcount), ConvertOperand(rhs, rhs8bit ? BitCount_8 : BitCount_16));
	}

	void x86_64CodeGenerator::Context::Movsx(Register lhs, OperandRef rhs, BitCount bitcount, bool rhs8bit /*= false*/)
	{
		LU_ASSERT(rhs.form != OperandForm::Immediate);
		Emit<Text>("movsx\t{}, {}", GetRegisterName(lhs, bitcount), ConvertOperand(rhs, rhs8bit ? BitCount_8 : BitCount_16));
	}

	void x86_64CodeGenerator::Context::Movsxd(Register lhs, OperandRef rhs)
	{
		LU_ASSERT(rhs.form != OperandForm::Immediate);
		Emit<Text>("movsx\t{}, {}", GetRegisterName(lhs, BitCount_64), ConvertOperand(rhs, BitCount_32));
	}

	void x86_64CodeGenerator::Context::Lea(Register reg, OperandRef op)
	{
		LU_ASSERT(op.form != OperandForm::Immediate && op.form != OperandForm::Register);
		Emit<Text>("lea\t{}, {}", GetRegisterName(reg, BitCount_64), ConvertOperand(op, BitCount_64));
	}

	void x86_64CodeGenerator::Context::DeclareVariable(VarDeclCG const& var_decl)
	{
		if (var_decl.is_extern)
		{
			Emit<None>("extern {} : {}", var_decl.name, GetWordType(var_decl.bits));
			return;
		}

		if (!var_decl.is_static) Emit<None>("public {}", var_decl.name);
		if (var_decl.init_value)
		{
			if(var_decl.is_const) Emit<Const>("{}\t{} {}", var_decl.name, GetWordType(var_decl.bits), *var_decl.init_value);
			else Emit<Data>("{}\t{} {}", var_decl.name, GetWordType(var_decl.bits), *var_decl.init_value);
		}
		else
		{
			Emit<BSS>("{}\t{} ?", var_decl.name, GetWordType(var_decl.bits));
		}
	}

	void x86_64CodeGenerator::Context::DeclareArray(ArrayDeclCG const& array_decl)
	{
		if (array_decl.is_extern)
		{
			Emit<None>("extern {} : {}", array_decl.name, GetWordType(array_decl.bits));
			return;
		}

		if (!array_decl.is_static) Emit<None>("public {}", array_decl.name);
		if (array_decl.init_values)
		{
			
		}
		else
		{
			Emit<BSS>("{}\t{} dup (?)", array_decl.name, GetWordType(array_decl.bits));
		}
	}

	void x86_64CodeGenerator::Context::DeclareFunction(FunctionDeclCG const& func_decl)
	{
		if (func_decl.is_extern)
		{
			Emit<None>("extern {} : proc", func_decl.name);
			return;
		}
		current_function = func_decl.name;
		Emit<Text>("\n{} proc {}", func_decl.name, func_decl.is_static ? "private" : "");
	}

	void x86_64CodeGenerator::Context::Call(char const* func_name)
	{
		Emit<Text>("call {}", func_name);
	}

	void x86_64CodeGenerator::Context::ReserveStack(uint32 stack)
	{
		Emit<Text>("push rbp");
		Emit<Text>("mov rbp, rsp");
		if (stack)
		{
			Emit<Text>("sub rsp, {}", stack);
			stack_used = stack;
		}
		stack_reg_saved = true;
	}

	void x86_64CodeGenerator::Context::JumpToReturn()
	{
		Emit<Text>("jmp {}_end", current_function);
	}

	void x86_64CodeGenerator::Context::Return()
	{
		if (current_function == "main") Emit<Text>("xor rax, rax");

		Emit<Text>("{}_end:", current_function);
		if (stack_reg_saved)
		{
			if (stack_used) Emit<Text>("add rsp, {}", stack_used);
			Emit<Text>("pop rbp");
			stack_reg_saved = false;
			stack_used = 0;
		}
		Emit<Text>("ret");
		Emit<Text>("{} endp", current_function);
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

