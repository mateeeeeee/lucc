#include <format>
#include "x86_64.h"
#include "x86_64Context.h"

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

	template<SegmentType segment, typename... Ts>
	void x86_64Context::Emit(std::string_view fmt, Ts&&... args)
	{
		std::string output = std::vformat(fmt, std::make_format_args(std::forward<Ts>(args)...));
		output += "\n";
		if		constexpr (segment == SegmentType::None)	 output_buffer.no_segment += output;
		else if constexpr (segment == SegmentType::BSS)		 output_buffer.bss_segment += output;
		else if constexpr (segment == SegmentType::Const)	 output_buffer.rodata_segment += output;
		else if constexpr (segment == SegmentType::Data)	 output_buffer.data_segment += output;
		else if constexpr (segment == SegmentType::Text)	 output_buffer.text_segment += output;
	}

	std::string x86_64Context::ConvertOperand(ResultRef op, BitCount bitcount)
	{
		switch (op.kind)
		{
		case ResultKind::Immediate: return std::format("{}", op.immediate);
		case ResultKind::Register:  return GetRegisterName(op.reg, bitcount);
		case ResultKind::Global:    return std::format("{} {}", GetWordCast(bitcount), op.global);
		case ResultKind::SIB:
		{
			std::string indirect_result = "[";
			if (op.sib.base_reg != InvalidRegister)
			{
				indirect_result += GetRegisterName(op.sib.base_reg, BitCount_64);
			}
			if (op.sib.index_reg != InvalidRegister)
			{
				if (!indirect_result.empty()) indirect_result += "+";
				indirect_result += GetRegisterName(op.sib.index_reg, BitCount_64);

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

	x86_64Context::x86_64Context(OutputBuffer& output_buffer) : output_buffer(output_buffer)
	{
		Emit<None>("extern ExitProcess: proc");
		Emit<BSS>(".data?");
		Emit<Const>(".const");
		Emit<Data>(".data");
		Emit<Text>(".code");
		registers_available.fill(true);
		registers_pushed.fill(false);
	}

	Register x86_64Context::AllocateRegister()
	{
		static Register const scratch_registers[] = { RBX, R10, R11, R12, R13, R14, R15 };
		for (int32 i = 0; i < std::size(scratch_registers); i++)
		{
			Register reg = scratch_registers[i];
			if (registers_available[reg])
			{
				registers_available[reg] = false;
				return reg;
			}
		}
		last_spilled_reg_index = (last_spilled_reg_index) % std::size(scratch_registers);
		Register reg = scratch_registers[last_spilled_reg_index++];
		spilled_scratched_registers.push_back(reg);
		Push(reg);
		return reg;
	}

	void x86_64Context::FreeRegister(Register reg)
	{
		LU_ASSERT(!registers_available[reg]);
		bool still_spilled = false;
		for (auto spilled_reg : spilled_scratched_registers)  if (spilled_reg == reg) { still_spilled = true; break; }
		if (!still_spilled) registers_available[reg] = true;
		if (!spilled_scratched_registers.empty() && reg == spilled_scratched_registers.back())
		{
			last_spilled_reg_index = (last_spilled_reg_index - 1) % 7;

			spilled_scratched_registers.pop_back();
			Pop(reg);
		}
	}

	void x86_64Context::FreeRegister(ResultRef res)
	{
		FreeRegister(res.reg);
	}

	uint32 x86_64Context::SaveVolatileRegisters()
	{
		static Register const volatile_registers[] = { RAX, RBX, RCX, RDX, R8, R9, R10, R11 };

		uint32 pushed_count = 0;
		for (int32 i = 0; i < std::size(volatile_registers); i++)
		{
			Register reg = volatile_registers[i];
			if (!registers_available[reg])
			{
				registers_pushed[reg] = true;
				pushed_count++;
				Push(reg);
			}
		}
		return pushed_count;
	}

	void x86_64Context::RestoreVolatileRegisters()
	{
		static Register const volatile_registers[] = { RAX, RBX, RCX, RDX, R8, R9, R10, R11 };
		for (int32 i = (int32)std::size(volatile_registers) - 1; i >= 0; i--)
		{
			Register reg = volatile_registers[i];
			if (registers_pushed[i])
			{
				Pop(reg);
				registers_pushed[reg] = false;
			}
		}
	}

	Register x86_64Context::GetCallRegister(uint32 arg_index)
	{
		static Register call_registers[ARGUMENTS_PASSED_BY_REGISTERS] = { RCX, RDX, R8, R9 };
		if (arg_index >= ARGUMENTS_PASSED_BY_REGISTERS || !registers_available[call_registers[arg_index]])
		{
			LU_ASSERT(false);
			return InvalidRegister;
		}
		registers_available[call_registers[arg_index]] = false;
		return call_registers[arg_index];
	}

	Register x86_64Context::GetReturnRegister()
	{
		static Register return_register = RAX;
		if (!registers_available[return_register])
		{
			LU_ASSERT(false);
			return InvalidRegister;
		}
		registers_available[return_register] = false;
		return return_register;
	}

	void x86_64Context::Add(ResultRef lhs, ResultRef rhs, BitCount bitcount)
	{
		LU_ASSERT(lhs.kind != ResultKind::Immediate);
		Emit<Text>("add\t{}, {}", ConvertOperand(lhs, bitcount), ConvertOperand(rhs, bitcount));
	}

	void x86_64Context::Sub(ResultRef lhs, ResultRef rhs, BitCount bitcount)
	{
		LU_ASSERT(lhs.kind != ResultKind::Immediate);
		Emit<Text>("sub\t{}, {}", ConvertOperand(lhs, bitcount), ConvertOperand(rhs, bitcount));
	}

	void x86_64Context::Imul(Register lhs, ResultRef rhs, BitCount bitcount)
	{
		LU_ASSERT(rhs.kind != ResultKind::Immediate);
		Emit<Text>("imul\t{}, {}", GetRegisterName(lhs, bitcount), ConvertOperand(rhs, bitcount));
	}

	void x86_64Context::Imul(Register lhs, ResultRef rhs, int32 imm, BitCount bitcount)
	{
		LU_ASSERT(rhs.kind != ResultKind::Immediate);
		Emit<Text>("imul\t{}, {}, {}", GetRegisterName(lhs, bitcount), ConvertOperand(rhs, bitcount), imm);
	}

	void x86_64Context::Idiv(Register dividend, ResultRef divisor, BitCount bitcount)
	{
		//LU_ASSERT(divisor.kind != ResultKind::Immediate);
		Xor(RDX, RDX, BitCount_64);
		static Register const dividend_register = RAX;
		if (dividend != dividend_register) Mov(dividend_register, dividend, bitcount);
		Emit<Text>("idiv\t{}", ConvertOperand(divisor, bitcount));
		if (dividend != dividend_register) Mov(dividend, dividend_register, bitcount);
	}

	void x86_64Context::Neg(ResultRef op, BitCount bitcount)
	{
		LU_ASSERT(op.kind != ResultKind::Immediate);
		Emit<Text>("neg\t{}", ConvertOperand(op, bitcount));
	}

	void x86_64Context::Inc(ResultRef op, BitCount bitcount)
	{
		LU_ASSERT(op.kind != ResultKind::Immediate);
		Emit<Text>("inc\t{}", ConvertOperand(op, bitcount));
	}

	void x86_64Context::Dec(ResultRef op, BitCount bitcount)
	{
		LU_ASSERT(op.kind != ResultKind::Immediate);
		Emit<Text>("dec\t{}", ConvertOperand(op, bitcount));
	}

	void x86_64Context::Shl(ResultRef lhs, ResultRef rhs, BitCount bitcount)
	{
		LU_ASSERT(rhs.kind != ResultKind::Global && rhs.kind != ResultKind::SIB);
		static Register const shift_register = RCX;
		if (rhs.kind == ResultKind::Immediate)
		{
			Emit<Text>("shl\t{}, {}", ConvertOperand(lhs, bitcount), rhs.immediate);
		}
		else
		{
			if (rhs.reg != shift_register) Mov(shift_register, rhs, BitCount_8);
			Emit<Text>("shl\t{}, {}", ConvertOperand(lhs, bitcount), GetRegisterName(shift_register, bitcount));
		}
	}

	void x86_64Context::Shr(ResultRef lhs, ResultRef rhs, BitCount bitcount)
	{
		LU_ASSERT(rhs.kind != ResultKind::Global && rhs.kind != ResultKind::SIB);
		static Register const shift_register = RCX;
		if (rhs.kind == ResultKind::Immediate)
		{
			Emit<Text>("shr\t{}, {}", ConvertOperand(lhs, bitcount), rhs.immediate);
		}
		else
		{
			if (rhs.reg != shift_register) Mov(shift_register, rhs, BitCount_8);
			Emit<Text>("shr\t{}, {}", ConvertOperand(lhs, bitcount), GetRegisterName(shift_register, bitcount));
		}
	}

	void x86_64Context::Sar(ResultRef lhs, ResultRef rhs, BitCount bitcount)
	{
		LU_ASSERT(rhs.kind != ResultKind::Global && rhs.kind != ResultKind::SIB);
		static Register const shift_register = RCX;
		if (rhs.kind == ResultKind::Immediate)
		{
			Emit<Text>("sar\t{}, {}", ConvertOperand(lhs, bitcount), rhs.immediate);
		}
		else
		{
			if (rhs.reg != shift_register) Mov(shift_register, rhs, BitCount_8);
			Emit<Text>("sar\t{}, {}", ConvertOperand(lhs, bitcount), GetRegisterName(shift_register, bitcount));
		}
	}

	void x86_64Context::And(ResultRef lhs, ResultRef rhs, BitCount bitcount)
	{
		LU_ASSERT(lhs.kind != ResultKind::Immediate);
		Emit<Text>("and\t{}, {}", ConvertOperand(lhs, bitcount), ConvertOperand(rhs, bitcount));
	}

	void x86_64Context::Or(ResultRef lhs, ResultRef rhs, BitCount bitcount)
	{
		LU_ASSERT(lhs.kind != ResultKind::Immediate);
		Emit<Text>("or\t{}, {}", ConvertOperand(lhs, bitcount), ConvertOperand(rhs, bitcount));
	}

	void x86_64Context::Xor(ResultRef lhs, ResultRef rhs, BitCount bitcount)
	{
		LU_ASSERT(lhs.kind != ResultKind::Immediate);
		Emit<Text>("xor\t{}, {}", ConvertOperand(lhs, bitcount), ConvertOperand(rhs, bitcount));
	}

	void x86_64Context::Not(ResultRef op, BitCount bitcount)
	{
		LU_ASSERT(op.kind != ResultKind::Immediate);
		Emit<Text>("not\t{}", ConvertOperand(op, bitcount));
	}

	void x86_64Context::Push(ResultRef op)
	{
		Emit<Text>("push\t{}", ConvertOperand(op, BitCount_64));
		stack_allocated += 8;
	}

	void x86_64Context::Pop(ResultRef op)
	{
		Emit<Text>("pop\t{}", ConvertOperand(op, BitCount_64));
		stack_allocated -= 8;
	}

	void x86_64Context::Label(char const* label)
	{
		Emit<Text>("{}: ", label);
	}

	void x86_64Context::Label(char const* label, uint64 label_id)
	{
		Emit<Text>("{}_{}: ", label, label_id);
	}

	uint64 x86_64Context::GenerateLabelId()
	{
		return GenerateUniqueInteger();
	}

	void x86_64Context::Cmp(ResultRef lhs, ResultRef rhs, BitCount bitcount)
	{
		Emit<Text>("cmp\t{}, {}", ConvertOperand(lhs, bitcount), ConvertOperand(rhs, bitcount));
	}

	void x86_64Context::Set(ResultRef op, ConditionCode cc)
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

	void x86_64Context::Jmp(char const* label, ConditionCode cc)
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

	void x86_64Context::Jmp(char const* label, uint64 label_id, ConditionCode cc)
	{
		std::string label_ = std::format("{}_{}", label, label_id);
		Jmp(label_.c_str(), cc);
	}

	void x86_64Context::Mov(ResultRef lhs, ResultRef rhs, BitCount bitcount)
	{
		LU_ASSERT(lhs.kind != ResultKind::Immediate);
		Emit<Text>("mov\t{}, {}", ConvertOperand(lhs, bitcount), ConvertOperand(rhs, bitcount));
	}

	void x86_64Context::MovOffset(Register lhs, char const* global)
	{
		Emit<Text>("mov\t{}, offset {}", GetRegisterName(lhs, BitCount_64), global);
	}

	void x86_64Context::Movabs(Register lhs, int64 value)
	{
		Emit<Text>("mov\t{}, {}", GetRegisterName(lhs, BitCount_64), value);
	}

	void x86_64Context::Movzx(Register lhs, ResultRef rhs, BitCount bitcount, bool rhs8bit /*= false*/)
	{
		LU_ASSERT(rhs.kind != ResultKind::Immediate);
		Emit<Text>("movzx\t{}, {}", GetRegisterName(lhs, bitcount), ConvertOperand(rhs, rhs8bit ? BitCount_8 : BitCount_16));
	}

	void x86_64Context::Movsx(Register lhs, ResultRef rhs, BitCount bitcount, bool rhs8bit /*= false*/)
	{
		LU_ASSERT(rhs.kind != ResultKind::Immediate);
		Emit<Text>("movsx\t{}, {}", GetRegisterName(lhs, bitcount), ConvertOperand(rhs, rhs8bit ? BitCount_8 : BitCount_16));
	}

	void x86_64Context::Movsxd(Register lhs, ResultRef rhs)
	{
		LU_ASSERT(rhs.kind != ResultKind::Immediate);
		Emit<Text>("movsxd\t{}, {}", GetRegisterName(lhs, BitCount_64), ConvertOperand(rhs, BitCount_32));
	}

	void x86_64Context::Lea(Register reg, ResultRef op)
	{
		LU_ASSERT(op.kind != ResultKind::Immediate && op.kind != ResultKind::Register);
		Emit<Text>("lea\t{}, {}", GetRegisterName(reg, BitCount_64), ConvertOperand(op, BitCount_64));
	}

	void x86_64Context::DeclareVariable(VarDeclCG const& var_decl)
	{
		if (var_decl.is_extern)
		{
			Emit<None>("extern {} : {}", var_decl.name, GetWordType(var_decl.bits));
			return;
		}

		if (!var_decl.is_static) Emit<None>("public {}", var_decl.name);

		if (var_decl.init_value)
		{
			if (var_decl.is_const)
			{
				if(var_decl.align) Emit<Const>("align {}", var_decl.align);
				Emit<Const>("{}\t{} {}", var_decl.name, GetWordType(var_decl.bits), *var_decl.init_value);
			}
			else
			{
				if (var_decl.align) Emit<Data>("align {}", var_decl.align);
				Emit<Data>("{}\t{} {}", var_decl.name, GetWordType(var_decl.bits), *var_decl.init_value);
			}
		}
		else
		{
			if (var_decl.align) Emit<BSS>("align {}", var_decl.align);
			Emit<BSS>("{}\t{} ?", var_decl.name, GetWordType(var_decl.bits));
		}
	}

	std::string x86_64Context::DeclareString(char const* str)
	{
		static uint64 string_unique_id = 0;
		std::string str_label = std::format("L_str_{}", string_unique_id++);
		Emit<Const>("{} byte \"{}\",0", str_label, str);
		return str_label;
	}

	void x86_64Context::DeclareArray(ArrayDeclCG const& array_decl)
	{
		if (array_decl.is_extern)
		{
			Emit<None>("extern {} : {}", array_decl.name, GetWordType(array_decl.bits));
			return;
		}

		if (!array_decl.is_static) Emit<None>("public {}", array_decl.name);
		if (array_decl.init_values)
		{
			//#todo
		}
		else
		{
			if (array_decl.align) Emit<BSS>("align {}", array_decl.align);
			Emit<BSS>("{}\t{} {} dup (?)", array_decl.name, GetWordType(array_decl.bits), array_decl.array_size);
		}
	}

	void x86_64Context::DeclareFunction(FunctionDeclCG const& func_decl)
	{
		if (func_decl.is_extern)
		{
			Emit<None>("extern {} : proc", func_decl.name);
			return;
		}
		if (!func_decl.is_definition) return;

		current_function = func_decl.name;
		Emit<Text>("\n{} proc {}", func_decl.name, func_decl.is_static ? "private" : "");
	}

	void x86_64Context::Call(char const* func_name)
	{
		Emit<Text>("call {}", func_name);
	}

	void x86_64Context::Call(Register reg)
	{
		Emit<Text>("call {}", GetRegisterName(reg, BitCount_64));
	}

	void x86_64Context::SaveFrameRegister()
	{
		Emit<Text>("push rbp");
		Emit<Text>("mov rbp, rsp");
		frame_reg_saved = true;
	}

	void x86_64Context::AllocateStack(uint32 size)
	{
		if (size)
		{
			Sub(RSP, size, BitCount_64);
			stack_allocated += size;
		}
	}

	void x86_64Context::FreeStack(uint32 size)
	{
		Add(RSP, size, BitCount_64);
		stack_allocated -= size;
	}

	void x86_64Context::JumpToReturn()
	{
		Emit<Text>("jmp {}_end", current_function);
	}

	void x86_64Context::Return()
	{
		if (current_function == "main") Xor(RAX, RAX, BitCount_64);

		Emit<Text>("{}_end:", current_function);
		if (stack_allocated)
		{
			FreeStack(stack_allocated);
		}
		if (frame_reg_saved)
		{
			Emit<Text>("pop rbp");
			frame_reg_saved = false;
		}
		if (current_function == "main")
		{
			Mov(RCX, RAX, BitCount_64);
			Sub(RSP, 32, BitCount_64);
			Call("ExitProcess");
		}
		Emit<Text>("ret");
		Emit<Text>("{} endp", current_function);
	}

}
