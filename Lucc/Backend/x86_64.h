#pragma once
#include "Core/Defines.h"

namespace lucc
{
	enum BitCount
	{
		BitCount_8,
		BitCount_16,
		BitCount_32,
		BitCount_64
	};
	enum Register
	{
		RAX,
		RBX,
		RCX,
		RDX,
		RSP,
		RBP,
		RSI,
		RDI,
		R8,
		R9,
		R10,
		R11,
		R12,
		R13,
		R14,
		R15,
		InvalidRegister,
		RegisterCount = InvalidRegister
	};
	static Register const scratch_registers[] = { RBX, R10, R11, R12, R13, R14, R15 };
	static Register const volatile_registers[] = { RAX, RBX, RCX, RDX, R8, R9, R10, R11 };

	inline bool IsScratchRegister(Register reg)
	{
		for (int i = 0; i < std::size(scratch_registers); i++)
		{
			if (reg == scratch_registers[i]) return true;
		}
		return false;
	}
	inline char const* GetRegisterName(Register reg, BitCount bits)
	{
		static constexpr char const* registers[RegisterCount][4] = {
			{"al"  , "ax"  , "eax" , "rax"},
			{"bl"  , "bx"  , "ebx" , "rbx"},
			{"cl"  , "cx"  , "ecx" , "rcx"},
			{"dl"  , "dx"  , "edx" , "rdx"},
			{"spl" , "sp"  , "esp" , "rsp"},
			{"bpl" , "bp"  , "ebp" , "rbp"},
			{"sil" , "si"  , "esi" , "rsi"},
			{"dil" , "di"  , "edi" , "rdi"},
			{"r8b" , "r8w" , "r8d" , "r8" },
			{"r9b" , "r9w" , "r9d" , "r9" },
			{"r10b", "r10w", "r10d", "r10"},
			{"r11b", "r11w", "r11d", "r11"},
			{"r12b", "r12w", "r12d", "r12"},
			{"r13b", "r13w", "r13d", "r13"},
			{"r14b", "r14w", "r14d", "r14"},
			{"r15b", "r15w", "r15d", "r15"},
		};
		return registers[reg][bits];
	}
	inline char const* GetWordType(size_t size) 
	{
		switch (size) 
		{
		case 1:  return "byte";
		case 2:  return "word";
		case 4:  return "dword";
		case 8:
		default: return "qword";
		}
	}
	inline char const* GetWordCast(size_t size)
	{
		switch (size)
		{
		case 1:  return "byte ptr";
		case 2:  return "word ptr";
		case 4:  return "dword ptr";
		case 8:
		default: return "qword ptr";
		}
	}
	inline BitCount GetBitCount(size_t size)
	{
		switch (size)
		{
		case 1:  return BitCount_8;
		case 2:  return BitCount_16;
		case 4:  return BitCount_32;
		case 8:
		default: return BitCount_64;
		}
	}
	inline constexpr uint32 ARGUMENTS_PASSED_BY_REGISTERS = 4;

	enum class ConditionCode
	{
		None,
		E,
		NE,
		B,
		BE,
		A,
		AE,
		L,
		LE,
		G,
		GE,
		Z,
		NZ,
		S,
		NS
	};
	inline ConditionCode InvertConditionCode(ConditionCode cc)
	{
		switch (cc) 
		{
		case ConditionCode::E:  return ConditionCode::NE;
		case ConditionCode::NE: return ConditionCode::E;
		case ConditionCode::B:  return ConditionCode::AE;
		case ConditionCode::BE: return ConditionCode::A;
		case ConditionCode::A:  return ConditionCode::BE;
		case ConditionCode::AE: return ConditionCode::B;
		case ConditionCode::L:  return ConditionCode::GE;
		case ConditionCode::LE: return ConditionCode::G;
		case ConditionCode::G:  return ConditionCode::LE;
		case ConditionCode::GE: return ConditionCode::L;
		case ConditionCode::Z:  return ConditionCode::NZ;
		case ConditionCode::NZ: return ConditionCode::Z;
		case ConditionCode::S:  return ConditionCode::NS;
		case ConditionCode::NS: return ConditionCode::S;
		case ConditionCode::None: 
		default:    
			return ConditionCode::None;
		}
	}

	enum class OperandForm
	{
		Immediate,
		Register,
		Global,
		SIB
	};

	enum SIBScale
	{
		SIBScale_None = 0x0,
		SIBScale_x1 = 0x1,
		SIBScale_x2 = 0x2,
		SIBScale_x4 = 0x4,
		SIBScale_x8 = 0x8,
	};
	struct SIB
	{
		Register base_reg = InvalidRegister;
		Register index_reg = InvalidRegister;
		SIBScale scale = SIBScale_None;
		int32 displacement = 0;
	};

	struct Operand
	{
		constexpr Operand(int64 imm) : immediate(imm), form(OperandForm::Immediate) {}
		constexpr Operand(Register reg) : reg(reg), form(OperandForm::Register) {}
		constexpr Operand(char const* global) : global(global), form(OperandForm::Global) {}
		constexpr Operand(Register base_reg, int32 displacement, Register index_reg = InvalidRegister, SIBScale scale = SIBScale_None) : 
			sib{ .base_reg = base_reg,.index_reg = index_reg, .scale = scale, .displacement = displacement}, form(OperandForm::SIB) {}

		constexpr OperandForm GetForm() const { return form; }

		union
		{
			int64 immediate;
			Register reg;
			char const* global;
			SIB sib;
		};
		OperandForm form;
	};
	using OperandRef = Operand const&;

	struct VarDeclCG
	{
		char const* name;
		bool is_static;
		bool is_const;
		bool is_extern;
		BitCount bits;
		int64* init_value = nullptr;
	};
	struct ArrayDeclCG
	{
		char const* name;
		bool is_static;
		bool is_const;
		bool is_extern;
		BitCount bits;
		int64* init_values = nullptr;
		size_t init_values_size = 0;
	};
	struct FunctionDeclCG
	{
		char const* name;
		bool is_static;
		bool is_extern;
	};
}