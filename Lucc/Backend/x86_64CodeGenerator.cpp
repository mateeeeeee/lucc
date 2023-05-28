#include <fstream>
#include <iostream>
#include <format>
#include <array>
#include "Core/Defines.h"
#include "Frontend/AST.h"
#include "x86_64CodeGenerator.h"


namespace lucc
{
	//The first four integer arguments are passed in registers. 
	//Integer values are passed in left - to - right order in RCX, RDX, R8, and R9, respectively. 
	//Arguments five and higher are passed on the stack.

	//Any floating - point and double - precision arguments in the first four parameters are passed in XMM0 - XMM3, 
	//depending on position.

	//Now that we've sorted our inputs, it's time to do the same for our outputs as well.
	//The Microsoft x64 calling convention, thankfully, has simpler rules when it comes to return values from functions.
	//1. Any scalar return value 64 bits or less in size is returned in rax.
	//2. Any floating - point value is returned in xmm0.
	//3.  If your function is returning a user-defined type (such as a struct), 
	//the same rules apply to it as if it were being passed in a register: 
	//it must be of a size of 1/2/4/8/16/32/64 bits. If it is, it will be returned in the rax register; 
	//if not, a pointer to its memory will be returned instead. 
	//The caller is responsible for allocating this memory and passing the pointer in the appropriate integer register before making the call to the function.

	//There are sixteen 64 - bit registers in x86 - 64: % rax, % rbx, % rcx, % rdx, % rdi, % rsi, % rbp,
	//% rsp, and% r8 - r15. Of these, % rax, % rcx, % rdx, % rdi, % rsi, % rsp, and% r8 - r11 are
	//considered caller - save registers, meaning that they are not necessarily saved across function
	//calls. By convention, % rax is used to store a function’s return value, if it existsand is no more
	//than 64 bits long. (Larger return types like structs are returned using the stack.) Registers% rbx,
	//% rbp, and% r12 - r15 are callee - save registers, meaning that they are saved across function
	//calls.Register% rsp is used as the stack pointer, a pointer to the topmost element in the stack.
	//Additionally, % rdi, % rsi, % rdx, % rcx, % r8, and% r9 are used to pass the first six integer
	//or pointer parameters to called functions.Additional parameters(or large parameters such as
	//	structs passed by value) are passed on the stack.
	//In 32 - bit x86, the base pointer(formerly % ebp, now % rbp) was used to keep track of the base of
	//the current stack frame, and a called function would save the base pointer of its caller prior to
	//updating the base pointer to its own stack frame.With the advent of the 64 - bit architecture, this
	//has been mostly eliminated, save for a few special cases when the compiler cannot determine
	//ahead of time how much stack space needs to be allocated for a particular function(see
	//	Dynamic stack allocation).

	//Setting Up : When a call instruction is executed, the address of the following instruction is
	//	pushed onto the stack as the return address and control passes to the specified function.
	//	If the function is going to use any of the , the
	//	current value of each should be pushed onto the stack to be restored at the end.For example :
	//  Pushq % rbx
	//	pushq% r12
	//	pushq% r13
	//	Finally, additional space may be allocated on the stack for local variables.While it is possible to
	//	make space on the stack as needed in a function body, it is generally more efficient to allocate
	//	this space all at once at the beginning of the function.This can be accomplished using the call
	//	subq $N, % rsp where N is the size of the callee’s stack frame.For example :


	void operator<<(std::ostream& os, x86_64CodeGenerator::OutputBuffer const& buff)
	{
		os << buff.preamble << "\n";
		os << buff.data_segment << "\n";
		os << buff.text_segment << "\nend\n";
	}

	class x86_64CodeGenerator::Context : public ICodegenContext
	{
		static constexpr size_t REG_COUNT = 9;
		static constexpr size_t FUNC_ARGS_COUNT_IN_REGISTERS = 4;
		static constexpr size_t RETURN_REGISTER_INDEX = 8;

		//callee - save registers (%rbx, %rbp, r12-r15) are not here for now
		static constexpr char const* qword_registers[REG_COUNT] = { "r10" , "r11" ,"r8" , "r9" , "rcx", "rdx", "rdi", "rsi", "rax" };
		static constexpr char const* dword_registers[REG_COUNT] = { "r10d", "r11d","r8d", "r9d", "ecx", "edx", "edi", "esi", "eax" };
		static constexpr char const* byte_registers[REG_COUNT]	= { "r10b", "r11b","r8b", "r9b", "cl" , "dl" , "dil", "sil", "al " };

		enum SegmentType : uint16
		{
			None,
			Data,
			Text
		};

	public:
		explicit Context(OutputBuffer& output_buffer) : output_buffer(output_buffer) 
		{
			free_registers.fill(true);
			Emit<Data>(".data");
			Emit<Text>(".code");
		}

		virtual register_t AllocateRegister() override
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
		virtual register_t	AllocateRegisterForReturn() override
		{
			if (free_registers[RETURN_REGISTER_INDEX])
			{
				free_registers[RETURN_REGISTER_INDEX] = false;
				return register_t(RETURN_REGISTER_INDEX);
			}
			else LU_ASSERT(false);
			return INVALID_REG;
		}
		virtual register_t AllocateRegisterForFunctionArg(size_t arg_index) override
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
		virtual void FreeRegister(register_t reg) override
		{
			free_registers[reg.id] = true;
		}
		virtual void FreeAllRegisters() override
		{
			free_registers.fill(true);
		}

		virtual void Mov(register_t reg, int64 val) override
		{
			Emit<Text>("mov\t{}, {}", dword_registers[reg.id], val);
		}
		virtual void Add(register_t reg1, register_t reg2) override
		{
			Emit<Text>("add\t{}, {}", dword_registers[reg1.id], dword_registers[reg2.id]);
		}
		virtual void AddImm(register_t reg1, int64 val) override
		{
			Emit<Text>("add\t{}, {}", dword_registers[reg1.id], val);
		}
		virtual void Sub(register_t reg1, register_t reg2) override
		{
			Emit<Text>("sub\t{}, {}", dword_registers[reg1.id], dword_registers[reg2.id]);
		}
		virtual void SubImm(register_t reg1, int64 val) override
		{
			Emit<Text>("sub\t{}, {}", dword_registers[reg1.id], val);
		}

		virtual void GenerateLabelId() override
		{
			label_id = GenerateInteger();
		}
		virtual void Label(char const* label) override
		{
			Emit<Text>("{}{}: ", label, label_id);
		}
		virtual void Compare(register_t reg, int64 val = 0) override
		{
			Emit<Text>("cmp\t{}, {}", dword_registers[reg.id], val);
		}
		virtual void Compare(register_t reg1, register_t reg2) override
		{
			Emit<Text>("cmp\t{}, {}", dword_registers[reg1.id], dword_registers[reg2.id]);
		}
		virtual void Set(register_t reg, Condition cond) override
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
		virtual void Jump(char const* label, Condition cond) override
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

		virtual void StoreReg(char const* sym_name, register_t reg) override
		{
			Emit<Text>("mov\t{}, {}", sym_name, dword_registers[reg.id]);
		}
		virtual void LoadReg(char const* sym_name, register_t reg) override
		{
			Emit<Text>("mov\t{}, {}", dword_registers[reg.id], sym_name);
		}
		virtual void StoreImm(char const* sym_name, int64 val) override
		{
			Emit<Text>("mov\t{}, {}", sym_name, val);
		}

		virtual void DeclareStaticVariable(char const* sym_name) override
		{
			Emit<Data>("{}\tdword ?", sym_name);
		}
		virtual void DeclareGlobalVariable(char const* sym_name) override
		{
			Emit<None>("public {}", sym_name);
			Emit<Data>("{}\tdword ?", sym_name);
		}

		virtual void DeclareStaticFunction(char const* sym_name) override
		{
			Emit<Text>("\n{} proc", sym_name);
		}
		virtual void DeclareGlobalFunction(char const* sym_name) override
		{
			Emit<None>("public {}", sym_name);
			Emit<Text>("\n{} proc", sym_name);
		}
		virtual void ReturnFromFunction(char const* sym_name) override
		{
			Emit<Text>("ret");
			Emit<Text>("{} endp", sym_name);
		}

	private:
		OutputBuffer& output_buffer;
		size_t label_id;
		std::array<bool, REG_COUNT> free_registers;

	private:
		template<SegmentType segment, typename... Ts>
		void Emit(std::string_view fmt, Ts&&... args)
		{
			std::string output = std::vformat(fmt, std::make_format_args(std::forward<Ts>(args)...));
			output += "\n";
			if		constexpr (segment == SegmentType::None) output_buffer.preamble		+= output;
			else if constexpr (segment == SegmentType::Data)	 output_buffer.data_segment += output;
			else if constexpr (segment == SegmentType::Text)	 output_buffer.text_segment += output;
		}

		static size_t GenerateInteger()
		{
			static size_t i = 0;
			return ++i;
		}
	};

	x86_64CodeGenerator::x86_64CodeGenerator(std::string_view output_file, AST* ast) : output_file(output_file), ast(ast) 
	{
		ctx = std::make_unique<Context>(output_buffer);
	}

	x86_64CodeGenerator::~x86_64CodeGenerator() = default;

	void x86_64CodeGenerator::Generate()
	{
		std::ofstream output(output_file);
		ast->translation_unit->Codegen(*ctx);
		output << output_buffer;
		std::cout << "\n\nAfter Codegen:\n";
		std::cout << output_buffer;
		output.close();
	}
}


