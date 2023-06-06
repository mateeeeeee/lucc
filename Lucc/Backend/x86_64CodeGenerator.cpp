#include <fstream>
#include <iostream>
#include "Frontend/AST.h"
#include "x86_64Context.h"


namespace lucc
{
	void operator<<(std::ostream& os, x86_64CodeGenerator::OutputBuffer const& buff)
	{
		os << buff.preamble << "\n";
		os << buff.data_segment << "\n";
		os << buff.text_segment << "\nend\n";
	}

	x86_64CodeGenerator::x86_64CodeGenerator(std::string_view output_file) : output_file(output_file)
	{
		ctx = std::make_unique<Context>(output_buffer);
	}

	x86_64CodeGenerator::~x86_64CodeGenerator() = default;

	void x86_64CodeGenerator::Generate(AST* ast)
	{
		std::ofstream output(output_file);
		ast->translation_unit->Codegen(*ctx);
		output << output_buffer;
		std::cout << "\n\nAfter Codegen:\n";
		std::cout << output_buffer;
		output.close();
	}
}


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

