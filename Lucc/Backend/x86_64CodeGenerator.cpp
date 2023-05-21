#include <fstream>
#include <iostream>
#include <format>
#include <array>
#include "Frontend/AST.h"
#include "x86_64CodeGenerator.h"


namespace lucc
{
	class x86_64CodeGenerator::Context : public ICodegenContext
	{
		static constexpr size_t REG_COUNT = 4;
		static constexpr char const* registers[REG_COUNT] = { "%r8", "%r9", "%r10", "%r11" };

	public:
		explicit Context(std::string& output_buffer) : output_buffer(output_buffer) {}

		virtual void EmitGlobal(char const* name, bool is_static) override
		{
			if (is_static) Emit("\t.local {}", name);
			else Emit("\t.globl {}", name);

			Emit("\t.bss");
			Emit("\t.align {}", 8);
			Emit("{}:", name);
			Emit("\t.zero {}", 8);
		}

		virtual void GenerateAddress(char const* name) override
		{
			Emit("\tlea {}(%%rip), %%rax", name);
		}
		virtual void GenerateInt64Literal(int64 value) override
		{
			Emit("\tmov ${}, %%rax", value);
		}

		virtual void Store(size_t type_size) override
		{
			if (type_size == 1) Emit("\tmov %%al, (%%rdi)");
			else if (type_size == 2) Emit("\tmov %%ax, (%%rdi)");
			else if (type_size == 4) Emit("\tmov %%eax, (%%rdi)");
			else Emit("\tmov %%rax, (%%rdi)");
		}
		virtual void Push() override
		{
			Emit("\tpush %%rax");
			depth++;
		}

	private:
		std::string& output_buffer;
		bool free[REG_COUNT] = { true, true, true, true };
		size_t depth = 0;

	private:
		template<typename... Ts>
		void Emit(std::string_view fmt, Ts&&... args)
		{
			output_buffer += std::vformat(fmt, std::make_format_args(std::forward<Ts>(args)...));
			output_buffer += "\n";
		}

		void FreeAllRegisters()
		{
			for (size_t i = 0; i < REG_COUNT; ++i) free[i] = true;
		}
		size_t AllocateRegister()
		{
			for (size_t i = 0; i < REG_COUNT; ++i)
			{
				if (free[i]) return i;
			}
			return -1;
		}
		void FreeRegister(size_t i)
		{
			free[i] = true;
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
		ast->translation_unit->Accept(*this, 0);
		output << output_buffer;
		std::cout << "\n\nAfter Codegen: \n";
		std::cout << output_buffer;
		output.close();
	}


	void x86_64CodeGenerator::Visit(VarDeclAST const& node, size_t depth)
	{
		node.Codegen(*ctx);
	}

	void x86_64CodeGenerator::Visit(FunctionDeclAST const& node, size_t depth)
	{
		node.Codegen(*ctx);
	}

	void x86_64CodeGenerator::Visit(BinaryExprAST const& node, size_t depth)
	{
		node.Codegen(*ctx);
	}

}

