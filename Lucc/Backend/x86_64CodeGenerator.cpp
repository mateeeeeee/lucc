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
		static constexpr char const* registers[REG_COUNT]	   = { "%r8", "%r9", "%r10", "%r11" };
		static constexpr char const* byte_registers[REG_COUNT] = { "%r8b", "%r9b", "%r10b", "%r11b" };

	public:
		explicit Context(std::string& output_buffer) : output_buffer(output_buffer) {}

		virtual size_t AllocateRegister() override
		{
			for (size_t i = 0; i < REG_COUNT; ++i)
			{
				if (free_registers[i]) return i;
			}
			return INVALID_REG;
		}
		virtual void FreeRegister(size_t i) override
		{
			free_registers[i] = true;
		}
		virtual void FreeAllRegisters() override
		{
			free_registers.fill(true);
		}


		virtual void Movq(int64 v, size_t reg) override
		{
			Emit("\tmov\t {} {}", registers[reg], v);
		}
		virtual void Add(size_t reg1, size_t reg2) override
		{
			Emit("\tadd\t{}, {}\n", registers[reg1], registers[reg2]);
		}

		virtual void Store(char const* sym_name, size_t reg) override
		{
			Emit("\tmov\t{}, {}(%%rip)\n", registers[reg], sym_name);
		}
		virtual void Load(char const* sym_name, size_t reg) override
		{
			Emit("\tmov\t{}(%%rip), {}\n", sym_name, registers[reg]);
		}

		virtual void DeclareGlobalSymbol(char const* sym_name) override
		{
			Emit("{}:\n\t.dq 0\n", sym_name);
		}

	private:
		std::string& output_buffer;
		std::array<bool, REG_COUNT> free_registers = { true, true, true, true };

	private:
		template<typename... Ts>
		void Emit(std::string_view fmt, Ts&&... args)
		{
			output_buffer += std::vformat(fmt, std::make_format_args(std::forward<Ts>(args)...));
			output_buffer += "\n";
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

