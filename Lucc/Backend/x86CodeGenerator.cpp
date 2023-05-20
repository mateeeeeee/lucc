#include <fstream>
#include <format>
#include <array>
#include "Frontend/AST.h"
#include "x86CodeGenerator.h"


namespace lucc
{
	class x86CodeGenerator::x86Context : public ICodegenContext
	{
		static constexpr size_t REG_COUNT = 4;
		static constexpr char const* registers[REG_COUNT] = { "%r8", "%r9", "%r10", "%r11" };

	public:
		explicit x86Context(std::string& output_buffer) : output_buffer(output_buffer) {}


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

	private:
		std::string& output_buffer;
		bool free[REG_COUNT] = { true, true, true, true };

	private:
		template<typename... Ts>
		void Emit(std::string_view fmt, Ts&&... args)
		{
			output_buffer += std::vformat(fmt, std::make_format_args(std::forward<Ts>(args)...));
			output_buffer += "\n";
		}
	};

	x86CodeGenerator::x86CodeGenerator(std::string_view output_file, AST* ast) : output_file(output_file), ast(ast) 
	{
		ctx = std::make_unique<x86Context>(output_buffer);
	}

	x86CodeGenerator::~x86CodeGenerator() = default;

	void x86CodeGenerator::Generate()
	{
		std::ofstream output(output_file);
		ast->translation_unit->Accept(*this, 0);
		output << output_buffer;
		output.close();
	}


	void x86CodeGenerator::Visit(VarDeclAST const& node, size_t depth)
	{
		node.Codegen(*ctx);
	}

	void x86CodeGenerator::Visit(FunctionDeclAST const& node, size_t depth)
	{
		node.Codegen(*ctx);
	}

	void x86CodeGenerator::Visit(BinaryExprAST const& node, size_t depth)
	{
		node.Codegen(*ctx);
	}

}

