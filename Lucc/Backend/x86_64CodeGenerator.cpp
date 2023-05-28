#include <fstream>
#include <iostream>
#include <format>
#include <array>
#include "Frontend/AST.h"
#include "x86_64CodeGenerator.h"


namespace lucc
{
	void operator<<(std::ostream& os, x86_64CodeGenerator::OutputBuffer const& buff)
	{
		os << buff.preamble << "\n";
		os << buff.data_segment << "\n";
		os << buff.text_segment << "\n";
	}

	class x86_64CodeGenerator::Context : public ICodegenContext
	{
		static constexpr size_t REG_COUNT = 4;
		static constexpr char const* registers[REG_COUNT]	   = { "r8",  "r9",  "r10",  "r11"  };
		static constexpr char const* byte_registers[REG_COUNT] = { "r8b", "r9b", "r10b", "r11b" };

		enum SegmentType : uint16
		{
			Preamble,
			Data,
			Text
		};

	public:
		explicit Context(OutputBuffer& output_buffer) : output_buffer(output_buffer) 
		{
			Emit<SegmentType::Data>(".data\n");
			Emit<SegmentType::Text>(".code\n");
		}

		virtual register_t AllocateRegister() override
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

		virtual void Mov(int64 v, register_t reg) override
		{
			Emit<Text>("mov\t{}, {}", registers[reg], v);
		}
		virtual void Add(register_t reg1, register_t reg2) override
		{
			Emit<Text>("add\t{}, {}\n", registers[reg1], registers[reg2]);
		}

		virtual void Store(char const* sym_name, register_t reg) override
		{
			Emit<Text>("mov\t{}, {}\n", sym_name, registers[reg]);
		}
		virtual void Load(char const* sym_name, register_t reg) override
		{
			Emit<Text>("mov\t{}, {}\n", registers[reg], sym_name);
		}

		virtual void DeclareStaticVariable(char const* sym_name) override
		{
			Emit<Data>("{}\tdword ?\n", sym_name);
		}
		virtual void DeclareGlobalVariable(char const* sym_name) override
		{
			Emit<Preamble>("public {}\n", sym_name);
			Emit<Data>("{}\tdword ?\n", sym_name);
		}

		virtual void DeclareStaticFunction(char const* sym_name) override
		{
			Emit<Text>("{} proc", sym_name);
		}
		virtual void DeclareGlobalFunction(char const* sym_name) override
		{
			Emit<Preamble>("public {}\n", sym_name);
			Emit<Text>("{} proc", sym_name);
		}
		virtual void ReturnFromFunction(char const* sym_name) override
		{
			Emit<Text>("ret");
			Emit<Text>("{} endp", sym_name);
		}

	private:
		OutputBuffer& output_buffer;
		std::array<bool, REG_COUNT> free_registers = { true, true, true, true };

	private:
		template<SegmentType segment, typename... Ts>
		void Emit(std::string_view fmt, Ts&&... args)
		{
			std::string output = std::vformat(fmt, std::make_format_args(std::forward<Ts>(args)...));
			output += "\n";
			if constexpr (segment == SegmentType::Preamble) output_buffer.preamble += output;
			else if constexpr (segment == SegmentType::Data) output_buffer.data_segment += output;
			else if constexpr (segment == SegmentType::Text) output_buffer.text_segment += output;
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


