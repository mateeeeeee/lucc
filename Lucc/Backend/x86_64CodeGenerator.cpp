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
		os << buff.text_segment << "\nend\n";
	}

	class x86_64CodeGenerator::Context : public ICodegenContext
	{
		static constexpr size_t REG_COUNT = 4;
		static constexpr char const* registers[REG_COUNT]	   = { "r8d",  "r9d",  "r10d",  "r11d"  };
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
					return i;
				}
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

		virtual void Mov(register_t reg, int64 val) override
		{
			Emit<Text>("mov\t{}, {}", registers[reg], val);
		}
		virtual void Add(register_t reg1, register_t reg2) override
		{
			Emit<Text>("add\t{}, {}", registers[reg1], registers[reg2]);
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
			Emit<Text>("cmp\t{}, {}", registers[reg], val);
		}
		virtual void Jump(char const* label) override
		{
			Emit<Text>("jmp\t{}{}", label, label_id);
		}
		virtual void JumpZero(char const* label) override
		{
			Emit<Text>("jz\t{}{}", label, label_id);
		}

		virtual void StoreReg(char const* sym_name, register_t reg) override
		{
			Emit<Text>("mov\t{}, {}", sym_name, registers[reg]);
		}
		virtual void LoadReg(char const* sym_name, register_t reg) override
		{
			Emit<Text>("mov\t{}, {}", registers[reg], sym_name);
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
			Emit<Preamble>("public {}", sym_name);
			Emit<Data>("{}\tdword ?", sym_name);
		}

		virtual void DeclareStaticFunction(char const* sym_name) override
		{
			Emit<Text>("\n{} proc", sym_name);
		}
		virtual void DeclareGlobalFunction(char const* sym_name) override
		{
			Emit<Preamble>("public {}", sym_name);
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


