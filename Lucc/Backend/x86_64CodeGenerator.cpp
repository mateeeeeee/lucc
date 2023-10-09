#include <iostream>
#include "x86_64Context.h"
#include "Frontend/AST.h"

namespace lucc
{
	void operator<<(std::ostream& os, OutputBuffer const& buff)
	{
		os << buff.no_segment << "\n";
		os << buff.rodata_segment << "\n";
		os << buff.bss_segment << "\n";
		os << buff.data_segment << "\n";
		os << buff.text_segment << "\nend\n";
	}

	x86_64CodeGenerator::x86_64CodeGenerator(std::string_view output_file) : output_file(output_file)
	{
		ctx = std::make_unique<x86_64Context>(output_buffer);
	}

	x86_64CodeGenerator::~x86_64CodeGenerator() = default;

	void x86_64CodeGenerator::Generate(AST* ast)
	{
		std::ofstream output(output_file);
		ast->translation_unit->Codegen(*ctx);
		output << output_buffer;
		output.close();
	}
}

