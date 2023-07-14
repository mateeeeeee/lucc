#pragma once
#include <string>
#include <iosfwd>
#include "x86_64.h"

namespace lucc
{
	class x86_64Context;
	struct AST;

	class x86_64CodeGenerator 
	{
	public:
		explicit x86_64CodeGenerator(std::string_view output_file);
		~x86_64CodeGenerator();

		void Generate(AST* ast);

	private:
		std::string output_file;
		OutputBuffer output_buffer;
		std::unique_ptr<x86_64Context> ctx;
	};

}