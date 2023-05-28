#pragma once
#include <string>
#include <iosfwd>
#include "ICodeGenerator.h"

namespace lucc
{
	struct AST;

	class x86_64CodeGenerator : public ICodeGenerator
	{
		class Context;

		struct OutputBuffer
		{
			std::string preamble;
			std::string data_segment;
			std::string text_segment;
		};
		friend void operator<<(std::ostream& os, OutputBuffer const& buff);

	public:
		x86_64CodeGenerator(std::string_view output_file, AST* ast);
		~x86_64CodeGenerator();

		virtual void Generate() override;

	private:
		AST* ast;
		std::string output_file;
		OutputBuffer output_buffer;

		std::unique_ptr<Context> ctx;
	};

}