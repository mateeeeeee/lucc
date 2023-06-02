#pragma once
#include <string>
#include <iosfwd>
#include "ICodeGenerator.h"

namespace lucc
{

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
		explicit x86_64CodeGenerator(std::string_view output_file);
		~x86_64CodeGenerator();

		virtual void Generate(AST* ast) override;

	private:
		std::string output_file;
		OutputBuffer output_buffer;

		std::unique_ptr<Context> ctx;
	};

}