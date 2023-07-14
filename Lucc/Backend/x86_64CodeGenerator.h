#pragma once
#include <string>
#include <iosfwd>
#include "ICodeGenerator.h"

namespace lucc
{

	class x86_64CodeGenerator : public ICodeGenerator
	{
		class Context;
		enum SegmentType : uint16
		{
			None,
			BSS,
			Const,
			Data,
			Text
		};
		struct OutputBuffer
		{
			std::string no_segment;
			std::string bss_segment;
			std::string rodata_segment;
			std::string data_segment;
			std::string text_segment;
		};

		friend void operator<<(std::ostream& os, OutputBuffer const& buff);

	public:
		explicit x86_64CodeGenerator(std::string_view output_file);
		~x86_64CodeGenerator();

		void Generate(AST* ast);

	private:
		std::string output_file;
		OutputBuffer output_buffer;
		std::unique_ptr<Context> ctx;
	};

}