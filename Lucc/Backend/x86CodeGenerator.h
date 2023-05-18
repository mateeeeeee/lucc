#pragma once
#include <string>
#include "ICodeGenerator.h"

namespace lucc
{
	class VarDeclAST;
	struct AST;

	class x86CodeGenerator : public ICodeGenerator
	{
	public:
		x86CodeGenerator(std::string_view output_file, AST* ast)
			: output_file(output_file), ast(ast) {}

		virtual void Generate() override;

	private:
		AST* ast;
		std::string output_file;
		std::string output_buffer;
	};


}