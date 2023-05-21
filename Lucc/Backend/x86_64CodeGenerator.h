#pragma once
#include <string>
#include "ICodeGenerator.h"
#include "Frontend/AST.h"

namespace lucc
{

	class x86_64CodeGenerator : public ICodeGenerator, public INodeVisitorAST
	{
		class Context;

	public:
		x86_64CodeGenerator(std::string_view output_file, AST* ast);
		~x86_64CodeGenerator();

		virtual void Generate() override;

	private:
		AST* ast;
		std::string output_file;
		std::string output_buffer;

		std::unique_ptr<Context> ctx;

	private:
		virtual void Visit(VarDeclAST const& node, size_t depth) override;
		virtual void Visit(FunctionDeclAST const& node, size_t depth) override;
		virtual void Visit(BinaryExprAST const& node, size_t depth) override;

	};

}