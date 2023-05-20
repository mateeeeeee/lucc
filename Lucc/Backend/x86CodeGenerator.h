#pragma once
#include <string>
#include "ICodeGenerator.h"
#include "Frontend/AST.h"

namespace lucc
{

	class x86CodeGenerator : public ICodeGenerator, public INodeVisitorAST
	{
		class x86Context;

	public:
		x86CodeGenerator(std::string_view output_file, AST* ast);
		~x86CodeGenerator();

		virtual void Generate() override;

	private:
		AST* ast;
		std::string output_file;
		std::string output_buffer;

		std::unique_ptr<x86Context> ctx;

	private:
		virtual void Visit(VarDeclAST const& node, size_t depth) override;
		virtual void Visit(FunctionDeclAST const& node, size_t depth) override;
		virtual void Visit(BinaryExprAST const& node, size_t depth) override;

	};

}