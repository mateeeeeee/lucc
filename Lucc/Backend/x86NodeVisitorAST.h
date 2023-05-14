#pragma once
#include "Frontend/AST.h"

namespace lucc
{
	class x86NodeVisitorAST : public INodeVisitorAST
	{
		friend class x86CodeGenerator;

	private:
		x86NodeVisitorAST(AST* ast);


		virtual void Visit(TranslationUnitAST const& node, size_t depth) override;
		virtual void Visit(VarDeclAST const& node, size_t depth) override;
		virtual void Visit(FunctionDeclAST const& node, size_t depth) override;

	private:
		std::string output_buffer;
		int32 stack_position;

	private:

		template<typename... Ts>
		void Emit(std::string_view format, Ts&&... args);
	};

}