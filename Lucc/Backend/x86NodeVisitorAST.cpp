#include <format>
#include "x86NodeVisitorAST.h"


namespace lucc
{
	x86NodeVisitorAST::x86NodeVisitorAST(AST* ast) 
		: stack_position(8)
	{
		ast->translation_unit->Accept(*this, 0);
	}

	void x86NodeVisitorAST::Visit(TranslationUnitAST const& node, size_t depth)
	{
		
	}

	void x86NodeVisitorAST::Visit(VarDeclAST const& node, size_t depth)
	{

	}

	void x86NodeVisitorAST::Visit(FunctionDeclAST const& node, size_t depth)
	{

	}

	template<typename...Ts>
	void x86NodeVisitorAST::Emit(std::string_view format, Ts&&... args)
	{
		output_buffer += std::format(format, std::forward<Ts>(args)...);
	}


}