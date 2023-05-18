#include <fstream>
#include <format>
#include "Frontend/AST.h"
#include "x86CodeGenerator.h"


namespace lucc
{
	namespace
	{
		class x86NodeVisitorAST : public INodeVisitorAST
		{
		public:
			explicit x86NodeVisitorAST(std::string& output_buffer) : output_buffer(output_buffer) {}

			void VisitAST(AST* ast)
			{
				ast->translation_unit->Accept(*this, 0);
			}

			virtual void Visit(VarDeclAST const& node, size_t depth)
			{

			}
			virtual void Visit(FunctionDeclAST const& node, size_t depth)
			{

			}

		private:
			std::string& output_buffer;

		private:

			template<typename... Ts>
			void Emit(std::string_view fmt, Ts&&... args)
			{
				output_buffer += std::format(fmt, std::forward<Ts>(args)...);
			}
			template<typename... Ts>
			void EmitLn(std::string_view fmt, Ts&&... args)
			{
				Emit(fmt, std::forward<Ts>(args)...);
				output_buffer += "\n";
			}
		};
	}


	void x86CodeGenerator::Generate()
	{
		std::ofstream output(output_file);
		
		output << output_buffer;
		output.close();
	}
}

