#include <fstream>
#include "x86CodeGenerator.h"
#include "x86NodeVisitorAST.h"


namespace lucc
{
	void x86CodeGenerator::Generate()
	{
		std::ofstream output(output_file);
		x86NodeVisitorAST x86_visitor(ast);
	}

}

