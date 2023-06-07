#pragma once

namespace lucc
{
	struct AST;

	class ICodeGenerator
	{
	public:
		virtual ~ICodeGenerator() = default;
		virtual void Generate(AST*) = 0;
	};
}