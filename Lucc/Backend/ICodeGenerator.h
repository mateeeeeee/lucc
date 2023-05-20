#pragma once

namespace lucc
{
	class ICodeGenerator
	{
	public:
		virtual ~ICodeGenerator() = default;
		virtual void Generate() = 0;
	};

	class ICodegenContext
	{
	public:
		virtual ~ICodegenContext() = default;
		
		
	};
}