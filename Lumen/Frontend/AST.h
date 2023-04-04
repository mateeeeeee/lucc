#pragma once

namespace lu
{
	
	class NodeAST
	{
	public:
		virtual ~NodeAST() = default;

	protected:
		NodeAST() = default;
	};
	
	class DeclarationAST : public NodeAST
	{
	public:


	private:
	};

	class StatementAST : public NodeAST
	{
	public:

	private:
	};


	
}