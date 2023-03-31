#pragma once

namespace lumen
{
	class Lexer;
	class Preprocessor
	{
	public:	
		static void Preprocess(Lexer&);
	};
}