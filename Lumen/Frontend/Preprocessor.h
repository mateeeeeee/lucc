#pragma once

namespace lumen
{
	class Lexer;
	class Preprocessor
	{
	public:	
		static bool Preprocess(Lexer&);
	};
}