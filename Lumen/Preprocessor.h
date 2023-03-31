#pragma once

namespace lumen
{
	class Lexer;

	class Preprocessor
	{
	public:
		Preprocessor(Lexer&) {}

		void Preprocess() {}

	private:
		Lexer& lexer;
	};
}