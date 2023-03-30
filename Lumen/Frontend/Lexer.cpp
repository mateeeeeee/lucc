#include "Lexer.h"
#include "SourceBuffer.h"

namespace lumen
{

	Lexer::Lexer(SourceBuffer const& source) : buf_start(source.GetBufferStart()), buf_end(source.GetBufferEnd()),
		buf_ptr(source.GetBufferStart())
	{

	}

}

