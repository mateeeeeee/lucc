#include "SourceBuffer.h"
#include <fstream>
#include <sstream>

namespace lumen
{

	SourceBuffer::SourceBuffer(char const* source_file)
		: ref_name(source_file)
	{
		std::ifstream input_stream(source_file);
		std::ostringstream buf;
		buf << input_stream.rdbuf();
		data_buffer = buf.str();
	}

	SourceBuffer::SourceBuffer(char const* buffer_start, char const* buffer_end, char const* refname)
		: ref_name(refname), data_buffer(buffer_start, buffer_end - buffer_start)
	{}

}

