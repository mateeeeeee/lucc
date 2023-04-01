#include "SourceBuffer.h"
#include <fstream>
#include <sstream>

namespace lu
{
	

	SourceBuffer::SourceBuffer(std::string_view source_file)
		: ref_name(source_file)
	{
		std::string path = PROJECT_DIR + std::string(source_file);
		std::ifstream input_stream(path);
		auto good = input_stream.good();
		std::ostringstream buf;
		buf << input_stream.rdbuf();
		data_buffer = buf.str();
		data_buffer.push_back('\0');
	}

	SourceBuffer::SourceBuffer(char const* buffer_start, char const* buffer_end, std::string_view refname)
		: ref_name(refname), data_buffer(buffer_start, buffer_end - buffer_start)
	{}

}

