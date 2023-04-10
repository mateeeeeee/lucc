#include <string_view>
#include <iostream>
#include <format>
#include "Diagnostics.h"
#include "SourceLocation.h"

namespace lucc::diag
{
	namespace
	{
		enum class Class : uint32
		{
			info,
			warning,
			error
		};
		std::string ToString(Class c)
		{
			switch (c)
			{
			case Class::error: return "Error";
			case Class::warning: return "Warning";
			case Class::info: return "Info";
			}
			return "";
		}

		std::unordered_map<Code, std::string_view> diag_msgs =
		{
			#define DIAG(diag_code, diag_class, diag_msg) {Code::##diag_code, diag_msg},
			#include "Diagnostics.def"
		};
		std::unordered_map<Code, Class> diag_classes =
		{
			#define DIAG(diag_code, diag_class, diag_msg) {Code::##diag_code, Class::##diag_class},
			#include "Diagnostics.def"
		};

		DiagSettings settings;
		std::vector<std::ostream*> output_streams;
	}

	void Initialize(DiagSettings const& _settings)
	{
		settings = _settings;
		RegisterOutput(std::cout);
	}

	void RegisterOutput(std::ostream& os)
	{
		output_streams.push_back(&os);
	}

	void Report(Code code, SourceLocation const& loc)
	{
		Class dclass = diag_classes[code];
		std::string output = std::format("[Diagnostics][{}]: {} in file {} at line: {}, col: {}\n",
										 ToString(dclass), diag_msgs[code], loc.filename, loc.line, loc.column);
		
		for (auto* os : output_streams) *os << output;
	}

	void Report(Code code)
	{
		Class dclass = diag_classes[code];
		std::string output = std::format("[Diagnostics][{}]: {}\n", ToString(dclass), diag_msgs[code]);
		for (auto* os : output_streams) *os << output;
	}

}

