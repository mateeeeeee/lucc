#include <string_view>
#include <iostream>
#include <format>
#include "Diagnostics.h"
#include "SourceLocation.h"

namespace lucc::diag
{
	namespace
	{
		enum class DiagKind : uint32
		{
			info,
			warning,
			error
		};
		std::string ToString(DiagKind c)
		{
			switch (c)
			{
			case DiagKind::error: return "Error";
			case DiagKind::warning: return "Warning";
			case DiagKind::info: return "Info";
			}
			return "";
		}

		std::unordered_map<Code, std::string_view> diag_msgs =
		{
			#define DIAG(diag_code, diag_class, diag_msg) {Code::##diag_code, diag_msg},
			#include "Diagnostics.def"
		};
		std::unordered_map<Code, DiagKind> diag_kinds =
		{
			#define DIAG(diag_code, diag_class, diag_msg) {Code::##diag_code, DiagKind::##diag_class},
			#include "Diagnostics.def"
		};

		bool error_reported = false;
		DiagSettings settings;
		SourceLocation loc;
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
		DiagKind dclass = diag_kinds[code];
		std::string output = std::format("[Diagnostics][{}]: {} in file {} at line: {}, col: {}\n",
										 ToString(dclass), diag_msgs[code], loc.filename, loc.line, loc.column);
		
		for (auto* os : output_streams) *os << output;
		if (dclass == DiagKind::error) std::exit(COMPILATION_FAILED);
	}

	void Report(Code code)
	{
		Report(code, loc);
	}

	void SetLocation(SourceLocation const& _loc)
	{
		loc = _loc;
	}

}

