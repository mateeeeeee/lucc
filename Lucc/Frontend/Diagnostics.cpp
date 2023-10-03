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

		std::unordered_map<DiagCode, std::string_view> diag_msgs =
		{
			#define DIAG(diag_code, diag_class, diag_msg) {DiagCode::##diag_code, diag_msg},
			#include "Diagnostics.def"
		};
		std::unordered_map<DiagCode, DiagKind> diag_kinds =
		{
			#define DIAG(diag_code, diag_class, diag_msg) {DiagCode::##diag_code, DiagKind::##diag_class},
			#include "Diagnostics.def"
		};

		bool error_reported = false;
		bool warnings_as_errors = false;
		SourceLocation loc;
		std::vector<std::ostream*> output_streams;
	}

	void Initialize(bool _warnings_as_errors)
	{
		warnings_as_errors = _warnings_as_errors;
		RegisterOutput(std::cout);
	}

	void RegisterOutput(std::ostream& os)
	{
		output_streams.push_back(&os);
	}

	void Report(DiagCode code, SourceLocation const& loc)
	{
		DiagKind diag_kind = diag_kinds[code];
		std::string output = std::format("[Diagnostics][{}]: {} in file {} at line: {}, col: {}\n",
										 ToString(diag_kind), diag_msgs[code], loc.filename, loc.line, loc.column);
		
		for (std::ostream* os : output_streams) *os << output;
		if (diag_kind == DiagKind::error) std::exit(EXIT_CODE_COMPILATION_FAILED);
	}

	void Report(DiagCode code)
	{
		Report(code, loc);
	}

	void SetLocation(SourceLocation const& _loc)
	{
		loc = _loc;
	}

}

