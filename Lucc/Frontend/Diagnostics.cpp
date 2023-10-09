#include <string_view>
#include "Diagnostics.h"
#include "Core/Logger.h"

namespace lucc
{
	std::unordered_map<DiagCode, std::string_view> Diagnostics::diag_msgs =
	{
		#define DIAG(diag_code, diag_kind, diag_msg) {DiagCode::##diag_code, diag_msg},
		#include "Diagnostics.def"
	};
	std::unordered_map<DiagCode, Diagnostics::DiagKind> Diagnostics::diag_kinds =
	{
		#define DIAG(diag_code, diag_kind, diag_msg) {DiagCode::##diag_code, DiagKind::##diag_kind},
		#include "Diagnostics.def"
	};

	std::string Diagnostics::ToString(DiagKind c)
	{
		switch (c)
		{
		case DiagKind::error: return "Error";
		case DiagKind::warning: return "Warning";
		case DiagKind::info: return "Info";
		}
		return "";
	}

	void Diagnostics::PrintMessage(DiagKind diag_kind, std::string const& msg)
	{
		switch (diag_kind)
		{
		case DiagKind::info:
			LU_INFO(msg);
			break;
		case DiagKind::warning:
			LU_WARN(msg);
			break;
		case DiagKind::error:
			LU_ERROR(msg);
			break;
		}
	}

	void Diagnostics::SetDefaultLocation(SourceLocation const& _loc)
	{
		loc = _loc;
	}
	void Diagnostics::Report(DiagCode code)
	{
		Report(loc, code);
	}
	void Diagnostics::Report(SourceLocation const& loc, DiagCode code)
	{
		DiagKind diag_kind = diag_kinds[code];
		std::string output = std::format("[Diagnostics][{}]: {} in file {} at line: {}, col: {}\n",
			ToString(diag_kind), diag_msgs[code], loc.filename, loc.line, loc.column);
		PrintMessage(diag_kind, output);
		if (exit_on_error && diag_kind == DiagKind::error) std::exit(EXIT_CODE_COMPILATION_FAILED);
	}


}

