#pragma once
#include <unordered_map>
#include <format>
#include "SourceLocation.h"
#include "Utility/Singleton.h"

namespace lucc
{
	inline constexpr int32 EXIT_CODE_COMPILATION_FAILED = INT32_MAX;
	enum DiagCode : uint32
	{
		#define DIAG(diag_code, diag_kind, diag_msg) diag_code,
		#include "Diagnostics.def"
	};

	class Diagnostics : public Singleton<Diagnostics>
	{
		friend class Singleton<Diagnostics>;

		enum class DiagKind : uint32
		{
			info,
			warning,
			error
		};
		static std::unordered_map<DiagCode, std::string_view> diag_msgs;
		static std::unordered_map<DiagCode, DiagKind> diag_kinds;
		static std::string ToString(DiagKind c);
		static void PrintMessage(DiagKind diag_kind, std::string const& msg);

	public:

		void SetDefaultLocation(SourceLocation const& loc);
		void Report(DiagCode code);
		void Report(SourceLocation const& loc, DiagCode code);
		template<typename... Args>
		void Report(SourceLocation const& loc, DiagCode code, Args&&... args)
		{
			DiagKind diag_kind = diag_kinds[code];
			std::string fmt = diag_msgs[code];
			std::string diag_msg = std::vformat(fmt, std::make_format_args(std::forward<Args>(args)...));
			std::string output = std::format("[Diagnostics][{}]: {} in file {} at line: {}, col: {}\n",
				ToString(diag_kind), diag_msg, loc.filename, loc.line, loc.column);
			output += "\n";

			PrintMessage(output);
			if (exit_on_error && diag_kind == DiagKind::error) std::exit(EXIT_CODE_COMPILATION_FAILED);
		}
	private:
		bool warnings_as_errors = false;
		bool exit_on_error = true;
		SourceLocation loc;
		bool error_reported = false;

	private:
		Diagnostics() {}
		~Diagnostics() {}
	};
#define g_Diagnostics Diagnostics::Get()
}