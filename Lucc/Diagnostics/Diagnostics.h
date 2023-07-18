#pragma once
#include<iosfwd>

namespace lucc
{
	struct SourceLocation;
}
namespace lucc::diag
{
	inline constexpr int32 COMPILATION_FAILED = INT32_MAX;

	enum class Code : uint16
	{
#define DIAG(diag_code, diag_class, diag_msg) diag_code,
#include "Diagnostics.def"
	};
	using enum Code;

	struct DiagSettings
	{
		bool treat_warnings_as_errors = false;
		bool multithreaded_support = false;
	};

	void Initialize(DiagSettings const& settings = {});
	void RegisterOutput(std::ostream& os);

	void Report(Code code, SourceLocation const& loc);
	void Report(Code code);
	void SetLocation(SourceLocation const& loc);
}