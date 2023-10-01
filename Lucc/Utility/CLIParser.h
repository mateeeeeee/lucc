#pragma once
#include <vector>
#include <string>

namespace lucc
{
	class CLIArg
	{
		friend class CLIParser;
	public:
		CLIArg(std::vector<std::string>&& prefixes, bool has_value)
			: prefixes(std::move(prefixes)), has_value(has_value)
		{}

		bool AsBool(bool default_value = false) const
		{
			LU_ASSERT(has_value);
			if (value == "true" || value == "1") return true;
			if (value == "false" || value == "0") return false;
			LU_ASSERT_MSG(false, "Invalid bool argument!");
			LU_UNREACHABLE();
		}
		bool AsBoolOr(bool def) const
		{
			if (IsPresent()) return AsBool();
			else return def;
		}

		int32 AsInt() const
		{
			LU_ASSERT(has_value);
			return (int32)strtol(value.c_str(), nullptr, 10);
		}
		int32 AsIntOr(int32 def) const
		{
			if (IsPresent()) return AsInt();
			else return def;
		}

		float AsFloat() const
		{
			LU_ASSERT(has_value);
			return (float)std::strtod(value.c_str(), nullptr);
		}
		float AsFloatOr(float def) const
		{
			if (IsPresent()) return AsFloat();
			else return def;
		}
		
		std::string AsString() const
		{
			LU_ASSERT(has_value);
			return value;
		}
		std::string AsStringOr(std::string const& def) const
		{
			if (IsPresent()) return AsString();
			else return def;
		}

		std::vector<std::string> const& AsVector() const
		{
			return value_vector;
		}

		bool IsPresent() const
		{
			return is_present;
		}
		operator bool() const
		{
			return IsPresent();
		}

	private:
		std::vector<std::string> prefixes;
		bool has_value;
		std::string value;
		std::vector<std::string> value_vector;
		bool is_present = false;

		void SetValue(std::string const& _value)
		{
			LU_ASSERT(has_value);
			value = _value;
		}
		void SetIsPresent()
		{
			is_present = true;
		}
	};

	class CLIParser
	{
	public:
		CLIParser() 
		{
			args.reserve(128);
		}

		[[nodiscard]] CLIArg& AddArg(bool has_value, std::convertible_to<std::string> auto... prefixes)
		{
			args.emplace_back(std::vector<std::string>{prefixes...}, has_value);
			return args.back();
		}

		void Parse(int argc, char** argv)
		{
			std::vector<std::string> cmdline(argv + 1, argv + argc);
			for (size_t i = 0; i < cmdline.size(); ++i)
			{
				bool found = false;
				for (CLIArg& arg : args)
				{
					for (auto const& prefix : arg.prefixes) 
					{
						bool prefix_found = cmdline[i] == prefix;
						if (prefix_found)
						{
							found = true;
							arg.SetIsPresent();
							if (arg.has_value) arg.SetValue(cmdline[++i]);
							break;
						}
					}
					if (found) break;
				}

				if (!found)
				{
					for (CLIArg& arg : args)
					{
						if (arg.prefixes.empty())
						{
							arg.value_vector.push_back(cmdline[i]);
							break;
						}
					}
				}
			}
		}
	private:
		std::vector<CLIArg> args;
	};
}
