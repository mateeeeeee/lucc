#pragma once
#include <vector>
#include <string>

namespace lucc
{
	class Hideset
	{
		friend class Preprocessor;
	private:
		explicit Hideset(std::string_view name);

		void Union(Hideset const& hs);
		void Intersection(Hideset const& hs);
		bool Contains(std::string_view name) const;
	private:
		std::vector<std::string> hideset;
	};
}