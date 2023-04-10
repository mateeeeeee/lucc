#include "Hideset.h"
#include <algorithm>

namespace lucc
{

	Hideset::Hideset(std::string_view name) : hideset()
	{
		hideset.push_back(std::string(name.data(), name.size()));
	}

	void Hideset::Union(Hideset const& hs)
	{
		//if(!std::is_sorted(hideset.begin(), hideset.end())) std::sort(hideset.begin(), hideset.end());
		//if(!std::is_sorted(hs->hideset.begin(), hs->hideset.end())) std::sort(hs->hideset.begin(), hs->hideset.end());
		std::vector<std::string> union_hs;
		std::set_union(hideset.begin(), hideset.end(), hs.hideset.begin(), hs.hideset.end(), std::back_inserter(union_hs));
		std::swap(hideset, union_hs);
	}

	void Hideset::Intersection(Hideset const& hs)
	{
		std::vector<std::string> intersection_hs;
		std::set_intersection(hideset.begin(), hideset.end(), hs.hideset.begin(), hs.hideset.end(), std::back_inserter(intersection_hs));
		std::swap(hideset, intersection_hs);
	}

	bool Hideset::Contains(std::string_view name) const
	{
		for (auto const& s : hideset) if (s.compare(name)) return true;
		return false;
	}

}
