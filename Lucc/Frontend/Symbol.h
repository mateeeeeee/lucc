#pragma once
#include <string>
#include <vector>
#include <unordered_map>
#include "Type.h"

namespace lucc
{
	enum class Storage : uint8
	{
		None,
		Auto,
		Register,
		Static,
		Extern,
		ThreadLocal,
		Typedef
	};
	enum class Linkage : uint8
	{
		Internal,
		External,
		NoLinkage,
		Invalid
	};

	struct VarSymbol
	{
		std::string name = "";
		QualifiedType qtype = builtin_types::Int;
		Storage storage = Storage::None;
		bool global = false;	

		bool is_enum = false;
		int32 enum_value = 0;

		mutable uint64 id = 0;
	};

	inline bool operator==(VarSymbol const& sym1, VarSymbol const& sym2)
	{
		return sym1.id == sym2.id;
	}
	inline bool operator!=(VarSymbol const& sym1, VarSymbol const& sym2)
	{
		return !(sym1 == sym2);
	}

	struct TagSymbol
	{
		std::string name = "";
		QualifiedType type;
		bool enum_type;

		mutable uint64 id = 0;
	};

	template<typename SymType>
	class ScopeTable
	{
		inline static uint64 id = 0;
	public:
		explicit ScopeTable(uint32 scope_id) : scope_id(scope_id) {}
		uint32 GetScope() const { return scope_id; }

		bool Insert(SymType const& symbol)
		{
			if (scope_sym_table.contains(symbol.name)) return false;
			symbol.id = id++;
			scope_sym_table[symbol.name] = symbol;
			return true;
		}

		SymType* LookUp(std::string const& sym_name)
		{
			if (scope_sym_table.contains(sym_name)) return &scope_sym_table[sym_name];
			else return nullptr;
		}


	private:
		uint32 const scope_id;
		std::unordered_map<std::string, SymType> scope_sym_table;
	};

	template<typename SymType>
	class SymbolTable
	{
	public:
		SymbolTable()
		{
			scopes.emplace_back(scope_id++);
		}
		void EnterScope()
		{
			scopes.emplace_back(scope_id++);
		}
		void ExitScope()
		{
			scopes.pop_back();
			--scope_id;
		}

		bool Insert(SymType const& symbol)
		{
			return scopes.back().Insert(symbol);
		}
		
		SymType* LookUp(std::string const& sym_name)
		{
			for (auto scope = scopes.rbegin(); scope != scopes.rend(); ++scope)
			{
				if (SymType* sym = scope->LookUp(sym_name)) return sym;
			}
			return nullptr;
		}
		SymType* LookUp(std::string_view sym_name)
		{
			return LookUp(std::string(sym_name));
		}

		bool IsGlobal() const { return scopes.size() == 1; }

	private:
		std::vector<ScopeTable<SymType>> scopes;
		uint32 scope_id = 0;
	};
}	