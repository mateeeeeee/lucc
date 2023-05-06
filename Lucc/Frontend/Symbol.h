#pragma once
#include <string>
#include <vector>
#include <unordered_map>
#include "Type.h"

namespace lucc
{
	enum ScopeKind : uint32
	{
		Scope_Invalid,
		Scope_Constants, 
		Scope_Labels, 
		Scope_Global, 
		Scope_Param, 
		Scope_Local
	};
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

	struct Symbol
	{
		std::string name = "";
		QualifiedType qtype;
	};

	class ScopeTable
	{
	public:
		explicit ScopeTable(uint32 scope_id) : scope_id(scope_id) {}
		uint32 GetScope() const { return scope_id; }

		bool Insert(Symbol const& symbol)
		{
			if (scope_sym_table.contains(symbol.name)) return false;
			scope_sym_table[symbol.name] = symbol;
			return true;
		}
		template<typename... Args> requires std::is_constructible_v<Symbol,Args...>
		bool Insert(Args&&... args)
		{
			return Insert(Symbol(std::forward<Args>(args...)));
		}

		bool Delete(std::string const& sym_name)
		{
			return scope_sym_table.erase(sym_name) > 0;
		}

		Symbol* LookUp(std::string const& sym_name)
		{
			if (scope_sym_table.contains(sym_name)) return &scope_sym_table[sym_name];
			else return nullptr;
		}

	private:
		uint32 const scope_id;
		std::unordered_map<std::string, Symbol> scope_sym_table;
	};

	class SymbolTable
	{
	public:
		SymbolTable()
		{
			scopes.emplace_back(Scope_Global);
		}
		void EnterScope()
		{
			scopes.emplace_back(scopes.back().GetScope() + 1);
		}
		void ExitScope()
		{
			scopes.pop_back();
		}

		bool Insert(Symbol const& symbol)
		{
			return scopes.back().Insert(symbol);
		}
		template<typename... Args> requires std::is_constructible_v<Symbol, Args...>
		bool Insert(Args&&... args)
		{
			return scopes.back().Insert(std:forward<Args>(args...));
		}

		bool Delete(std::string const& sym_name)
		{
			return scopes.back().Delete(sym_name);
		}

		Symbol* LookUp(std::string const& sym_name)
		{
			for (auto scope = scopes.rbegin(); scope != scopes.rend(); ++scope)
			{
				if (Symbol* sym = scope->LookUp(sym_name)) return sym;
			}
			return nullptr;
		}

	private:
		std::vector<ScopeTable> scopes;
	};

	//extern Table constants;
	//extern Table externals;
	//extern Table globals;
	//extern Table identifiers;
	//extern Table labels;
	//extern Table types;
	//Subsets of these tables implement three of the name spaces in ANSI C.
	//identifiers holds the ordinary identifiers. externals holds the subset
	//of identifiers that have been declared extern; it is used to warn about
	//conflicting declarations of external identifiers.globals is the part of the
	//identifiers table that holds identifiers with file scope.
	//Compiler - defined internal labels are stored in 1abe1 s, and type tags
	//are stored in types.
}	