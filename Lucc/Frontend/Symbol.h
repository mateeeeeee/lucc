#pragma once
#include <string>
#include <vector>
#include <unordered_map>
#include "SourceLocation.h"

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
	};

	class SymbolTable
	{
		using CurrentScopeSymbolTable = std::unordered_map<std::string, Symbol>;
	public:
		explicit SymbolTable(ScopeKind scope) : current_scope(scope){ symtable.emplace_back(); }

		void EnterScope()
		{
			++current_scope;
			symtable.emplace_back();
		}
		void ExitScope()
		{
			symtable.pop_back();
			--current_scope;
		}

		int32 GetCurrentScope() const { return current_scope; }
		int32 AddSymbol(std::string const& name, Symbol const& symbol)
		{
			if (symtable.back().contains(name)) return Scope_Invalid;
			symtable.back()[name] = symbol;
			return current_scope;
		}
		int32 Exists(std::string const& name)
		{
			int32 scope = current_scope;
			for (auto symtable_ptr = symtable.crbegin(); symtable_ptr != symtable.crend(); ++symtable_ptr)
			{
				if (symtable_ptr->contains(name)) return scope;
				--scope;
			}
			return Scope_Invalid;
		}

	private:
		std::vector<CurrentScopeSymbolTable> symtable;
		uint32 current_scope;
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