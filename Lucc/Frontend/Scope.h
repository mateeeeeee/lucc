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

	struct DeclSymbol
	{
		std::string name = "";
		QualifiedType qtype = builtin_types::Int;
		Storage storage = Storage::None;
		bool global = false;	

		bool is_enum = false;
		int32 enum_value = 0;

		class DeclAST* decl_ast = nullptr;
	};
	struct TagSymbol
	{
		std::string name = "";
		QualifiedType type;
		bool enum_type;
	};

	template<typename T>
	class Scope
	{
		using SymType = T;
	public:
		
		bool Insert(SymType const& symbol)
		{
			if (scope_sym_table.contains(symbol.name)) return false;
			scope_sym_table[symbol.name] = symbol;
			return true;
		}

		SymType* LookUp(std::string const& sym_name)
		{
			if (scope_sym_table.contains(sym_name)) return &scope_sym_table[sym_name];
			else return nullptr;
		}

	private:
		std::unordered_map<std::string, SymType> scope_sym_table;
	};

	template<typename T>
	class ScopeStack
	{
	public:
		using SymType = T;

	public:
		ScopeStack()
		{
			scopes.emplace_back();
		}
		void EnterScope()
		{
			scopes.emplace_back();
		}
		void ExitScope()
		{
			scopes.pop_back();
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
		SymType* LookUpCurrentScope(std::string_view sym_name)
		{
			if (SymType* sym = scopes.back().LookUp(std::string(sym_name))) return sym;
			return nullptr;
		}

		bool IsGlobal() const { return scopes.size() == 1; }

	private:
		std::vector<Scope<SymType>> scopes;
	};

	template<typename T>
	struct ScopeStackGuard
	{
		ScopeStackGuard(ScopeStack<T>& scope_stack) : scope_stack(scope_stack)
		{
			scope_stack.EnterScope();
		}
		~ScopeStackGuard()
		{
			scope_stack.ExitScope();
		}
		ScopeStack<T>& scope_stack;
	};
	#define SCOPE_STACK_GUARD(scope_stack) ScopeStackGuard<std::remove_reference_t<decltype(*scope_stack)>::SymType> LU_CONCAT(_scope_stack_,__COUNTER__)(*scope_stack)
}	