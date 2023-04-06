#pragma once
#include <unordered_map>
#include <string_view>
#include <vector>
#include "Type.h"

namespace lu
{
	enum class ScopeKind : uint8
	{
		File,
		Block,
		Prototype,
		Function
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

	class SymbolTable
	{
		struct SymbolAttributes
		{
			QualifiedType qtype{};
			Linkage linkage = Linkage::External;
			Storage specifier = Storage::None;
			SourceLocation source_loc{};
		};

		struct TagAttributes
		{
			QualifiedType qtype{};
			SourceLocation source_loc{};
		};

	public:
		SymbolTable() = default;

		bool AddSymbol(std::string_view symbol_name, QualifiedType const& type,
			Linkage linkage = Linkage::NoLinkage, Storage storage = Storage::None)
		{
			if (symbol_map.contains(symbol_name))
			{
				//diag
				return false;
			}
			symbol_map.emplace(std::piecewise_construct,
				std::forward_as_tuple(symbol_name),
				std::forward_as_tuple(type, linkage, storage));
			return true;
		}
		bool HasSymbol(std::string_view symbol_name) const
		{
			return symbol_map.contains(symbol_name);
		}
		SymbolAttributes const& GetSymbolAttributes(std::string_view symbol_name) const
		{
			return const_cast<SymbolTable&>(*this).symbol_map[symbol_name];
		}


	private:
		std::unordered_map<std::string_view, SymbolAttributes> symbol_map;
	};

	class Scope
	{
	public:


	private:
	};

	class ScopeStack
	{
	public:
		ScopeStack()
		{
			scopes.push_back(ScopeKind::File);
		}

		void EnterPrototype() { scopes.emplace_back(ScopeKind::Prototype); }
		void EnterBlock() { scopes.emplace_back(ScopeKind::Block); }
		void ExitPrototype() { scopes.pop_back(); }
		void ExitBlock() { scopes.pop_back(); }

		 

	private:
		std::vector<ScopeKind> scopes;
	};
}