#pragma once
#include <unordered_map>
#include <string_view>
#include <vector>
#include "Type.h"

namespace lucc
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

	struct Object
	{
		std::string name = "";
		QualifiedType qtype{};
		Linkage linkage = Linkage::NoLinkage;
		Storage storage = Storage::None;
	};

	struct Var
	{
		enum class Kind : uint8
		{
			Object,
			Typedef,
			EnumVar
		} kind;
		union 
		{
			Object* obj;
			QualifiedType type_def{};
			struct EnumVar
			{
				ArithmeticType* underlying_type;
				int32 value;
			} enum_var;
		};
		
	};
	struct Tag
	{
		std::string name = "";
		QualifiedType qtype{};
	};

	class ScopeStack
	{
		class SymbolTable
		{
		public:
			SymbolTable() = default;

			Var& AddVar(std::string_view name)
			{
				return var_map[name];
			}
			bool HasVar(std::string_view name) const
			{
				return var_map.contains(name);
			}
			Var const& GetVar(std::string_view name)
			{
				return var_map[name];
			}

			Tag& AddTag(std::string_view name, Tag const& type)
			{
				tags_map[name] = type;
				return tags_map[name];
			}
			bool HasTag(std::string_view name) const
			{
				return tags_map.contains(name);
			}
			Tag const& GetTag(std::string_view name)
			{
				return tags_map[name];
			}


		private:
			std::unordered_map<std::string_view, Var> var_map;
			std::unordered_map<std::string_view, Tag> tags_map;
		};
		struct SymbolTableScopePair
		{
			SymbolTable sym_table;
			ScopeKind   scope;
		};

	public:
		ScopeStack()
		{
			scopes.emplace_back(SymbolTable{}, ScopeKind::File);
		}

		void EnterPrototype() { scopes.emplace_back(SymbolTable{}, ScopeKind::Prototype); }
		void EnterBlock() { scopes.emplace_back(SymbolTable{}, ScopeKind::Block); }
		void ExitPrototype() { scopes.pop_back(); }
		void ExitBlock() { scopes.pop_back(); }

		Var& AddVar(std::string_view name)
		{
			return scopes.back().sym_table.AddVar(name);
		}
		bool HasVar(std::string_view name) const
		{
			return scopes.back().sym_table.HasVar(name);
		}
		Var const& GetVar(std::string_view name)
		{
			return scopes.back().sym_table.GetVar(name);
		}

		Tag& AddTag(std::string_view name, Tag const& type)
		{
			return scopes.back().sym_table.AddTag(name, type);
		}
		bool HasTag(std::string_view name) const
		{
			return scopes.back().sym_table.HasTag(name);
		}
		Tag const& GetTag(std::string_view name)
		{
			return scopes.back().sym_table.GetTag(name);
		}

		ScopeKind GetCurrentScopeKind() const { return scopes.back().scope; }
	private:
		std::vector<SymbolTableScopePair> scopes;
	};
}