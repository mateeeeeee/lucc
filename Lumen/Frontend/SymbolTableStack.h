#pragma once
#include <unordered_map>
#include <string_view>
#include <vector>
#include <variant>
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

		size_t offset = 0;
		bool is_defined = false;
		bool is_local = false;
		bool is_inline = false;
	};
	struct EnumVar
	{
		ArithmeticType* underlying_type;
		int32 value;
	};

	using TypedefVar = QualifiedType;

	struct Var
	{
		enum Index 
		{
			OBJECT,
			TYPEDEF,
			ENUMVAR
		};

		template<typename T>
		void Set(T&& t)
		{
			value = std::move(t);
		}

		template<Index i>
		auto& Get()
		{
			return std::get<i>(value);
		}

		template<Index i>
		auto const& Get() const
		{
			return std::get<i>(value);
		}
		
		Index Active() const
		{
			return static_cast<Index>(value.index());
		}

		std::variant<Object, TypedefVar, EnumVar> value;
	};
	struct Tag
	{
		std::string name = "";
		QualifiedType qtype{};
	};

	class SymbolTableStack
	{
		class SymbolTable
		{
		public:
			SymbolTable() = default;
			~SymbolTable() = default;

			Var& AddVar(std::string_view name)
			{
				return var_map[name];
			}
			bool HasVar(std::string_view name) const
			{
				return var_map.contains(name);
			}
			Var& GetVar(std::string_view name)
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
		SymbolTableStack()
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
		Var& GetVar(std::string_view name)
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