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
			QualifiedType* type_def;
			struct EnumVar
			{
				Type* underlying_type;
				int32 value;
			} enum_var;
		};
		
	};

	class ScopeStack
	{
		class SymbolTable
		{
		public:
			SymbolTable() = default;

		private:
			std::unordered_map<std::string_view, Var> var_map;
			std::unordered_map<std::string_view, QualifiedType> tags_map;
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

		

		ScopeKind GetCurrentScope() const { return scopes.back().scope; }
	private:
		std::vector<SymbolTableScopePair> scopes;
	};
}