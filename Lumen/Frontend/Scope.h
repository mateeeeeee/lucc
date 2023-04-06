#pragma once
#include <unordered_map>
#include <string_view>
#include <stack>
#include "Type.h"

namespace lu
{
	class Object;

	enum class ScopeKind : uint8
	{
		File,
		Block,
		Prototype,
		Function
	};

	class Scope
	{
		struct ScopeVar
		{
			QualifiedType* type_def;
			QualifiedType* enum_type;
			int32 enum_val;
		};

	public:
		Scope(ScopeKind kind) : kind(kind) {}
		ScopeKind GetKind() const { return kind; }

		bool AddTag(std::string_view tag, QualifiedType* tag_qtype)
		{
			if (tags.find(tag) != tags.end())
			{
				//diag
				return false;
			}
			tags[tag] = tag_qtype;
		}
		
	private:
		ScopeKind kind;
		std::unordered_map<std::string_view, QualifiedType*> tags{};
	};

	class ScopeStack
	{

	public:
		ScopeStack()
		{
			scope_stack.push(ScopeKind::File);
		}
		bool AddTag(std::string_view tag, QualifiedType* tag_qtype)
		{
			scope_stack.top().AddTag(tag, tag_qtype);
		}

	private:
		std::stack<Scope> scope_stack;
	};
}