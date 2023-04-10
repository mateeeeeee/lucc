#include "AST.h"

namespace lucc
{
	void TranslationUnitDeclAST::AddExternalDeclaration(std::unique_ptr<DeclAST>&& ext_decl)
	{
		external_declarations.push_back(std::move(ext_decl));
	}
	void TranslationUnitDeclAST::Accept(NodeVisitorAST& visitor, size_t indent) const
	{
		visitor.Visit(*this, indent);
		for (auto&& decl : external_declarations) decl->Accept(visitor, indent + 1);
	}

	void VarDeclAST::Accept(NodeVisitorAST& visitor, size_t indent) const
	{
		visitor.Visit(*this, indent);
		init_expr->Accept(visitor, indent + 1);
	}

	ParamVarDeclAST::ParamVarDeclAST(FunctionParameter const& param) : VarDeclAST(param.qtype, param.name){}
	void ParamVarDeclAST::Accept(NodeVisitorAST& visitor, size_t indent) const
	{
		visitor.Visit(*this, indent);
	}

	FieldDeclAST::FieldDeclAST(QualifiedType const& type, std::string_view id) : type(type), identifier(id)
	{

	}

	void RecordDeclAST::AddField(std::unique_ptr<FieldDeclAST>&& field)
	{
		fields.push_back(std::move(field));
	}
	void RecordDeclAST::Accept(NodeVisitorAST& visitor, size_t indent) const
	{
		visitor.Visit(*this, indent);
		for (auto&& field : fields) field->Accept(visitor, indent + 1);
	}

	void FunctionDeclAST::AddParamDeclaration(std::unique_ptr<ParamVarDeclAST>&& param)
	{
		param_decls.push_back(std::move(param));
	}
	void FunctionDeclAST::AddBody(std::unique_ptr<CompoundStmtAST>&& body)
	{
		func_body = std::move(body);
	}
	void FunctionDeclAST::Accept(NodeVisitorAST& visitor, size_t indent) const
	{
		visitor.Visit(*this, indent);
		for (auto&& param : param_decls) param->Accept(visitor, indent + 1);
		if(func_body) func_body->Accept(visitor, indent + 1);
	}

	TypedefDeclAST::TypedefDeclAST(QualifiedType const& type, std::string_view typealias) : type(type), typealias(typealias)
	{}
	void TypedefDeclAST::Accept(NodeVisitorAST& visitor, size_t indent) const
	{
		visitor.Visit(*this, indent);
	}

	DeclStmtAST::DeclStmtAST(std::unique_ptr<DeclAST>&& _decl) : decl(std::move(_decl))
	{

	}
	void DeclStmtAST::Accept(NodeVisitorAST& visitor, size_t indent) const
	{
		decl->Accept(visitor, indent);
	}
	
	void CompoundStmtAST::AddStatement(std::unique_ptr<StmtAST>&& stmt)
	{
		statements.push_back(std::move(stmt));
	}
	void CompoundStmtAST::Accept(NodeVisitorAST& visitor, size_t indent) const
	{
		visitor.Visit(*this, indent);
		for (auto&& stmt : statements) stmt->Accept(visitor, indent + 1);
	}

	void ReturnStmtAST::Accept(NodeVisitorAST& visitor, size_t indent) const
	{
		visitor.Visit(*this, indent);
	}

	void IfStmtAST::Accept(NodeVisitorAST& visitor, size_t indent) const
	{
		visitor.Visit(*this, indent);
		condition->Accept(visitor, indent + 1);
		then_stmt->Accept(visitor, indent + 1);
		if (else_stmt) else_stmt->Accept(visitor, indent + 1);
	}

	void WhileStmtAST::Accept(NodeVisitorAST& visitor, size_t indent) const
	{
		visitor.Visit(*this, indent);
		condition->Accept(visitor, indent + 1);
		body->Accept(visitor, indent + 1);
	}

	void DoWhileStmtAST::Accept(NodeVisitorAST& visitor, size_t indent) const
	{
		visitor.Visit(*this, indent);
		condition->Accept(visitor, indent + 1);
		body->Accept(visitor, indent + 1);
	}

}

