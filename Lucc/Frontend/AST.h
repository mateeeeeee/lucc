#pragma once
#include <vector>
#include <memory>
#include <string>
#include "Type.h"
#include "SourceLocation.h"

namespace lucc
{
	class NodeAST;
	class TranslationUnitDeclAST;
	class TypedefDeclAST;
	class ExprAST;
	class FunctionDeclAST;
	class ParamVarDeclAST;
	class CompoundStmtAST;
	class ReturnStmtAST;

	class NodeVisitorAST
	{
	public:
		virtual ~NodeVisitorAST() = default;
		virtual void Visit(TranslationUnitDeclAST const& node, size_t indent) = 0;
		virtual void Visit(TypedefDeclAST const& node, size_t indent) = 0;
		virtual void Visit(FunctionDeclAST const& node, size_t indent) = 0;
		virtual void Visit(CompoundStmtAST const& node, size_t indent) = 0;
		virtual void Visit(ReturnStmtAST const& node, size_t indent) = 0;
		virtual void Visit(ParamVarDeclAST const& node, size_t indent) = 0;
		virtual void Visit(NodeAST const& node, size_t indent) = 0;
	};

	class NodeAST
	{
	public:
		virtual ~NodeAST() = default;
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const = 0;

	protected:
		NodeAST() = default;
	};
	class DeclAST : public NodeAST
	{
	public:
		DeclAST() = default;
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override 
		{}
	private:
		//SourceLocation Loc;
	};
	class StmtAST : public NodeAST
	{
	public:
		StmtAST() = default;
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override 
		{
		}
	};

	class TranslationUnitDeclAST : public DeclAST
	{
	public:
		TranslationUnitDeclAST() = default;
		void AddExternalDeclaration(std::unique_ptr<DeclAST>&& ext_decl);
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;

	private:
		std::vector<std::unique_ptr<DeclAST>> external_declarations;
	};

	/// Represent the declaration of a variable (in which case it is
	/// an lvalue) a function (in which case it is a function designator) or
	/// an enum constant.
	class ValueDeclAST : public DeclAST
	{
	public:
		ValueDeclAST(QualifiedType const& type, std::string_view id)
			: type(type), identifier(id)
		{}
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override
		{
			visitor.Visit(*this, indent);
		}
	private:
		QualifiedType type;
		std::string identifier;
	};
	class VarDeclAST : public ValueDeclAST
	{
	public:
		VarDeclAST(QualifiedType const& type, std::string_view id, std::unique_ptr<ExprAST>&& expr = nullptr)
			: ValueDeclAST(type, id) {}
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;

	private:
		QualifiedType type;
		std::string identifier;
		std::unique_ptr<ExprAST> init_expr;
	};
	class ParamVarDeclAST : public VarDeclAST
	{
	public:
		ParamVarDeclAST(FunctionParameter const& param);
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;
	};
	class FunctionDeclAST : public ValueDeclAST
	{
	public:
		FunctionDeclAST(QualifiedType const& qtype, std::string_view name) :
			ValueDeclAST(qtype, name)
		{}
		void AddParamDeclaration(std::unique_ptr<ParamVarDeclAST>&& param);
		void AddBody(std::unique_ptr<CompoundStmtAST>&& body);
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;

	private:
		std::vector<std::unique_ptr<ParamVarDeclAST>> param_decls;
		std::unique_ptr<CompoundStmtAST> func_body;
	};

	class FieldDeclAST : public DeclAST
	{
	public:
		FieldDeclAST(QualifiedType const& type, std::string_view id);

	private:
		QualifiedType type;
		std::string identifier;
	};
	class RecordDeclAST : public DeclAST
	{
	public:
		RecordDeclAST() = default;
		void AddField(std::unique_ptr<FieldDeclAST>&& field);
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;

	private:
		std::vector<std::unique_ptr<FieldDeclAST>> fields;
	};
	class TypedefDeclAST : public DeclAST
	{
	public:
		TypedefDeclAST(QualifiedType const& type, std::string_view typealias);
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;

	private:
		QualifiedType type;
		std::string typealias;
	};

	class NullStmtAST : public StmtAST {};
	class DeclStmtAST : public StmtAST
	{
	public:
		DeclStmtAST(std::unique_ptr<DeclAST>&& _decl);
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;

	private:
		std::unique_ptr<DeclAST> decl;
	};
	class ExprStmtAST : public StmtAST 
	{
	public:
		ExprStmtAST(std::unique_ptr<ExprAST>&& _expr)
			: expr(std::move(_expr)) {}
		
	private:
		std::unique_ptr<ExprAST> expr;
	};
	class CompoundStmtAST : public StmtAST
	{
	public:
		CompoundStmtAST() = default;
		void AddStatement(std::unique_ptr<StmtAST>&& stmt);
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;
	
	private:
		std::vector<std::unique_ptr<StmtAST>> statements;
	};
	class IfStmtAST : public StmtAST
	{
	public:
		IfStmtAST(std::unique_ptr<ExprAST>&& condition, std::unique_ptr<StmtAST>&& then_stmt) : condition(std::move(condition)), then_stmt(std::move(then_stmt)),
			else_stmt(std::move(else_stmt))
		{}

		void AddElseStatement(std::unique_ptr<StmtAST>&& _else_stmt)
		{
			else_stmt = std::move(_else_stmt);
		}

		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;

	private:
		std::unique_ptr<ExprAST> condition;
		std::unique_ptr<StmtAST> then_stmt;
		std::unique_ptr<StmtAST> else_stmt;
	};
	class WhileStmtAST : public StmtAST
	{
	public:
		WhileStmtAST(std::unique_ptr<ExprAST>&& condition, std::unique_ptr<StmtAST>&& body) 
			: condition(std::move(condition)), body(std::move(body))
		{}

		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;

	private:
		std::unique_ptr<ExprAST> condition;
		std::unique_ptr<StmtAST> body;
	};
	class DoWhileStmtAST : public StmtAST
	{
	public:
		DoWhileStmtAST(std::unique_ptr<ExprAST>&& condition, std::unique_ptr<StmtAST>&& body)
			: condition(std::move(condition)), body(std::move(body))
		{}

		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;
	private:
		std::unique_ptr<ExprAST> condition;
		std::unique_ptr<StmtAST> body;
	};
	class ReturnStmtAST : public StmtAST
	{
	public:
		ReturnStmtAST() = default;
		explicit ReturnStmtAST(std::unique_ptr<ExprAST>&& expr) : expr(std::move(expr)) {}
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override;
	private:
		std::unique_ptr<ExprAST> expr;
	};

	enum class ExprValueCategory : bool
	{
		LValue,
		RValue
	};
	class ExprAST : public StmtAST
	{
	public:
		ExprAST(QualifiedType const& type, ExprValueCategory kind = ExprValueCategory::LValue)
			: type(type), kind(kind) {}
		virtual void Accept(NodeVisitorAST& visitor, size_t indent) const override {}

		ExprValueCategory GetValueCategory() const { return kind; }
	private:
		ExprValueCategory kind;
		QualifiedType type;
	};

	class DeclRefExprAST : public ExprAST
	{
	public:
		DeclRefExprAST(QualifiedType const& type, std::unique_ptr<ValueDeclAST>&& ref_decl)
			: ExprAST(type), decl(std::move(ref_decl))
		{}

	private:
		std::unique_ptr<ValueDeclAST> decl;
	};

	enum class UnaryOperatorKind : uint8
	{
		PreIncrement, PreDecrement,
		PostIncrement, PostDecrement,
		Plus, Minus, 
		BitNot, LogicalNot,
		Dereference, AddressOf,
		Cast
	};

	class UnaryOperatorAST final : public ExprAST
	{
		using Opcode = UnaryOperatorKind;
	public:
		UnaryOperatorAST(QualifiedType const& type, ExprValueCategory kind, Opcode code,
			std::unique_ptr<ExprAST>&& ref_expr) : ExprAST(type, kind), opcode(code),
			ref_expr(std::move(ref_expr))
		{}

		Opcode GetOpcode() const { return opcode; }

	private:
		Opcode opcode;
		std::unique_ptr<ExprAST> ref_expr;
	};

	enum class BinaryOpKind : uint8 
	{
		Assignment,
		Add, Sub, Mul, Div, Mod,
		BitAnd, BitOr, BitXor, BitShl, BitShr,
		LogicalAnd, LogicalOr,
		Equal, NEqual, Less, Greater, LessEq, GreaterEq,
		MemberAccess,
		kComma
	};

	class BinaryOperatorAST final : public ExprAST
	{
		using Opcode = BinaryOpKind;
	public:
		BinaryOperatorAST(QualifiedType const& type, ExprValueCategory kind, Opcode code,
			std::unique_ptr<ExprAST>&& lhs_operand, 
			std::unique_ptr<ExprAST>&& rhs_operand) : ExprAST(type, kind), opcode(code),
			lhs_operand(std::move(lhs_operand)), rhs_operand(std::move(rhs_operand))
		{}

		Opcode GetOpcode() const { return opcode; }

	private:
		Opcode opcode;
		std::unique_ptr<ExprAST> lhs_operand;
		std::unique_ptr<ExprAST> rhs_operand;
	};

	template<std::integral T>
	class IntegerLiteralAST final : public ExprAST
	{
	public:
		IntegerLiteralAST(T value, QualifiedType const& type, SourceLocation const l) : ExprAST(type, ExprValueCategory::RValue), value(value), loc(loc)
		{}

	private:
		T value;
		SourceLocation loc;
	};

	class StringLiteralAST final : public ExprAST
	{
	public:
		StringLiteralAST(std::string_view value, QualifiedType const& type, SourceLocation const l) : ExprAST(type, ExprValueCategory::RValue), value(value), loc(loc)
		{}

	private:
		std::string_view value;
		SourceLocation loc;
	};

	class CallExprAST final : public ExprAST
	{
	public:
		CallExprAST(QualifiedType const& type, std::unique_ptr<DeclRefExprAST>&& fn) : ExprAST(type), fn(std::move(fn)) {}
		void AddArgument(std::unique_ptr<ExprAST>&& arg)
		{
			args.push_back(std::move(arg));
		}
	private:
		std::unique_ptr<DeclRefExprAST> fn;
		std::vector<std::unique_ptr<ExprAST>> args;
	};

	struct AST
	{
		AST() { tr_unit = std::make_unique<TranslationUnitDeclAST>(); }
		std::unique_ptr<TranslationUnitDeclAST> tr_unit;
	};
}
