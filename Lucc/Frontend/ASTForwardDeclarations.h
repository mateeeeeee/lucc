#pragma once

namespace lucc
{
	class NodeAST;
	class TranslationUnit;

	class Expr;
	class UnaryExpr;
	class BinaryExpr;
	class TernaryExpr;
	class CastExpr;
	class ImplicitCastExprAST;
	class FunctionCallExpr;
	class IntLiteral;
	class StringLiteral;
	class IdentifierExpr;
	class DeclRefExpr;
	class MemberRefExpr;

	class Stmt;
	class CompoundStmt;
	class DeclStmt;
	class ExprStmt;
	class NullStmt;
	class IfStmt;
	class WhileStmt;
	class DoWhileStmt;
	class ForStmt;
	class SwitchStmt;
	class CaseStmt;
	class ReturnStmt;
	class GotoStmt;
	class LabelStmt;
	class BreakStmt;
	class ContinueStmt;

	class Decl;
	class VariableDecl;
	class FunctionDecl;
	class TypedefDecl;

	struct AST;
}