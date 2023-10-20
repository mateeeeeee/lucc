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
	class ReturnStmt;
	class IfStmt;
	class BreakStmt;
	class ContinueStmt;
	class WhileStmt;
	class DoWhileStmt;
	class ForStmt;
	class CaseStmt;
	class SwitchStmt;
	class GotoStmt;
	class LabelStmt;

	class Decl;
	class VariableDecl;
	class FunctionDecl;
	class TypedefDecl;

	struct AST;
}