#pragma once

namespace lucc
{
	class NodeAST;
	class TranslationUnitAST;

	class ExprAST;
	class UnaryExprAST;
	class BinaryExprAST;
	class TernaryExprAST;
	class CastExprAST;
	class ImplicitCastExprAST;
	class FunctionCallExprAST;
	class IntLiteralAST;
	class StringLiteralAST;
	class IdentifierExprAST;
	class DeclRefExprAST;
	class MemberRefExprAST;

	class StmtAST;
	class CompoundStmtAST;
	class DeclStmtAST;
	class ExprStmtAST;
	class NullStmtAST;
	class IfStmtAST;
	class WhileStmtAST;
	class DoWhileStmtAST;
	class ForStmtAST;
	class SwitchStmtAST;
	class CaseStmtAST;
	class ReturnStmtAST;
	class GotoStmtAST;
	class LabelStmtAST;
	class BreakStmtAST;
	class ContinueStmtAST;

	class DeclAST;
	class VariableDeclAST;
	class FunctionDeclAST;
	class TypedefDeclAST;

	struct AST;
}