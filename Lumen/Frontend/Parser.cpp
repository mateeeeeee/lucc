#include "Parser.h"
#include "Lexer.h"
#include "AST.h"


namespace lu
{
	namespace types
	{
		static constexpr Type Void = VoidType();
		static constexpr ArithmeticType Bool = ArithmeticType(ArithmeticType::Bool);
		static constexpr ArithmeticType Char = ArithmeticType(ArithmeticType::Char);
		static constexpr ArithmeticType UnsignedChar = ArithmeticType(ArithmeticType::Char, true);
		static constexpr ArithmeticType Short = ArithmeticType(ArithmeticType::Short);
		static constexpr ArithmeticType UnsignedShort = ArithmeticType(ArithmeticType::Short, true);
		static constexpr ArithmeticType Int = ArithmeticType(ArithmeticType::Int);
		static constexpr ArithmeticType UnsignedInt = ArithmeticType(ArithmeticType::Int, true);
		static constexpr ArithmeticType Long = ArithmeticType(ArithmeticType::Long);
		static constexpr ArithmeticType UnsignedLong = ArithmeticType(ArithmeticType::Long, true);
		static constexpr ArithmeticType LongLong = ArithmeticType(ArithmeticType::LongLong);
		static constexpr ArithmeticType UnsignedLongLong = ArithmeticType(ArithmeticType::LongLong, true);
		static constexpr ArithmeticType Float = ArithmeticType(ArithmeticType::Float);
		static constexpr ArithmeticType Double = ArithmeticType(ArithmeticType::Double);
		static constexpr ArithmeticType LongDouble = ArithmeticType(ArithmeticType::Double | ArithmeticType::Long);
	}

	enum class StorageSpecifier : uint8
	{
		None,
		Auto,
		Register,
		Static,
		Extern,
		ThreadLocal,
		Typedef
	};

	enum class ScopeKind : uint8
	{
		File,
		Block,
		Prototype,
		Function
	};

	struct Parser::DeclSpecInfo
	{
		size_t align = 0;
		QualifiedType qtype = &types::Int;
		StorageSpecifier storage = StorageSpecifier::None;
		FunctionSpecifier func_spec = FunctionSpecifier::None;
	};

	struct Parser::DeclaratorInfo
	{
		std::string name;
		QualifiedType qtype;
		SourceLocation loc;
	};


	Parser::Parser(std::vector<Token> const& _tokens)
		: tokens(_tokens), current_token(tokens.begin())
	{}

	bool Parser::Parse()
	{
		ast = std::make_unique<AST>();
		return ParseTranslationUnit();
	}

	//<translation-unit> ::= { <external-declaration> }*
	bool Parser::ParseTranslationUnit()
	{
		while (current_token->IsNot(TokenKind::eof))
		{
			std::unique_ptr<DeclAST> ext_decl = ParseExternalDeclaration();
			if (!ext_decl) return false;
			ast->tr_unit->AddExternalDeclaration(std::move(ext_decl));
		}
		return true;
	}

	std::unique_ptr<DeclAST> Parser::ParseExternalDeclaration()
	{
		DeclSpecInfo decl_spec{};
		if (!ParseDeclSpec(decl_spec)) return nullptr;

		// Typedef
		if (decl_spec.storage == StorageSpecifier::Typedef)
		{
			//Parse Typedef
		}



		return nullptr;
	}

	bool Parser::ParseDeclSpec(DeclSpecInfo& decl_spec)
	{
		decl_spec = DeclSpecInfo{};
		while (current_token->IsDeclSpec())
		{
			TokenKind kind = current_token->GetKind();
			if (current_token->IsStorageSpecifier())
			{
				if (decl_spec.storage != StorageSpecifier::None)
				{
					//diag
					return false;
				}

				switch (kind)
				{
				case TokenKind::KW_static:
					decl_spec.storage = StorageSpecifier::Static;
					break;
				case TokenKind::KW_register:
					decl_spec.storage = StorageSpecifier::Register;
					break;
				case TokenKind::KW_typedef:
					decl_spec.storage = StorageSpecifier::Typedef;
					break;
				case TokenKind::KW__Thread_local:
					decl_spec.storage = StorageSpecifier::ThreadLocal;
					break;
				case TokenKind::KW_extern:
					decl_spec.storage = StorageSpecifier::Extern;
					break;
				}

				//check that typedef was first declared, so the typedef int new_int is ok, but int typedef new_int is not
			}
			//ignore for now, later add support: const, atomic, tls, alignas
			if (Consume(TokenKind::KW_const, TokenKind::KW_volatile,
				TokenKind::KW_auto, TokenKind::KW_register, TokenKind::KW__Atomic,
				TokenKind::KW__Alignas, TokenKind::KW__Thread_local))
				continue;

			//#todo Add support for user-defined types

			//builtin types

			enum TypeFlags
			{
				VOID = 1 << 0,
				BOOL = 1 << 2,
				CHAR = 1 << 4,
				SHORT = 1 << 6,
				INT = 1 << 8,
				LONG = 1 << 10,
				FLOAT = 1 << 12,
				DOUBLE = 1 << 14,
				OTHER = 1 << 16,
				SIGNED = 1 << 17,
				UNSIGNED = 1 << 18,
			};
			uint32 counter = 0;
			switch (kind)
			{
			case TokenKind::KW_void: counter += VOID; break;
			case TokenKind::KW_bool: counter += BOOL; break;
			case TokenKind::KW_char: counter += CHAR; break;
			case TokenKind::KW_short: counter += SHORT; break;
			case TokenKind::KW_int: counter += INT; break;
			case TokenKind::KW_long: counter += LONG; break;
			case TokenKind::KW_float: counter += FLOAT; break;
			case TokenKind::KW_double: counter += DOUBLE; break;
			case TokenKind::KW_signed: counter += SIGNED; break;
			case TokenKind::KW_unsigned: counter += UNSIGNED; break;
			}

			switch (counter) 
			{
			case VOID:
				decl_spec.qtype = &types::Void;
				break;
			case BOOL:
				decl_spec.qtype = &types::Bool;
				break;
			case CHAR:
			case SIGNED + CHAR:
				decl_spec.qtype = &types::Char;
				break;
			case UNSIGNED + CHAR:
				decl_spec.qtype = &types::UnsignedChar;
				break;
			case SHORT:
			case SHORT + INT:
			case SIGNED + SHORT:
			case SIGNED + SHORT + INT:
				decl_spec.qtype = &types::Short;
				break;
			case UNSIGNED + SHORT:
			case UNSIGNED + SHORT + INT:
				decl_spec.qtype = &types::UnsignedShort;
				break;
			case INT:
			case SIGNED:
			case SIGNED + INT:
				decl_spec.qtype = &types::Int;
				break;
			case UNSIGNED:
			case UNSIGNED + INT:
				decl_spec.qtype = &types::UnsignedInt;
				break;
			case LONG:
			case LONG + INT:
			case SIGNED + LONG:
			case SIGNED + LONG + INT:
				decl_spec.qtype = &types::Long;
				break;
			case LONG + LONG:
			case LONG + LONG + INT:
			case SIGNED + LONG + LONG:
			case SIGNED + LONG + LONG + INT:
				decl_spec.qtype = &types::LongLong;
				break;
			case UNSIGNED + LONG:
			case UNSIGNED + LONG + INT:
				decl_spec.qtype = &types::UnsignedLong;
				break;
			case UNSIGNED + LONG + LONG:
			case UNSIGNED + LONG + LONG + INT:
				decl_spec.qtype = &types::UnsignedLongLong;
				break;
			case FLOAT:
				decl_spec.qtype = &types::Float;
				break;
			case DOUBLE:
				decl_spec.qtype = &types::Double;
				break;
			case LONG + DOUBLE:
				decl_spec.qtype = &types::LongDouble;
				break;
			default:
				//diag 
				return false;
			}
		}
		return true;
	}

	bool Parser::ParseDeclarator(DeclaratorInfo& declarator)
	{
		return false;
	}

}

