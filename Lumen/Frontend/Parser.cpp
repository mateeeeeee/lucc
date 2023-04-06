#include "Parser.h"
#include "Lexer.h"
#include "AST.h"
#include "Symbol.h"

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

	struct Parser::DeclSpecInfo
	{
		size_t align = 0;
		QualifiedType qtype = types::Int;
		Storage storage = Storage::None;
		FunctionSpecifier func_spec = FunctionSpecifier::None;
	};
	struct Parser::DeclaratorInfo
	{
		std::string name = "";
		QualifiedType qtype{};
		SourceLocation loc;
	};

	Parser::Parser(std::vector<Token> const& _tokens)
		: tokens(_tokens), current_token(tokens.begin())
	{}

	bool Parser::Parse()
	{
		ast = std::make_unique<AST>();
		scope_stack = std::make_unique<ScopeStack>();
		return ParseTranslationUnit();
	}

	//<translation-unit> ::= { <external-declaration> }*
	bool Parser::ParseTranslationUnit()
	{
		while (current_token->IsNot(TokenKind::eof))
		{
			if (!ParseExternalDeclaration()) return false;
		}
		return true;
	}

	bool Parser::ParseExternalDeclaration()
	{
		DeclSpecInfo decl_spec{};
		if (!ParseDeclSpec(decl_spec)) return false;

		// Typedef
		if (decl_spec.storage == Storage::Typedef)
		{
			auto typedef_decls = ParseTypedefDeclaration(decl_spec);
			if (typedef_decls.empty()) return false;
			for(auto&& typedef_decl : typedef_decls)
				ast->tr_unit->AddExternalDeclaration(std::move(typedef_decl));
		}

		return false;
	}

	std::vector<std::unique_ptr<TypedefDeclAST>> Parser::ParseTypedefDeclaration(DeclSpecInfo& decl_spec)
	{
		bool first = true;
		std::vector<std::unique_ptr<TypedefDeclAST>> typedefs{};
		while (!Consume(TokenKind::semicolon)) 
		{
			if (!first && !Consume(TokenKind::comma)) 
			{
				//diag
				return {};
			}
			first = false;

			DeclaratorInfo typedef_info{};
			ParseDeclarator(decl_spec, typedef_info);

			if (typedef_info.name.empty())
			{
				//diag
				return {};
			}
			
			//push_scope(get_ident(ty->name))->type_def = ty;
		}
		
		return {};
	}

	bool Parser::ParseDeclSpec(DeclSpecInfo& decl_spec)
	{
		decl_spec = DeclSpecInfo{};
		while (current_token->IsDeclSpec())
		{
			TokenKind kind = current_token->GetKind();
			if (current_token->IsStorageSpecifier())
			{
				if (decl_spec.storage != Storage::None)
				{
					//diag
					return false;
				}

				switch (kind)
				{
				case TokenKind::KW_static:
					decl_spec.storage = Storage::Static;
					break;
				case TokenKind::KW_register:
					decl_spec.storage = Storage::Register;
					break;
				case TokenKind::KW_typedef:
					decl_spec.storage = Storage::Typedef;
					break;
				case TokenKind::KW__Thread_local:
					decl_spec.storage = Storage::ThreadLocal;
					break;
				case TokenKind::KW_extern:
					decl_spec.storage = Storage::Extern;
					break;
				}
			}
			//ignore for now, later add support: atomic, tls, alignas
			if (Consume(
				TokenKind::KW_auto, TokenKind::KW_register, TokenKind::KW__Atomic,
				TokenKind::KW__Alignas, TokenKind::KW__Thread_local))
				continue;

			if (Consume(TokenKind::KW_const))
			{
				decl_spec.qtype.AddConst();
				continue;
			}
			if (Consume(TokenKind::KW_volatile))
			{
				decl_spec.qtype.AddVolatile();
				continue;
			}

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
				decl_spec.qtype.SetRawType(types::Void);
				break;
			case BOOL:
				decl_spec.qtype.SetRawType(types::Bool);
				break;
			case CHAR:
			case SIGNED + CHAR:
				decl_spec.qtype.SetRawType(types::Char);
				break;
			case UNSIGNED + CHAR:
				decl_spec.qtype.SetRawType(types::UnsignedChar);
				break;
			case SHORT:
			case SHORT + INT:
			case SIGNED + SHORT:
			case SIGNED + SHORT + INT:
				decl_spec.qtype.SetRawType(types::Short);
				break;
			case UNSIGNED + SHORT:
			case UNSIGNED + SHORT + INT:
				decl_spec.qtype.SetRawType(types::UnsignedShort);
				break;
			case INT:
			case SIGNED:
			case SIGNED + INT:
				decl_spec.qtype.SetRawType(types::Int);
				break;
			case UNSIGNED:
			case UNSIGNED + INT:
				decl_spec.qtype.SetRawType(types::UnsignedInt);
				break;
			case LONG:
			case LONG + INT:
			case SIGNED + LONG:
			case SIGNED + LONG + INT:
				decl_spec.qtype.SetRawType(types::Long);
				break;
			case LONG + LONG:
			case LONG + LONG + INT:
			case SIGNED + LONG + LONG:
			case SIGNED + LONG + LONG + INT:
				decl_spec.qtype.SetRawType(types::LongLong);
				break;
			case UNSIGNED + LONG:
			case UNSIGNED + LONG + INT:
				decl_spec.qtype.SetRawType(types::UnsignedLong);
				break;
			case UNSIGNED + LONG + LONG:
			case UNSIGNED + LONG + LONG + INT:
				decl_spec.qtype.SetRawType(types::UnsignedLongLong);
				break;
			case FLOAT:
				decl_spec.qtype.SetRawType(types::Float);
				break;
			case DOUBLE:
				decl_spec.qtype.SetRawType(types::Double);
				break;
			case LONG + DOUBLE:
				decl_spec.qtype.SetRawType(types::LongDouble);
				break;
			default:
				//diag 
				return false;
			}
		}
		return true;
	}

	//declarator = pointers ("(" ident ")" | "(" declarator ")" | ident) type-suffix
	bool Parser::ParseDeclarator(DeclSpecInfo const& decl_spec, DeclaratorInfo& declarator)
	{
		declarator.qtype = decl_spec.qtype;
		ParsePointers(declarator.qtype);

		if (Consume(TokenKind::left_round))
		{
			//#todo handle ()
			return false;
		}

		if (current_token->Is(TokenKind::identifier))
		{
			declarator.name = current_token->GetIdentifier();
			declarator.loc = current_token->GetLocation();
			++current_token;
		}
		ParseTypeSuffix(declarator.qtype);
		return true;
	}

	//pointers = ("*" ("const" | "volatile" | "restrict")*)*
	void Parser::ParsePointers(QualifiedType& qtype)
	{
		while (Consume(TokenKind::star))
		{
			PointerType ptr_type(qtype);
			qtype.SetRawType(ptr_type);
			if (current_token->Is(TokenKind::KW_const))
			{
				qtype.AddConst();
				++current_token;
			}
			if (current_token->Is(TokenKind::KW_volatile))
			{
				qtype.AddVolatile();
				++current_token;
			}
			//add restrict later
		}
	}
	void Parser::ParseTypeSuffix(QualifiedType& type)
	{
		if (Consume(TokenKind::left_round))
		{
			// func-params = ("void" | param ("," param)* ("," "...")?)? ")"
			// param       = declspec declarator
		}
		else if (Consume(TokenKind::left_square))
		{
			// array-dimensions = "["("static" | "restrict")* const-expr? "]" type-suffix
			while (Consume(TokenKind::KW_static, TokenKind::KW_break));
			if (Consume(TokenKind::right_square)) 
			{
				ArrayType arr_type(type);
				type.SetRawType(arr_type);
				ParseTypeSuffix(type);
				return;
			}
			
		}
		
	}

}

