#include "Parser.h"
#include "Lexer.h"
#include "AST.h"
#include "ScopeStack.h"
#include "Diagnostics.h"
#include "Core/Defines.h"

namespace lucc
{
	namespace builtin_types
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
		QualifiedType qtype = builtin_types::Int;
		Storage storage = Storage::None;
		FunctionSpecifier func_spec = FunctionSpecifier::None;
	};
	struct Parser::DeclaratorInfo
	{
		std::string name = "";
		QualifiedType qtype{};
		SourceLocation loc;
	};
	struct Parser::DeclarationInfo
	{
		DeclarationInfo() {}
		DeclarationInfo(DeclSpecInfo const& decl_spec, DeclaratorInfo const& declarator)
			: name(declarator.name), qtype(declarator.qtype), loc(declarator.loc),
			  align(decl_spec.align), storage(decl_spec.storage), func_spec(decl_spec.func_spec)
		{}
		std::string name = "";
		QualifiedType qtype{};
		SourceLocation loc;
		size_t align = 0;
		Storage storage = Storage::None;
		FunctionSpecifier func_spec = FunctionSpecifier::None;
	};

	Parser::Parser(std::vector<Token> const& _tokens)
		: tokens(_tokens), current_token(tokens.begin())
	{}

	Parser::~Parser() = default;

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
			return true;
		}

		DeclaratorInfo declarator_info{};
		ParseDeclarator(decl_spec, declarator_info);
		if (declarator_info.qtype->Is(TypeKind::Function))
		{
			auto func_decl = ParseFunctionDeclaration(DeclarationInfo(decl_spec, declarator_info));
			if (func_decl) ast->tr_unit->AddExternalDeclaration(std::move(func_decl));
			return func_decl != nullptr;
		}
		else //global variable
		{

		}

		return false;
	}

	std::vector<std::unique_ptr<TypedefDeclAST>> Parser::ParseTypedefDeclaration(DeclSpecInfo const& decl_spec)
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
			typedefs.push_back(std::make_unique<TypedefDeclAST>(typedef_info.qtype, typedef_info.name));
			
			Var& typedef_var = scope_stack->AddVar(typedef_info.name);
			typedef_var.kind = Var::Kind::Typedef;
			typedef_var.type_def = typedef_info.qtype;
		}
		
		return typedefs;
	}

	std::unique_ptr<FunctionDeclAST> Parser::ParseFunctionDeclaration(DeclarationInfo const& declaration)
	{
		if (Consume(TokenKind::semicolon))
		{

		}

		return nullptr;
	}

	bool Parser::ParseDeclSpec(DeclSpecInfo& decl_spec, bool forbid_storage_specs)
	{
		decl_spec = DeclSpecInfo{};
		decl_spec.qtype = builtin_types::Int;

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
		while (current_token->IsDeclSpec())
		{
			if (current_token->IsStorageSpecifier())
			{
				if (forbid_storage_specs)
				{
					Report(diag::storage_specifier_forbidden_context, current_token->GetLocation());
					return false;
				}

				TokenKind kind = current_token->GetKind();
				++current_token;
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
				default:
					LU_UNREACHABLE();
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

			if (!current_token->IsDeclSpec()) break;

			TokenKind kind = current_token->GetKind();
			//#todo Add support for user-defined types

			//builtin types
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
			default: LU_UNREACHABLE();
			}
			++current_token;

			switch (counter) 
			{
			case VOID:
				decl_spec.qtype.SetRawType(builtin_types::Void);
				break;
			case BOOL:
				decl_spec.qtype.SetRawType(builtin_types::Bool);
				break;
			case CHAR:
			case SIGNED + CHAR:
				decl_spec.qtype.SetRawType(builtin_types::Char);
				break;
			case UNSIGNED + CHAR:
				decl_spec.qtype.SetRawType(builtin_types::UnsignedChar);
				break;
			case SHORT:
			case SHORT + INT:
			case SIGNED + SHORT:
			case SIGNED + SHORT + INT:
				decl_spec.qtype.SetRawType(builtin_types::Short);
				break;
			case UNSIGNED + SHORT:
			case UNSIGNED + SHORT + INT:
				decl_spec.qtype.SetRawType(builtin_types::UnsignedShort);
				break;
			case INT:
			case SIGNED:
			case SIGNED + INT:
				decl_spec.qtype.SetRawType(builtin_types::Int);
				break;
			case UNSIGNED:
			case UNSIGNED + INT:
				decl_spec.qtype.SetRawType(builtin_types::UnsignedInt);
				break;
			case LONG:
			case LONG + INT:
			case SIGNED + LONG:
			case SIGNED + LONG + INT:
				decl_spec.qtype.SetRawType(builtin_types::Long);
				break;
			case LONG + LONG:
			case LONG + LONG + INT:
			case SIGNED + LONG + LONG:
			case SIGNED + LONG + LONG + INT:
				decl_spec.qtype.SetRawType(builtin_types::LongLong);
				break;
			case UNSIGNED + LONG:
			case UNSIGNED + LONG + INT:
				decl_spec.qtype.SetRawType(builtin_types::UnsignedLong);
				break;
			case UNSIGNED + LONG + LONG:
			case UNSIGNED + LONG + LONG + INT:
				decl_spec.qtype.SetRawType(builtin_types::UnsignedLongLong);
				break;
			case FLOAT:
				decl_spec.qtype.SetRawType(builtin_types::Float);
				break;
			case DOUBLE:
				decl_spec.qtype.SetRawType(builtin_types::Double);
				break;
			case LONG + DOUBLE:
				decl_spec.qtype.SetRawType(builtin_types::LongDouble);
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
			//#todo handle recursive declarators ()
			return false;
		}

		if (current_token->Is(TokenKind::identifier))
		{
			declarator.name = current_token->GetIdentifier();
			declarator.loc = current_token->GetLocation();
			++current_token;
		}
		return ParseTypeSuffix(declarator.qtype);
	}

	//pointers = ("*" ("const" | "volatile" | "restrict")*)*
	bool Parser::ParsePointers(QualifiedType& qtype)
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
		return true;
	}

	// func-params = ("void" | param ("," param)* ("," "...")?)? ")"
	// param = declspec declarator
	// array-dimensions = "["("static" | "restrict")* const-expr? "]" type-suffix
	bool Parser::ParseTypeSuffix(QualifiedType& type)
	{
		if (Consume(TokenKind::left_round))
		{
			if (Consume(TokenKind::KW_void))
			{
				FuncType func_type(type);
				type.SetRawType(func_type);
				if (!Consume(TokenKind::right_round))
				{
					Report(diag::function_params_not_closed, current_token->GetLocation());
					return false;
				}
				else return true;
			}
			else
			{
				bool is_variadic = false;
				std::vector<FunctionParameter> param_types{};
				bool first = true;
				while (!Consume(TokenKind::right_round))
				{
					if (!first && !Consume(TokenKind::comma))
					{
						Report(diag::function_params_missing_coma, current_token->GetLocation());
						return false;
					}
					first = false;
					
					if (Consume(TokenKind::ellipsis))
					{
						is_variadic = true;
						if (!Consume(TokenKind::right_round))
						{
							Report(diag::variadic_params_not_last, current_token->GetLocation());
							return false;
						}
						else break;
					}

					DeclSpecInfo param_decl_spec{};
					if (!ParseDeclSpec(param_decl_spec)) return false;
					DeclaratorInfo param_declarator{};
					if (!ParseDeclarator(param_decl_spec, param_declarator)) return false;
					QualifiedType& qtype = param_declarator.qtype;
					if (qtype->Is(TypeKind::Void)) return false;
					else if (qtype->Is(TypeKind::Array))
					{
						ArrayType const& array_type = TypeCast<ArrayType const&>(*qtype);
						QualifiedType base_type = array_type.BaseQualifiedType();
						PointerType decayed_param_type(base_type);
						qtype = QualifiedType(decayed_param_type);
					}
					else if (qtype->Is(TypeKind::Function))
					{
						FuncType const& function_type = TypeCast<FuncType const&>(*qtype);
						PointerType decayed_param_type(function_type);
						qtype = QualifiedType(decayed_param_type);
					}
					param_types.emplace_back(param_declarator.name, param_declarator.qtype);
				}
				FuncType func_type(type, param_types, is_variadic);
				type.SetRawType(func_type);
				return true;
			}
		}
		else if (Consume(TokenKind::left_square))
		{
			while (Consume(TokenKind::KW_static, TokenKind::KW_restrict));
			if (Consume(TokenKind::right_square)) 
			{
				ArrayType arr_type(type);
				type.SetRawType(arr_type);
				return ParseTypeSuffix(type);
			}
			else if (current_token->Is(TokenKind::number))
			{
				size_t array_size = std::stoull(current_token->GetIdentifier().data(), nullptr, 0); //#todo check if the it succeeded
				
				ArrayType arr_type(type, array_size);
				type.SetRawType(arr_type);
				if (!Consume(TokenKind::right_square))
				{
					Report(diag::function_params_not_closed, current_token->GetLocation());
					return false;
				}
				else return ParseTypeSuffix(type);
			}
		}
		return true;
	}

}

