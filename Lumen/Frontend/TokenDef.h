#pragma once

#ifndef TOKEN
#define TOKEN(X)
#endif
#ifndef PUNCTUATOR
#define PUNCTUATOR(X,Y) TOKEN(X)
#endif
#ifndef KEYWORD
#define KEYWORD(X) TOKEN(X)
#endif

#ifndef PP_KEYWORD
#define PP_KEYWORD(X) TOKEN(PP_##X)
#endif

TOKEN(Unknown)
TOKEN(EoF)
TOKEN(Comment)
TOKEN(Number)
TOKEN(Identifier)
TOKEN(String)

PUNCTUATOR(LeftSquare, "[")
PUNCTUATOR(RightSquare, "]")
PUNCTUATOR(LeftRound, "(")
PUNCTUATOR(RightRound, ")")
PUNCTUATOR(LeftBrace, "{")
PUNCTUATOR(RightBrace, "}")
PUNCTUATOR(Period, ".")
PUNCTUATOR(Ellipsis, "...")
PUNCTUATOR(Amp, "&")
PUNCTUATOR(AmpAmp, "&&")
PUNCTUATOR(AmpEqual, "&=")
PUNCTUATOR(Star, "*")
PUNCTUATOR(StarEqual, "*=")
PUNCTUATOR(Plus, "+")
PUNCTUATOR(PlusPlus, "++")
PUNCTUATOR(PlusEqual, "+=")
PUNCTUATOR(Minus, "-")
PUNCTUATOR(Arrow, "->")
PUNCTUATOR(MinusMinus, "--")
PUNCTUATOR(MinusEqual, "-=")
PUNCTUATOR(Tilde, "~")
PUNCTUATOR(Exclaim, "!")
PUNCTUATOR(ExclaimEqual, "!=")
PUNCTUATOR(Slash, "/")
PUNCTUATOR(SlashEqual, "/=")
PUNCTUATOR(Modulo, "%")
PUNCTUATOR(ModuloEqual, "%=")
PUNCTUATOR(Less, "<")
PUNCTUATOR(LessLess, "<<")
PUNCTUATOR(LessEqual, "<=")
PUNCTUATOR(LessLessEqual, "<<=")
PUNCTUATOR(Greater, ">")
PUNCTUATOR(GreaterGreater, ">>")
PUNCTUATOR(GreaterEqual, ">=")
PUNCTUATOR(GreaterGreatErequal, ">>=")
PUNCTUATOR(Caret, "^")
PUNCTUATOR(CaretEqual, "^=")
PUNCTUATOR(Pipe, "|")
PUNCTUATOR(PipePipe, "||")
PUNCTUATOR(PipeEqual, "|=")
PUNCTUATOR(Question, "?")
PUNCTUATOR(Colon, ":")
PUNCTUATOR(Semicolon, ";")
PUNCTUATOR(Equal, "=")
PUNCTUATOR(EqualEqual, "==")
PUNCTUATOR(Comma, ",")

KEYWORD(While)
KEYWORD(Break)
KEYWORD(Switch)
KEYWORD(Case)
KEYWORD(For)
KEYWORD(Goto)
KEYWORD(Continue)
KEYWORD(Do)
KEYWORD(If)
KEYWORD(Else)
KEYWORD(Return)
KEYWORD(Enum)
KEYWORD(Struct)
KEYWORD(Const)
KEYWORD(Typedef)
KEYWORD(Bool)
KEYWORD(Char)
KEYWORD(Int)
KEYWORD(Unsigned)
KEYWORD(Long)
KEYWORD(Float)
KEYWORD(Double)
KEYWORD(Void)


PP_KEYWORD(If)
PP_KEYWORD(Ifdef)
PP_KEYWORD(Ifndef)
PP_KEYWORD(Elif)
PP_KEYWORD(Elifdef)
PP_KEYWORD(Elifndef)
PP_KEYWORD(Else)
PP_KEYWORD(Endif)
PP_KEYWORD(Defined)
PP_KEYWORD(Include)
PP_KEYWORD(Define)
PP_KEYWORD(Undef)

#undef PP_KEYWORD
#undef KEYWORD
#undef PUNCTUATOR
#undef TOKEN

