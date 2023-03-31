#ifndef TOKEN
#define TOKEN(X)
#endif
#ifndef PUNCTUATOR
#define PUNCTUATOR(X,Y) TOKEN(X)
#endif
#ifndef KEYWORD
#define KEYWORD(X) TOKEN(KW_##X)
#endif

#ifndef PP_KEYWORD
#define PP_KEYWORD(X) TOKEN(PP_##X)
#endif

TOKEN(unknown)
TOKEN(eof)
TOKEN(newline)
TOKEN(eod)
TOKEN(comment)
TOKEN(number)
TOKEN(identifier)
TOKEN(string_literal)

PUNCTUATOR(left_square, "[")
PUNCTUATOR(right_square, "]")
PUNCTUATOR(left_round, "(")
PUNCTUATOR(right_round, ")")
PUNCTUATOR(left_brace, "{")
PUNCTUATOR(right_brace, "}")
PUNCTUATOR(period, ".")
PUNCTUATOR(ellipsis, "...")
PUNCTUATOR(amp, "&")
PUNCTUATOR(amp_amp, "&&")
PUNCTUATOR(amp_equal, "&=")
PUNCTUATOR(star, "*")
PUNCTUATOR(starEqual, "*=")
PUNCTUATOR(plus, "+")
PUNCTUATOR(plusPlus, "++")
PUNCTUATOR(plusEqual, "+=")
PUNCTUATOR(minus, "-")
PUNCTUATOR(arrow, "->")
PUNCTUATOR(minus_minus, "--")
PUNCTUATOR(minus_equal, "-=")
PUNCTUATOR(tilde, "~")
PUNCTUATOR(exclaim, "!")
PUNCTUATOR(not_equal, "!=")
PUNCTUATOR(slash, "/")
PUNCTUATOR(slash_equal, "/=")
PUNCTUATOR(modulo, "%")
PUNCTUATOR(modulo_equal, "%=")
PUNCTUATOR(less, "<")
PUNCTUATOR(less_less, "<<")
PUNCTUATOR(less_equal, "<=")
PUNCTUATOR(less_less_equal, "<<=")
PUNCTUATOR(greater, ">")
PUNCTUATOR(greater_greater, ">>")
PUNCTUATOR(greater_equal, ">=")
PUNCTUATOR(greater_great_equal, ">>=")
PUNCTUATOR(caret, "^")
PUNCTUATOR(caret_equal, "^=")
PUNCTUATOR(pipe, "|")
PUNCTUATOR(pipe_pipe, "||")
PUNCTUATOR(pipe_equal, "|=")
PUNCTUATOR(question, "?")
PUNCTUATOR(colon, ":")
PUNCTUATOR(semicolon, ";")
PUNCTUATOR(equal, "=")
PUNCTUATOR(equal_equal, "==")
PUNCTUATOR(comma, ",")
PUNCTUATOR(hash, "#")
PUNCTUATOR(hash_hash, "##")

KEYWORD(while)
KEYWORD(break)
KEYWORD(switch)
KEYWORD(case)
KEYWORD(for)
KEYWORD(goto)
KEYWORD(continue)
KEYWORD(do)
KEYWORD(if)
KEYWORD(else)
KEYWORD(return)
KEYWORD(enum)
KEYWORD(struct)
KEYWORD(const)
KEYWORD(typedef)
KEYWORD(bool)
KEYWORD(char)
KEYWORD(int)
KEYWORD(unsigned)
KEYWORD(long)
KEYWORD(float)
KEYWORD(double)
KEYWORD(void)

PP_KEYWORD(if)
PP_KEYWORD(else)
PP_KEYWORD(elif)
PP_KEYWORD(ifdef)
PP_KEYWORD(ifndef)
PP_KEYWORD(elifdef)
PP_KEYWORD(elifndef)
PP_KEYWORD(endif)
PP_KEYWORD(defined)
PP_KEYWORD(include)
PP_KEYWORD(define)
PP_KEYWORD(undef)

#undef PP_KEYWORD
#undef KEYWORD
#undef PUNCTUATOR
#undef TOKEN

