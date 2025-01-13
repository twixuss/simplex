#pragma once

/*
#define x(ch)
ENUMERATE_CHARS_ALPHA(x)
#undef x
*/
#define ENUMERATE_CHARS_ALPHA(x) \
	x('a') x('A') x('n') x('N') \
	x('b') x('B') x('o') x('O') \
	x('c') x('C') x('p') x('P') \
	x('d') x('D') x('q') x('Q') \
	x('e') x('E') x('r') x('R') \
	x('f') x('F') x('s') x('S') \
	x('g') x('G') x('t') x('T') \
	x('h') x('H') x('u') x('U') \
	x('i') x('I') x('v') x('V') \
	x('j') x('J') x('w') x('W') \
	x('k') x('K') x('x') x('X') \
	x('l') x('L') x('y') x('Y') \
	x('m') x('M') x('z') x('Z') \

/*
#define x(ch)
ENUMERATE_CHARS_DIGIT(x)
#undef x
*/
#define ENUMERATE_CHARS_DIGIT(x) \
	x('0') x('1') x('2') x('3') x('4') \
	x('5') x('6') x('7') x('8') x('9') \

/*
#define x(ch)
ENUMERATE_CHARS_HEX(x)
#undef x
*/
#define ENUMERATE_CHARS_HEX(x) \
	ENUMERATE_CHARS_DIGIT(x) \
	x('a') x('b') x('c') x('d') x('e') x('f') \
	x('A') x('B') x('C') x('D') x('E') x('F') \

/*
#define x(string)
ENUMERATE_DOUBLE_CHAR_TOKENS(x)
#undef x
*/
#define ENUMERATE_DOUBLE_CHAR_TOKENS(x) \
	x("==") \
	x("!=") \
	x(">=") \
	x("<=") \
	x("+=") \
	x("-=") \
	x("*=") \
	x("/=") \
	x("%=") \
	x("&=") \
	x("|=") \
	x("^=") \
	x("&&") \
	x("||") \
	x("<<") \
	x(">>") \
	x("..") \
	x("=>") \

/*
#define x(string)
ENUMERATE_TRIPLE_CHAR_TOKENS(x)
#undef x
*/
#define ENUMERATE_TRIPLE_CHAR_TOKENS(x) \
	x("<<=") \
	x(">>=") \


/*
#define x(name)
ENUMERATE_CONCRETE_BUILTIN_TYPES(x)
#undef x
*/
#define ENUMERATE_CONCRETE_BUILTIN_TYPES(x) \
	x(Type) \
	x(U8) \
	x(U16) \
	x(U32) \
	x(U64) \
	x(S8) \
	x(S16) \
	x(S32) \
	x(S64) \
	x(Bool) \
	x(String) \
	x(None) \

/*
#define x(name)
ENUMERATE_ABSTRACT_BUILTIN_TYPES(x)
#undef x
*/
#define ENUMERATE_ABSTRACT_BUILTIN_TYPES(x) \
	x(UnsizedInteger) \
	x(Overload) \

/*
#define x(name)
ENUMERATE_BUILTIN_TYPES(x)
#undef x
*/
#define ENUMERATE_BUILTIN_TYPES(x) \
	ENUMERATE_CONCRETE_BUILTIN_TYPES(x) \
	ENUMERATE_ABSTRACT_BUILTIN_TYPES(x) \

/*
#define x(name)
ENUMERATE_KEYWORDS(x)
#undef x
*/
#define ENUMERATE_KEYWORDS(x) \
	ENUMERATE_CONCRETE_BUILTIN_TYPES(x) \
	x(const) \
	x(let) \
	x(var) \
	x(return) \
	x(if) \
	x(then) \
	x(else) \
	x(false) \
	x(true) \
	x(while) \
	x(break) \
	x(continue) \
	x(match) \
	x(typeof) \
	x(inline) \
	x(noinline) \
	x(struct) \
	x(as) \
	x(none) \
	x(import) \

/*
#define x(name, token, precedence)
ENUMERATE_BINARY_OPERATIONS(x)
#undef x
*/
#define ENUMERATE_BINARY_OPERATIONS(x) \
	x(dot, "." , 9) \
	\
	x(as, "as", 8)\
    \
	x(mul, "*" , 7) \
	x(div, "/" , 7) \
	x(mod, "%" , 7) \
	\
	x(add, "+" , 6) \
	x(sub, "-" , 6) \
	\
	x(bor, "|" , 5) \
	x(ban, "&" , 5) \
	x(bxo, "^" , 5) \
	x(bsl, "<<", 5) \
	x(bsr, ">>", 5) \
	\
	x(equ, "==", 4) \
	x(neq, "!=", 4) \
	x(les, "<" , 4) \
	x(leq, "<=", 4) \
	x(grt, ">" , 4) \
	x(grq, ">=", 4) \
	\
	x(lan, "&&", 3) \
	x(lor, "||", 3) \
	\
	x(ran, "..", 2) \
	\
	x(ass, "=" , 1) \
	\
	x(addass, "+=" , 1) \
	x(subass, "-=" , 1) \
	x(mulass, "*=" , 1) \
	x(divass, "/=" , 1) \
	x(modass, "%=" , 1) \
	x(borass, "|=" , 1) \
	x(banass, "&=" , 1) \
	x(bxoass, "^=" , 1) \
	x(bslass, "<<=", 1) \
	x(bsrass, ">>=", 1) \

/*
#define x(name, token)
#define y(name)
ENUMERATE_UNARY_OPERATIONS(x, y)
#undef y
#undef x
*/
#define ENUMERATE_UNARY_OPERATIONS(x, y) \
	x(plus, "+") \
	x(minus, "-") \
	x(lnot, "!") \
	x(star, "*") \
	x(addr, "&") \
	x(typeof, "typeof") \
	y(pointer) \
	y(dereference) \

/*
#define x(name)
#define y(name, value)
ENUMERATE_TOKEN_KIND(x, y)
#undef y
#undef x
*/
#define ENUMERATE_TOKEN_KIND(x, y) \
	y(eof, '\0') \
	y(eol, '\n') \
	y(name, 'a') \
	y(string, '"') \
	y(number, '0') \
	y(directive, '#') \
	ENUMERATE_KEYWORDS(x) \

/*
#define x(name)
ENUMERATE_EXPRESSION_KIND(x)
#undef x
*/
#define ENUMERATE_EXPRESSION_KIND(x) \
	x(Block) \
	x(Call) \
	x(Definition) \
	x(IntegerLiteral) \
	x(BooleanLiteral) \
	x(NoneLiteral) \
	x(StringLiteral) \
	x(Lambda) \
	x(LambdaHead) \
	x(Name) \
	x(IfExpression) \
	x(BuiltinTypeName) \
	x(Binary) \
	x(Match) \
	x(Unary) \
	x(Struct) \
	x(ArrayType) \
	x(Subscript) \
	x(ArrayConstructor) \

/*
#define x(name)
ENUMERATE_STATEMENT_KIND(x)
#undef x
*/
#define ENUMERATE_STATEMENT_KIND(x) \
	x(IfStatement) \
	x(Return) \
	x(While) \
	x(Continue) \
	x(Break) \
	x(Import) \

/*
#define x(name)
ENUMERATE_NODE_KIND(x)
#undef x
*/
#define ENUMERATE_NODE_KIND(x) \
	ENUMERATE_EXPRESSION_KIND(x) \
	ENUMERATE_STATEMENT_KIND(x) \

/*
#define x(name)
ENUMERATE_INLINE_STATUS(x)
#undef x
*/
#define ENUMERATE_INLINE_STATUS \
	x(unspecified) \
	x(always) \
	x(never) \

/*
#define x(name)
ENUMERATE_EXECUTION_VALUE_KIND(x)
#undef x
*/
#define ENUMERATE_EXECUTION_VALUE_KIND \
	x(none) \
	x(U8) \
	x(U16) \
	x(U32) \
	x(U64) \
	x(S8) \
	x(S16) \
	x(S32) \
	x(S64) \
	x(UnsizedInteger) \
	x(Bool) \
	x(String) \
	x(lambda) \
	x(Type) \
	x(pointer) \
	x(break_) \
	x(continue_) \
	x(return_) \
	x(struct_) \
	x(array) \

