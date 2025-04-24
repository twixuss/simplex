#pragma once
#include "token.h"

#include <tl/list.h>

struct Lexer {
	static Lexer create(String source);
	
	Token next_token();

private:
	String source;
	utf8 *cursor;
	
	void print_invalid_character_error();
	
	void next();

};
