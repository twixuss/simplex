#pragma once
#include <tl/common.h>
#include <tl/hash_map.h>
#include <tl/source_location.h>

namespace tl {
namespace c_parser {

struct Macro {
	Span<utf8> name;
	Span<utf8> value;
};

enum class IncludeForm {
	quoted,
	angle_bracket,
};

enum class TokenKind : u32 {
	new_line = '\n',
	name = 'a',
	unknown = 0x80'00'00'00,
	preprocessor_include,
	preprocessor_define,
	preprocessor_if,
	preprocessor_ifdef,
	preprocessor_ifndef,
	preprocessor_elif,
	preprocessor_elifdef,
	preprocessor_elifndef,
	preprocessor_else,
	preprocessor_endif,
};

struct Token {
	TokenKind kind;
	Span<utf8> string;
};

template <
	CallableAnyRet<Macro>                  OnParsedDefine  = decltype([](Macro){}),
	CallableAnyRet<Span<utf8>,IncludeForm> OnParsedInclude = decltype([](Span<utf8>,IncludeForm){})>
struct PreprocessSourceOptions {
	Span<Macro> macros = {};
	OnParsedDefine on_parsed_define = {};
	OnParsedInclude on_parsed_include = {};
};

/*
#define x(name)
TL_C_PARSER_ENUMERATE_PREPROCESS_ERROR_KIND(x)
#undef x
*/
#define TL_C_PARSER_ENUMERATE_PREPROCESS_ERROR_KIND(x) \
	x(unexpected_end_of_source) \
	x(unexpected_character) \
	x(invalid_include_form) \
	x(if_else_unmatched) \

enum class PreprocessErrorKind {
	#define x(name) name,
	TL_C_PARSER_ENUMERATE_PREPROCESS_ERROR_KIND(x)
	#undef x
};

inline umm append(StringBuilder &builder, PreprocessErrorKind const &error) {
	switch (error) {
		#define x(name) case PreprocessErrorKind::name: return append(builder, #name);
		TL_C_PARSER_ENUMERATE_PREPROCESS_ERROR_KIND(x)
		#undef x
	}
	return append_format(builder, "(unknown PreprocessError {})", (int)error);
}

struct PreprocessError {
	PreprocessErrorKind kind;
	Token token;
};

struct Preprocessor {
	Span<utf8> source;
	utf8 *cursor;
	
	bool is_ident(char c) {
		return
			('a' <= * cursor && *cursor <= 'z') ||
			('A' <= * cursor && *cursor <= 'Z') ||
			('0' <= * cursor && *cursor <= '9') ||
			*cursor == '_';
	}

	bool skip_spaces() {
		while (1) {
			if (cursor == source.end()) {
				return false;
			}
						
			switch (*cursor) {
				case ' ':
				case '\t':
					++cursor;
					continue;
				default:
					return true;
			}
		}
	}
	bool skip_ident() {
		while (1) {
			if (cursor == source.end()) {
				return false;
			}
						
			if (('a' <= * cursor && *cursor <= 'z') ||
				('A' <= * cursor && *cursor <= 'Z') ||
				('0' <= * cursor && *cursor <= '9') ||
				*cursor == '_') 
			{
				++cursor;
				continue;
			} else {
				return true;
			}
		}
	}
	void skip_until_new_line() {
		while (cursor != source.end() && *cursor != '\n' && *cursor != '\r') {
			++cursor;
		}
	}

	Token next_token() {
		skip_spaces();

		Token token;
		token.string.data = cursor;
		switch (*cursor) {
			case '#': {
				++cursor;
				auto start = cursor;
				while (is_ident(*cursor)) {
					++cursor;
				}
				auto directive = Span(start, cursor);

				if (false) {}
				else if (directive == u8"include"s ) { token.kind = TokenKind::preprocessor_include ; }
				else if (directive == u8"define"s  ) { token.kind = TokenKind::preprocessor_define  ; }
				else if (directive == u8"if"s      ) { token.kind = TokenKind::preprocessor_if      ; }
				else if (directive == u8"ifdef"s   ) { token.kind = TokenKind::preprocessor_ifdef   ; }
				else if (directive == u8"ifndef"s  ) { token.kind = TokenKind::preprocessor_ifndef  ; }
				else if (directive == u8"elif"s    ) { token.kind = TokenKind::preprocessor_elif    ; }
				else if (directive == u8"elifdef"s ) { token.kind = TokenKind::preprocessor_elifdef ; }
				else if (directive == u8"elifndef"s) { token.kind = TokenKind::preprocessor_elifndef; }
				else if (directive == u8"else"s    ) { token.kind = TokenKind::preprocessor_else    ; }
				else if (directive == u8"endif"s   ) { token.kind = TokenKind::preprocessor_endif   ; }
				else { token.kind = TokenKind::unknown; }
				break;
			}
			case '\n': {
				token.kind = TokenKind::new_line;
				break;
			}
			default: {
				token.kind = TokenKind::unknown;
				break;
			}
		}
		token.string.set_end(cursor);
	}
	
	HashMap<Span<utf8>, Span<utf8>> active_macros;
	
	struct If {
		bool enabled = false;
		bool has_else = false;
	};

	List<If> if_stack;

	int evaluate(Span<utf8> expression) {
		not_implemented("evaluate {} not implemented", expression);
		return 0;
	}

	Result<Empty, PreprocessErrorKind> on_parsed_if(Span<utf8> condition) {
		if_stack.add({
			.enabled = !!evaluate(condition),
		});
		return Empty{};
	}
	Result<Empty, PreprocessErrorKind> on_parsed_elif(Span<utf8> condition) {
		if (if_stack.count == 0)
			return PreprocessErrorKind::if_else_unmatched;
		if (if_stack.back().has_else)
			return PreprocessErrorKind::if_else_unmatched;

		if_stack.back().enabled = evaluate(condition);
		return Empty{};
	}
	Result<Empty, PreprocessErrorKind> on_parsed_else() {
		if (if_stack.count == 0)
			return PreprocessErrorKind::if_else_unmatched;
		if (if_stack.back().has_else)
			return PreprocessErrorKind::if_else_unmatched;

		if_stack.back().enabled ^= 1;
		if_stack.back().has_else = true;
		return Empty{};
	}
	Result<Empty, PreprocessErrorKind> on_parsed_endif() {
		if (!if_stack.pop())
			return PreprocessErrorKind::if_else_unmatched;
		return Empty{};
	}

	template <
		CallableAnyRet<Macro>                  OnParsedDefine  = decltype([](Macro){}),
		CallableAnyRet<Span<utf8>,IncludeForm> OnParsedInclude = decltype([](Span<utf8>,IncludeForm){})>
	Result<Empty, PreprocessError> preprocess_source(Span<utf8> source, PreprocessSourceOptions<OnParsedDefine, OnParsedInclude> options = {}) {

		this->source = source;
		cursor = source.begin();

	
		active_macros.clear();
		for (auto macro : options.macros) {
			active_macros.insert(macro.name, macro.value);
		}

		if_stack.clear();

		PreprocessError error;
		Token token;

		#define error(kind_) \
			return PreprocessError{ .kind = kind_, .token = token }
		
		#define next() \
			token = next_token()

		#define must_next() \
			next(); \
			if (token.kind == TokenKind::unknown) \
				error(PreprocessErrorKind::unexpected_end_of_source)

		while (1) {
			if (cursor >= source.end())
				break;

			next();

			switch (token.kind) {
				case TokenKind::preprocessor_define: {
					must_next();

					if (token.kind != TokenKind::name) {
						error(PreprocessErrorKind::invalid_directive);
					}
					auto name = token;
					
					must_next();

					List<Token> value;
					while (token.kind != TokenKind::new_line && token.kind != TokenKind::unknown) {
						value.add(token);
						next();
					}

					active_macros.get_or_insert(name.string) = value;

					options.on_parsed_define({name.string, value});
					break;
				}
				case TokenKind::preprocessor_include: {
					cursor += include_s.count;
					
					if (!skip_spaces())
						return PreprocessErrorKind::unexpected_end_of_source;

					auto path_start = cursor;
					skip_until_new_line();
					auto path = trim(Span(path_start, cursor), [&](char c) { return is_whitespace(c); });
					IncludeForm form;
					
					if (starts_with(path, u8"<"s) && ends_with(path, u8">"s)) {
						path = {path.begin() + 1, path.end() - 1};
						form = IncludeForm::angle_bracket;
					} else if (starts_with(path, u8"\""s) && ends_with(path, u8"\""s)) {
						path = {path.begin() + 1, path.end() - 1};
						form = IncludeForm::quoted;
					} else {
						return PreprocessErrorKind::invalid_include_form;
					}

					options.on_parsed_include(path, form);
					break;
				}
				case TokenKind::preprocessor_ifdef: {
					cursor += ifdef_s.count;

					auto condition_start = cursor;
					skip_until_new_line();
					auto condition = trim(Span(condition_start, cursor), [&](char c) { return is_whitespace(c); });
					
					scoped(temporary_allocator_and_checkpoint);
					condition = tformat(u8"defined({})"s, condition);

					if (auto result = on_parsed_if(condition); result.is_error())
						return result.error();
					break;
				}
				case TokenKind::preprocessor_ifndef: {
					cursor += ifndef_s.count;

					auto condition_start = cursor;
					skip_until_new_line();
					auto condition = trim(Span(condition_start, cursor), [&](char c) { return is_whitespace(c); });
					
					scoped(temporary_allocator_and_checkpoint);
					condition = tformat(u8"!defined({})"s, condition);

					if (auto result = on_parsed_if(condition); result.is_error())
						return result.error();
					break;
				}
				case TokenKind::preprocessor_if: {
					cursor += if_s.count;

					auto condition_start = cursor;
					skip_until_new_line();
					auto condition = trim(Span(condition_start, cursor), [&](char c) { return is_whitespace(c); });
					
					if (auto result = on_parsed_if(condition); result.is_error())
						return result.error();
					break;
				}
				case TokenKind::preprocessor_elifdef: {
					cursor += elifdef_s.count;

					auto condition_start = cursor;
					skip_until_new_line();
					auto condition = trim(Span(condition_start, cursor), [&](char c) { return is_whitespace(c); });
					
					scoped(temporary_allocator_and_checkpoint);
					condition = tformat(u8"defined({})"s, condition);

					if (auto result = on_parsed_elif(condition); result.is_error())
						return result.error();
					break;
				}
				case TokenKind::preprocessor_elifndef: {
					cursor += elifndef_s.count;

					auto condition_start = cursor;
					skip_until_new_line();
					auto condition = trim(Span(condition_start, cursor), [&](char c) { return is_whitespace(c); });
					
					scoped(temporary_allocator_and_checkpoint);
					condition = tformat(u8"!defined({})"s, condition);

					if (auto result = on_parsed_elif(condition); result.is_error())
						return result.error();
					break;
				}
				case TokenKind::preprocessor_elif: {
					cursor += elif_s.count;

					auto condition_start = cursor;
					skip_until_new_line();
					auto condition = trim(Span(condition_start, cursor), [&](char c) { return is_whitespace(c); });
					
					if (auto result = on_parsed_elif(condition); result.is_error())
						return result.error();
					break;
				}
				case TokenKind::preprocessor_else: {
					cursor += else_s.count;
					skip_until_new_line();
					
					if (auto result = on_parsed_else(); result.is_error())
						return result.error();
					break;
				}
				case TokenKind::preprocessor_endif: {
					cursor += endif_s.count;
					skip_until_new_line();
					
					if (auto result = on_parsed_endif(); result.is_error())
						return result.error();
					break;
				}
			}
		}

		while (cursor < source.end()) {
			auto rest = [&] { return Span(cursor, source.end()); };

			switch (*cursor) {
				case ' ':
				case '\t':
				case '\r':
				case '\n': {
					++cursor;
					continue;
				}

				case '#': {
					++cursor;
					auto define_s = "define"s;
					auto include_s = "include"s;
					auto if_s = "if"s;
					auto ifdef_s = "ifdef"s;
					auto ifndef_s = "ifndef"s;
					auto elif_s = "elif"s;
					auto elifdef_s = "elifdef"s;
					auto elifndef_s = "elifndef"s;
					auto else_s = "else"s;
					auto endif_s = "endif"s;
					break;
				}
				default: {
					++cursor;
					break;
				}
			}
		}
		return Empty{};
	}
};

}
}