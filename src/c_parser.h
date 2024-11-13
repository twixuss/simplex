#pragma once
#include <tl/common.h>
#include <tl/hash_map.h>

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

enum class TokenKind {
	
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
TL_C_PARSER_ENUMERATE_PREPROCESS_ERROR(x)
#undef x
*/
#define TL_C_PARSER_ENUMERATE_PREPROCESS_ERROR(x) \
	x(unexpected_end_of_source) \
	x(unexpected_character) \
	x(invalid_include_form) \
	x(if_else_unmatched) \

enum class PreprocessError {
	#define x(name) name,
	TL_C_PARSER_ENUMERATE_PREPROCESS_ERROR(x)
	#undef x
};

inline umm append(StringBuilder &builder, PreprocessError const &error) {
	switch (error) {
		#define x(name) case PreprocessError::name: return append(builder, #name);
		TL_C_PARSER_ENUMERATE_PREPROCESS_ERROR(x)
		#undef x
	}
	return append_format(builder, "(unknown PreprocessError {})", (int)error);
}

template <
	CallableAnyRet<Macro>                  OnParsedDefine  = decltype([](Macro){}),
	CallableAnyRet<Span<utf8>,IncludeForm> OnParsedInclude = decltype([](Span<utf8>,IncludeForm){})>
Result<Empty, PreprocessError> preprocess_source(Span<utf8> source, PreprocessSourceOptions<OnParsedDefine, OnParsedInclude> options = {}) {

	utf8 *cursor = source.begin();

	auto skip_spaces = [&] {
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
	};
	auto skip_ident = [&] {
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
	};

	auto skip_until_new_line = [&] {
		while (cursor != source.end() && *cursor != '\n' && *cursor != '\r') {
			++cursor;
		}
	};
	
	HashMap<Span<utf8>, Span<utf8>> active_macros;
	for (auto macro : options.macros) {
		active_macros.insert(macro.name, macro.value);
	}

	struct If {
		bool enabled = false;
		bool has_else = false;
	};

	List<If> if_stack;
	
	auto evaluate = [&] (Span<utf8> expression) -> int {
		not_implemented("evaluate {} not implemented", expression);
		return 0;
	};

	auto on_parsed_if = [&] (Span<utf8> condition) -> Result<Empty, PreprocessError> {
		if_stack.add({
			.enabled = !!evaluate(condition),
		});
		return Empty{};
	};
	auto on_parsed_elif = [&] (Span<utf8> condition) -> Result<Empty, PreprocessError> {
		if (if_stack.count == 0)
			return PreprocessError::if_else_unmatched;
		if (if_stack.back().has_else)
			return PreprocessError::if_else_unmatched;

		if_stack.back().enabled = evaluate(condition);
		return Empty{};
	};
	auto on_parsed_else = [&] () -> Result<Empty, PreprocessError> {
		if (if_stack.count == 0)
			return PreprocessError::if_else_unmatched;
		if (if_stack.back().has_else)
			return PreprocessError::if_else_unmatched;

		if_stack.back().enabled ^= 1;
		if_stack.back().has_else = true;
		return Empty{};
	};
	auto on_parsed_endif = [&] () -> Result<Empty, PreprocessError> {
		if (!if_stack.pop())
			return PreprocessError::if_else_unmatched;
		return Empty{};
	};

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
				if (starts_with(rest(), define_s)) {
					cursor += define_s.count;
					
					if (!skip_spaces())
						return PreprocessError::unexpected_end_of_source;

					auto name_start = cursor;
					if (!skip_ident())
						return PreprocessError::unexpected_end_of_source;
					auto name = Span(name_start, cursor);

					if (!skip_spaces())
						return PreprocessError::unexpected_end_of_source;

					auto value_start = cursor;
					skip_until_new_line();
					auto value = Span(value_start, cursor);

					active_macros.get_or_insert(name) = value;

					options.on_parsed_define({name, value});
				} else if (starts_with(rest(), include_s)) {
					cursor += include_s.count;
					
					if (!skip_spaces())
						return PreprocessError::unexpected_end_of_source;

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
						return PreprocessError::invalid_include_form;
					}

					options.on_parsed_include(path, form);
				} else if (starts_with(rest(), ifdef_s)) {
					cursor += ifdef_s.count;

					auto condition_start = cursor;
					skip_until_new_line();
					auto condition = trim(Span(condition_start, cursor), [&](char c) { return is_whitespace(c); });
					
					scoped(temporary_allocator_and_checkpoint);
					condition = tformat(u8"defined({})"s, condition);

					if (auto result = on_parsed_if(condition); result.is_error())
						return result.error();
				} else if (starts_with(rest(), ifndef_s)) {
					cursor += ifndef_s.count;

					auto condition_start = cursor;
					skip_until_new_line();
					auto condition = trim(Span(condition_start, cursor), [&](char c) { return is_whitespace(c); });
					
					scoped(temporary_allocator_and_checkpoint);
					condition = tformat(u8"!defined({})"s, condition);

					if (auto result = on_parsed_if(condition); result.is_error())
						return result.error();
				} else if (starts_with(rest(), if_s)) {
					cursor += if_s.count;

					auto condition_start = cursor;
					skip_until_new_line();
					auto condition = trim(Span(condition_start, cursor), [&](char c) { return is_whitespace(c); });
					
					if (auto result = on_parsed_if(condition); result.is_error())
						return result.error();
				} else if (starts_with(rest(), elifdef_s)) {
					cursor += elifdef_s.count;

					auto condition_start = cursor;
					skip_until_new_line();
					auto condition = trim(Span(condition_start, cursor), [&](char c) { return is_whitespace(c); });
					
					scoped(temporary_allocator_and_checkpoint);
					condition = tformat(u8"defined({})"s, condition);

					if (auto result = on_parsed_elif(condition); result.is_error())
						return result.error();
				} else if (starts_with(rest(), elifndef_s)) {
					cursor += elifndef_s.count;

					auto condition_start = cursor;
					skip_until_new_line();
					auto condition = trim(Span(condition_start, cursor), [&](char c) { return is_whitespace(c); });
					
					scoped(temporary_allocator_and_checkpoint);
					condition = tformat(u8"!defined({})"s, condition);

					if (auto result = on_parsed_elif(condition); result.is_error())
						return result.error();
				} else if (starts_with(rest(), elif_s)) {
					cursor += elif_s.count;

					auto condition_start = cursor;
					skip_until_new_line();
					auto condition = trim(Span(condition_start, cursor), [&](char c) { return is_whitespace(c); });
					
					if (auto result = on_parsed_elif(condition); result.is_error())
						return result.error();
				} else if (starts_with(rest(), else_s)) {
					cursor += else_s.count;
					skip_until_new_line();
					
					if (auto result = on_parsed_else(); result.is_error())
						return result.error();
				} else if (starts_with(rest(), endif_s)) {
					cursor += endif_s.count;
					skip_until_new_line();
					
					if (auto result = on_parsed_endif(); result.is_error())
						return result.error();
				}
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

}
}