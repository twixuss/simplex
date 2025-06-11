#pragma once
#include "common.h"
#include "lexer.h"
#include "nodes_fwd.h"
#include "reporter.h"
#include "debug.h"

struct Imports {
	struct FileToImport {
		String path;
		String location;
	};
	GList<FileToImport> files_to_import;
	GHashSet<String> imported_files;

	void add_file(Imports::FileToImport file_to_import) {
		if (!imported_files.find(file_to_import.path)) {
			imported_files.insert(file_to_import.path, {});
			files_to_import.add(file_to_import);
		}
	}
};

extern LockProtected<Imports, SpinLock> imports;

struct Parser {
	enum class YieldResult {
		parsed_node,
		success,
		fail,
	};

	Lexer lexer;
	Token token;
	Token previous_token;
	Block *current_block = get_global_block_unprotected();
	While *current_loop = 0;
	Expression *current_container = 0;
	Reporter reporter;
	Node *last_parsed_node = 0;
	ReusableFiber fiber = {};
	Fiber parent_fiber = {};
	Node *result_node;
	YieldResult last_yield_result = YieldResult::fail;
	GList<utf8> extern_library = {};

	// Debug
	List<Token> all_tokens;

	List<utf8> unescape_string_or_fail(String string);

	template <class T>
	T finish_node(T node) {
		last_parsed_node = node;
		return node;
	}

	void report_last_parsed_node();

	struct ParseListOptions {
		bool call_next_after_finishing = true;
	};

	// Returns true if list was present.
	// Current token must be the opening token.
	bool parse_list(u64 opening, u64 separator, u64 closing, ParseListOptions options, auto fn) {
		if (token.kind != opening)
			return false;
		
		next();
		skip_lines();

		if (token.kind != closing) {
			while (true) {
				fn();
				
				skip_lines();

				if (token.kind == separator) {
					next();
					skip_lines();
					if (token.kind == closing) {
						break;
					}
					continue;
				}
				if (token.kind == closing) {
					break;
				}
			}
		}
		
		if (options.call_next_after_finishing)
			next();

		return true;
	}

	bool parse_template_parameter_list(Expression *parent, Block *template_parameters_block);
	
	struct NamedLambda {
		Expression *lambda_or_head = 0;
		String name = {};
	};

	NamedLambda parse_lambda();
	
	struct NamedStruct {
		Struct *Struct = 0;
		String name = {};
	};

	NamedStruct parse_struct();

	struct NamedEnum {
		Enum *Enum = 0;
		String name = {};
	};

	NamedEnum parse_enum();
	
	bool fail_due_to_unseparated_ambiguous_expression(Expression *expression, String separator);

	bool is_valid_name_part(TokenKind kind);

	void ensure_signature_validity_for_operators(Definition *definition);

	void parse_name(String *location, String *name);

	// Parses parse_expression_1 with binary operators and definitions.
	Expression *parse_expression(bool whitespace_is_skippable_before_binary_operator = false, u32 right_precedence = 0);
	// Parses parse_expression_0 plus member access.
	Expression *parse_expression_1();
	// Parses single-part expressions
	Expression *parse_expression_0();
	Node *parse_statement();
	Node *parse_statement_with_attributes();

	void yield(YieldResult result);
	void main();

	Node *parse_next_node();

	void link_constant_definition_to_initial_value(Definition *definition);

	void ensure_allowed_in_statement_context(Node *node);
	bool next();
	void expect(std::underlying_type_t<TokenKind> expected_kind);
	void expect_not(std::underlying_type_t<TokenKind> unexpected_kind);
	void expect(std::initializer_list<std::underlying_type_t<TokenKind>> expected_kinds);
	void expect_not(std::initializer_list<std::underlying_type_t<TokenKind>> unexpected_kinds);
	void skip_lines();

	void init(String source);

	void free();
};

bool parse_source(String source, auto on_parse_global_node) {
	#if ENABLE_ASSERTIONS
	auto &content_start_to_file_name = context_base->content_start_to_file_name;
	locked_use(content_start_to_file_name) {
		assert(content_start_to_file_name.find(source.data));
	};
	#endif

	Parser parser = {};
	parser.init(source);
	defer { 
		parser.reporter.print_all(); 
		parser.free();
	};

	Node *node = 0;
	while (node = parser.parse_next_node()) {
		on_parse_global_node(node);
	}

	return parser.last_yield_result == Parser::YieldResult::success;
}

bool read_file_and_parse_into_global_block(String import_location, String path);