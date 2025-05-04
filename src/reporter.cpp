#pragma once
#include "reporter.h"

SourceLocation get_source_location(String location, GetSourceLocationOptions options) {
	SourceLocation result;
	result.location = location;
	result.location_column_number = 0;

	utf8 *chunk_start = location.begin();
	utf8 *chunk_end = location.end();

	if (location == u8"\n"s) {
		// Show preceding line in this case
		chunk_start--;
		chunk_end--;
	}

	while (*chunk_start && *chunk_start != '\n') {
		--chunk_start;
		++result.location_column_number;
	}
	++chunk_start;
	// Now chunk_start it is at the beggining of first line of `location`

	while (*chunk_end && *chunk_end != '\n') {
		++chunk_end;
	}
	// Now chunk_end it is at the end of last line of `location`

	// chunk_* now points at line(s) with `location`. Extend to desired line count.
	
	int missing_lines_before = 0;
	for (int i = 0; i < options.lines_before; ++i) {
		--chunk_start;
		if (*chunk_start == 0) {
			missing_lines_before = options.lines_before - i;
			break;
		}
		assert(*chunk_start == '\n');
		--chunk_start;
		
		while (*chunk_start && *chunk_start != '\n') {
			--chunk_start;
		}
		++chunk_start;
	}
	
	int missing_lines_after = 0;
	for (int i = 0; i < options.lines_after; ++i) {
		if (*chunk_end == 0) {
			missing_lines_after = options.lines_after - i;
			break;
		}

		++chunk_end;
		while (*chunk_end && *chunk_end != '\n') {
			++chunk_end;
		}
	}

	for (int i = 0; i < missing_lines_before; ++i) {
		result.lines.add(Span(chunk_start, (umm)0));
	}
	split_by_one(Span(chunk_start, chunk_end), u8'\n', [&](String line) {
		result.lines.add(line);
	});
	for (int i = 0; i < missing_lines_after; ++i) {
		result.lines.add(Span(chunk_end, (umm)0));
	}

	assert(result.lines.count);
	assert(result.lines.front().begin() <= location.begin());
	if (location != u8"\n"s) {
		assert(result.lines.back().end() >= location.end());
	}

	result.lines_start_number = 1;
	utf8 *cursor = chunk_start;
	while (*cursor != '\0') {
		if (*cursor == '\n') 
			++result.lines_start_number;
		--cursor;
	}

	result.location_line_number = result.lines_start_number + options.lines_before;

	auto &content_start_to_file_name = context_base->content_start_to_file_name;
	auto found_file_name = locked_use(content_start_to_file_name) { return content_start_to_file_name.find(cursor + 1); };
	assert(found_file_name);
	result.file = *found_file_name.value;

	return result;
}

void append(StringBuilder &builder, SourceLocation location) {
	append_format(builder, "{}:{}:{}", location.file, location.location_line_number, location.location_column_number);
}

void print_replacing_tabs_with_spaces(String string) {
	const umm n_spaces = 4;
	for (auto c : string) {
		if (c == '\t')
			for (umm i = 0; i < n_spaces; ++i)
				print(' ');
		else 
			print(c);
	}
}

void print_with_length_of(char r, umm count) {
	for (umm i = 0; i < count; ++i) {
		print(r);
	}
}

void print_with_length_of(char r, String string) {
	for (auto c : string) {
		if (c == '\t')
			for (umm i = 0; i < 4;++i) 
				tl::print(r);
		else 
			print(r);
	}
}

void print_report_indentation(int indentation) {
	for (u32 i = indentation; i--;) {
		tl::print(u8"|   "s);
	}
}

void print_source_chunk(SourceLocation source_location, int indentation, ConsoleColor highlight_color) {
	auto number_width = [](umm x) {
		if (x < 10) return 1;
		if (x < 100) return 2;
		if (x < 1000) return 3;
		if (x < 10000) return 4;
		if (x < 100000) return 5;
		if (x < 1000000) return 6;
		return 7;
	};

	auto max_line_number = source_location.lines_start_number + source_location.lines.count - 1;
	auto line_number_width = number_width(max_line_number);
	auto line_number_alignment = align_right(line_number_width, ' ');

	auto output_line = [&](u32 line_number, String line, String highlight) {
		print_report_indentation(indentation);
		umm chars_pre_source = print(u8" {} | ", Format(line_number, line_number_alignment));

		highlight.set_begin(clamp(highlight.begin(), line.begin(), line.end()));
		highlight.set_end(clamp(highlight.end(), line.begin(), line.end()));
		if (highlight.count) {
			auto prefix = String(line.begin(), highlight.begin());
			auto postfix = String(highlight.end(), line.end());

			print_replacing_tabs_with_spaces(prefix);
			with(highlight_color, print_replacing_tabs_with_spaces(highlight));
			print_replacing_tabs_with_spaces(postfix);
			println();
			if (!is_stdout_console()) {
				withs(highlight_color) {
					print_with_length_of(' ', chars_pre_source);
					print_with_length_of(' ', prefix);
					print_with_length_of('~', highlight);
					print_with_length_of(' ', postfix);
					println();
				};
			}
		} else {
			print_replacing_tabs_with_spaces(line);
			println();
			if (source_location.location == u8"\n"s) {
				withs(highlight_color) {
					print_with_length_of(' ', chars_pre_source);
					print_with_length_of(' ', line);
					println('~');
				};
			}
		}

	};

	for (int i = 0; i < source_location.lines.count; ++i) {
		output_line(source_location.lines_start_number + i, source_location.lines[i], source_location.location);
	}
}
ConsoleColor get_color(ReportKind kind) {
	switch (kind) {
		case ReportKind::info:    return ConsoleColor::cyan;
		case ReportKind::warning: return ConsoleColor::yellow;
		case ReportKind::error:   return ConsoleColor::red;
		case ReportKind::help:    return ConsoleColor::green;
	}
	return {};
}

umm print_report_kind(ReportKind kind) {
	switch (kind) {
		case ReportKind::info:    return with(get_color(kind), ::print("Info"));
		case ReportKind::warning: return with(get_color(kind), ::print("Warning"));
		case ReportKind::error:   return with(get_color(kind), ::print("Error"));
		case ReportKind::help:    return with(get_color(kind), ::print("Help"));
	}
	return 0;
}

void Report::print() {
	scoped(temporary_allocator_and_checkpoint);

	bool verbose = indentation < context_base->nested_reports_verbosity;
		
	if (verbose) {
		print_report_indentation(indentation);
		println();
	}

	if (location.data) {
		auto source_location = get_source_location(location);

		print_report_indentation(indentation);
		tl::print("{}: ", source_location);

		if (verbose) {
			println();
			print_report_indentation(indentation);
			print_report_kind(kind);
			println(": {}",  message);

			print_source_chunk(source_location, indentation, get_color(kind));
		} else {
			print_report_kind(kind);
			println(": {}",  message);
		}
	} else {
		print_report_indentation(indentation);
		print_report_kind(kind);
		println(": {}", message);
	}
}

void Reporter::add(Report report) {
	report.indentation = indentation;
	reports.add(report);
}
void Reporter::on_report(Report report) {
	reports.add(report);
}
void Reporter::print_all() {
	scoped(context_base->stdout_mutex);
	for (auto &report : reports) {
		report.print();
	}
}

void ImmediateReporter::on_report(Report report) {
	scoped(context_base->stdout_mutex);
	report.print();
}
