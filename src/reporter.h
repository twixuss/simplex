#pragma once
#include "common.h"

extern bool break_on_error;
extern u32 nested_reports_verbosity;

struct SourceLine {
	String string;
	u32 number = 0;
};

struct SourceLocation {
	String location;
	String file;
	u32 location_line_number = 0;
	u32 location_column_number = 0;

	u32 lines_start_number = 0;
	List<String> lines;
};

extern LockProtected<GHashMap<utf8 *, String>, SpinLock> content_start_to_file_name;

struct GetSourceLocationOptions {
	int lines_before = 0;
	int lines_after = 0;
};

SourceLocation get_source_location(String location, GetSourceLocationOptions options = {});

void append(StringBuilder &builder, SourceLocation location);

void print_replacing_tabs_with_spaces(String string);
void print_with_length_of(char r, umm count);
void print_with_length_of(char r, String string);

enum class ReportKind : u8 {
	info,
	warning,
	error,
	help,
};

ConsoleColor get_color(ReportKind kind);
umm print_report_kind(ReportKind kind);

void print_report_indentation(int indentation);

void print_source_chunk(SourceLocation source_location, int indentation, ConsoleColor highlight_color);

struct Report {
	ReportKind kind = {};
	String location = {};
	String message = {};
	u32 indentation = 0;

	static Report create(ReportKind kind, u32 indentation, String location, auto const &message) {
		return {
			.kind = kind,
			.location = location,
			.message = (String)to_string(message),
			.indentation = indentation,
		};
	}
	static Report create(ReportKind kind, u32 indentation, String location, char const *format, auto const &arg, auto const &...args) {
		return {
			.kind = kind,
			.location = location,
			.message = (String)tl::format(format, arg, args...),
			.indentation = indentation,
		};
	}


	void print();
};

struct ReporterBase {
	u32 indentation = 0;
	void info   (this auto &&self, String location, auto const &...args) { self.on_report(Report::create(ReportKind::info,    self.indentation, location, args...)); }
	void warning(this auto &&self, String location, auto const &...args) { self.on_report(Report::create(ReportKind::warning, self.indentation, location, args...)); }
	void error  (this auto &&self, String location, auto const &...args) {
		if (break_on_error) {
			debug_break();
		}
		self.on_report(Report::create(ReportKind::error, self.indentation, location, args...));
	}
	void help   (this auto &&self, String location, auto const &...args) { self.on_report(Report::create(ReportKind::help, self.indentation, location, args...)); }
	void info   (this auto &&self, auto const &...args) { return self.info   (String{}, args...); }
	void warning(this auto &&self, auto const &...args) { return self.warning(String{}, args...); }
	void error  (this auto &&self, auto const &...args) { return self.error  (String{}, args...); }
	void help   (this auto &&self, auto const &...args) { return self.help   (String{}, args...); }
};

struct Reporter : ReporterBase {
	List<Report> reports;

	void add(Report report);
	void on_report(Report report);
	void print_all();
};

struct ImmediateReporter : ReporterBase {
	void on_report(Report report);
};

inline ImmediateReporter immediate_reporter;
