// Dependency checker based on https://github.com/tsoding/nob
// Find include dependencies for c files.
// Use to automatically rebuild when included file is modified.
//
// To include as a header file:
/*
#include <nob.h>
#include <nobdc.h>
*/
// To include as a c file:
/*
#define NOB_STRIP_PREFIX
#include <nob.h>
#define NOBDC_IMPLEMENTATION
#include <nobdc.h>
*/
// To enable verbose logging:
/*
#define NOBDC_VERBOSE
*/
// Usage: see comments above `nobdc_needs_rebuild`

// NOTE that the implementation is very simple/dumb.
// #includes in comments or disabled #if blocks are counted, which means
// there might be false positives (unnecessary rebuilds).

#ifndef NOBDC_DEF
#define NOBDC_DEF
#endif

//
// Determines if the content of the c file or any of its dependencies was changed.
//
// Parameters
//             c_file_path - Path to the c file you want to build
//            dl_file_path - Path to the dependency list file. It should be unique per c file.
//                           It will store the header paths and their modification timestamps.
//     include_directories - Where to search for files included with <> angle brackets.
//                           Only pass dirs with headers you might modify.
//
// Returns true if there were modifications to the c file or its dependencies.
//
NOBDC_DEF bool nobdc_needs_rebuild(char const *c_file_path, char const *dl_file_path, char const **include_directories, size_t include_directories_count);

#ifdef NOBDC_IMPLEMENTATION

// In case of failure when opening a file nobdc tries again.
// In my case failures happen pretty often, and I guess it's because
// the editor saves files after hitting build, but handles are not closed
// in time.
	// How many times to attempt opening a file in total in case of failure.
	#ifndef NOBDC_OPEN_FILE_ATTEMPTS
	#define NOBDC_OPEN_FILE_ATTEMPTS 6
	#endif
	// How many milliseconds to sleep between retries.
	#ifndef NOBDC_OPEN_FILE_DELAY_MS
	#define NOBDC_OPEN_FILE_DELAY_MS 200
	#endif

#ifdef NOBDC_VERBOSE
#define nobdc_verbose nob_log
#else
#define nobdc_verbose(...)
#endif


static char *nobdc_sv_find(String_View haystack, String_View needle) {
	for (intptr_t i = 0; i <= (intptr_t)(haystack.count - needle.count); ++i)
		if (memcmp(haystack.data + i, needle.data, needle.count) == 0)
			return (char *)haystack.data + i;
	return 0;
}

// Copied from nob_needs_rebuild
static uint64_t nobdc_get_file_write_time(const char *path) {
#ifdef _WIN32
    HANDLE output_path_fd = CreateFileA(path, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_READONLY, NULL);
    if (output_path_fd == INVALID_HANDLE_VALUE) {
        nob_log(NOB_ERROR, "Could not open file %s: %s", path, nob_win32_error_message(GetLastError()));
        return 0;
    }
    FILETIME output_path_time;
    BOOL bSuccess = GetFileTime(output_path_fd, NULL, NULL, &output_path_time);
    CloseHandle(output_path_fd);
    if (!bSuccess) {
        nob_log(NOB_ERROR, "Could not get time of %s: %s", path, nob_win32_error_message(GetLastError()));
        return 0;
    }
	return *(uint64_t *)&output_path_time;
#else
    struct stat statbuf = {0};
    if (stat(path, &statbuf) < 0) {
        nob_log(NOB_ERROR, "could not stat %s: %s", path, strerror(errno));
        return 0;
    }
    return statbuf.st_mtime;
#endif
}

static void nobdc_sleep_ms(size_t ms) {
	#ifdef _WIN32
	Sleep(ms);
	#else
	struct timespec ts;
	ts.tv_sec = (ms) / 1000;
	ts.tv_nsec = ((ms) % 1000) * 1000000;
	nanosleep(&ts, NULL);
	#endif
}

static uint64_t nobdc_get_file_write_time_retry(const char *path) {
	for (int i = 0;;) {
		uint64_t result = nobdc_get_file_write_time(path);
		if (result || ++i == NOBDC_OPEN_FILE_ATTEMPTS) return result;
		nobdc_sleep_ms(NOBDC_OPEN_FILE_DELAY_MS);
	}
	return 0;
}

static bool nobdc_read_entire_file_retry(char const *path, String_Builder *sb) {
	for (int i = 0;;) {
		bool result = read_entire_file(path, sb);
		if (result || ++i == NOBDC_OPEN_FILE_ATTEMPTS) return result;
		nobdc_sleep_ms(NOBDC_OPEN_FILE_DELAY_MS);
	}
	return false;
}

static char *nobdc_canonicalize(char const *path) {
#ifdef _WIN32
    char *absolute_path = temp_alloc(MAX_PATH);
    if (GetFullPathNameA(path, MAX_PATH, absolute_path, NULL) == 0) strcpy(absolute_path, path);
	
	for (char *c = absolute_path; *c; ++c) {
		// Lowercase and uppercase are not distinguished;
		*c = tolower(*c);

		// As well as \ and /, but GetFullPathNameA already does that.
		// if (*c == '/')
		// 	*c = '\\';
	}

	return absolute_path;
#else
    char *absolute_path = temp_alloc(PATH_MAX);
    if (realpath(path, absolute_path) == NULL) strcpy(absolute_path, path);
	return absolute_path;
#endif
}

typedef struct {
	String_View path;
	uint64_t write_time;
} Nobdc_Dependency;

typedef struct {
	Nobdc_Dependency *items;
	size_t count;
	size_t capacity;
} Nobdc_Dependencies;

static int nobdc_compare_dependencies_lexically(void const *a_, void const *b_) {
	Nobdc_Dependency const *a = a_;
	Nobdc_Dependency const *b = b_;
	int diff = a->path.count - b->path.count;
	if (diff) return diff;
	return memcmp(a->path.data, b->path.data, a->path.count);
}

static char *nobdc_temp_concat_path(char const *dir, String_View include_without_quotes) {
	char *include_path = temp_sprintf("%s/"SV_Fmt, dir, SV_Arg(include_without_quotes));
	include_path = nobdc_canonicalize(include_path); // Must canonicalize to not have different paths pointing to same file
	if (!file_exists(include_path) || get_file_type(include_path) != FILE_REGULAR) return 0;
	return include_path;
}

bool nobdc_needs_rebuild(char const *c_file_path, char const *dl_file_path, char const **include_directories, size_t include_directories_count) {
	size_t temp_mark = temp_save();

	File_Paths remaining_files = {0}; // Stack of files we need to parse. NOTE: paths are in temp memory
	File_Paths processed_files = {0}; // List of all files we parsed. Used only to skip duplicates.
	Nobdc_Dependencies current_dependencies = {0}; // Current dependencies of the c file.

	da_append(&remaining_files, c_file_path);

	String_Builder sb = {0};

	while (remaining_files.count) {
		char const *file_path = da_last(&remaining_files);
		remaining_files.count -= 1;

		da_foreach(char const *, existing, &processed_files)
			if (strcmp(file_path, *existing) == 0)
				goto next_file;
		da_append(&processed_files, file_path);

		sb.count = 0;
		if (!nobdc_read_entire_file_retry(file_path, &sb)) continue;
		String_View rest = sb_to_sv(sb);

		da_append(&current_dependencies, ((Nobdc_Dependency){
			.path = sv_from_cstr(file_path),
			.write_time = nobdc_get_file_write_time_retry(file_path)
		}));

		while (1) {
			char *include_cstr = nobdc_sv_find(rest, sv_from_cstr("#include"));
			if (!include_cstr) break;

			String_View include;
			include.data = include_cstr + 8;
			include.count = rest.data + rest.count - include.data;
			rest = include;
			include = sv_chop_by_delim(&rest, '\n');

			char *comment = nobdc_sv_find(include, sv_from_cstr("//"));
			if (comment) include.count = comment - include.data;

			comment = nobdc_sv_find(include, sv_from_cstr("/*"));
			if (comment) include.count = comment - include.data;

			include = sv_trim(include);

			if (include.count < 3) continue; // minimal include is "a" or <a>
			
			String_View include_without_quotes = include;
			include_without_quotes.data += 1;
			include_without_quotes.count -= 2;

			if (include.data[0] == '"') {
				// relative
				char *dir = nob_temp_dir_name(file_path);
				while (dir[strlen(dir) - 1] == '/') dir[strlen(dir) - 1] = '\0';

				char *include_path = nobdc_temp_concat_path(dir, include_without_quotes);
				if (!include_path) continue;

				da_append(&remaining_files, include_path);
			} else if (include.data[0] == '<') {
				// absolute
				for (size_t i = 0; i < include_directories_count; ++i) {
					char *include_path = nobdc_temp_concat_path(include_directories[i], include_without_quotes);
					if (!include_path) continue;

					da_append(&remaining_files, include_path);
					break;
				}
			}
		}
	next_file:;
	}

	// Sort new dependencies for comparing with previous dependencies. Previous dependencies are already sorted.
	qsort(
		current_dependencies.items,
		current_dependencies.count, 
		sizeof(current_dependencies.items[0]),
		nobdc_compare_dependencies_lexically
	);

	// Parse previous dependencies
	Nobdc_Dependencies prev_dependencies = {0};
	sb.count = 0;
	// No need to check error.
	read_entire_file(dl_file_path, &sb); 
	String_View rest = sb_to_sv(sb);
	while (rest.count) {
		String_View line = sv_chop_by_delim(&rest, '\n');
		uint64_t stored_write_time = (uint64_t)atoll(line.data); // atoll will end parsing when encountering a space.
		sv_chop_by_delim(&line, ' ');
		String_View include_path = line;
		da_append(&prev_dependencies, ((Nobdc_Dependency){.path = include_path, .write_time = stored_write_time}));
	}

	// Overwrite file of old dependencies with new ones.
	FILE *dl_file = fopen(dl_file_path, "wb");
	if (dl_file) {
		da_foreach (Nobdc_Dependency, current_dep, &current_dependencies) {
			fprintf(dl_file, "%llu "SV_Fmt"\n", current_dep->write_time, SV_Arg(current_dep->path));
		}
	} else {
		nob_log(ERROR, "Failed to open \"%s\" for writing: %s", dl_file_path, strerror(errno));
	}

	// Check for changes
	bool should_rebuild = false;
	if (current_dependencies.count != prev_dependencies.count) {
		should_rebuild = true;
		nobdc_verbose(INFO, "REBUILD REASON: Number of dependencies changed from %llu to %llu", prev_dependencies.count, current_dependencies.count);
	} else {
		for (size_t i = 0; i < current_dependencies.count; ++i) {
			Nobdc_Dependency curr_dep = current_dependencies.items[i];
			Nobdc_Dependency prev_dep = prev_dependencies.items[i];
			if (sv_eq(curr_dep.path, prev_dep.path)) {
				if (curr_dep.write_time > prev_dep.write_time) {
					should_rebuild = true;
					nobdc_verbose(INFO, "REBUILD REASON: Write time of "SV_Fmt" was changed", SV_Arg(curr_dep.path));
					break;
				}
			} else {
				should_rebuild = true;
				nobdc_verbose(INFO, "REBUILD REASON: Dependency path changed from "SV_Fmt" to "SV_Fmt, SV_Arg(prev_dep.path), SV_Arg(curr_dep.path));
				break;
			}
		}
	}

	if (dl_file) fclose(dl_file);
	if (sb.items) sb_free(sb);
	temp_rewind(temp_mark);

	return should_rebuild;
}

#endif
