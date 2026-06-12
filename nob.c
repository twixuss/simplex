#define NOB_IMPLEMENTATION
#define NOB_STRIP_PREFIX
#define NOB_REBUILD_URSELF(binary_path, source_path) "gcc", "-Wall", "-Wextra", "-Werror", "-x", "c", "-o", binary_path, source_path, "-ggdb"
#define NOBDC_IMPLEMENTATION
#define NOBDC_VERBOSE
#include "dep/nob.h"
#include "dep/nobdc.h"
#include <locale.h>

Cmd cmd = {0};
bool debug = true;
#define temp_dir temp_sprintf("temp/%s", debug ? "debug" : "release")
    
void append_default_cflags(bool disable_sanitizer) {
    cmd_append(&cmd, "-fPIC");
    cmd_append(&cmd, "-municode");
    cmd_append(&cmd, "-m64");
    cmd_append(&cmd, "--std=c++23");
    

    cmd_append(&cmd, "-msse4.2");
    cmd_append(&cmd, "-mlzcnt");
    cmd_append(&cmd, "-maes");
    cmd_append(&cmd, "-march=native");
    cmd_append(&cmd, "-include", "src/mingw-avx-alignment-workaround.h");
    //cmd_append(&cmd, "-mpreferred-stack-boundary=5");
    //cmd_append(&cmd, "-DIMGUI_DISABLE_SSE"); // Problems with simd on mingw
    
    cmd_append(&cmd, "-ggdb");

    // cmd_append(&cmd, "-H"); // print include tree
    // cmd_append(&cmd, "-ftime-report");

    if (!disable_sanitizer) {
        cmd_append(&cmd, "-fsanitize=undefined");
        cmd_append(&cmd, "-fsanitize-undefined-trap-on-error");
        // Annoying because traps on integer overflow in noise generators.
        //cmd_append(&cmd, "-fno-sanitize-recover=signed-integer-overflow");
    }

    if (debug) {
        cmd_append(&cmd, "-DBUILD_DEBUG=1");
        cmd_append(&cmd, "-Og");
    } else {
        cmd_append(&cmd, "-DBUILD_DEBUG=0");
        cmd_append(&cmd, "-O3");
    }
    cmd_append(&cmd, "-ffast-math");
    cmd_append(&cmd, "-fno-omit-frame-pointer");

    cmd_append(&cmd, "-Wall");
    cmd_append(&cmd, "-Wno-init-list-lifetime"); // Don't want warnings when converting std::initializer_list to Span
    cmd_append(&cmd, "-Wno-unused-local-typedefs");

    //cmd_append(&cmd, "-Wextra"); // -Wextra enables one annoyng warning that can't be disabled, a bunch of irrelevant warnings and implies all warning below:
    cmd_append(&cmd, "-Walloc-size");
    cmd_append(&cmd, "-Wcalloc-transposed-args");
    cmd_append(&cmd, "-Wcast-function-type");
    cmd_append(&cmd, "-Wclobbered");
    cmd_append(&cmd, "-Wdangling-reference");
    cmd_append(&cmd, "-Wdeprecated-copy");
    cmd_append(&cmd, "-Wempty-body");
    //cmd_append(&cmd, "-Wenum-conversion"); // annoying
    cmd_append(&cmd, "-Wexpansion-to-defined");
    cmd_append(&cmd, "-Wignored-qualifiers");
    cmd_append(&cmd, "-Wimplicit-fallthrough=5");
    cmd_append(&cmd, "-Werror=implicit-fallthrough");
    cmd_append(&cmd, "-Werror=multichar");
    cmd_append(&cmd, "-Werror=strict-aliasing");
    cmd_append(&cmd, "-Wmaybe-uninitialized");
    cmd_append(&cmd, "-Wmissing-field-initializers");
    cmd_append(&cmd, "-Wredundant-move");
    cmd_append(&cmd, "-Wshift-negative-value");
    cmd_append(&cmd, "-Wsign-compare");
    cmd_append(&cmd, "-Wsized-deallocation");
    cmd_append(&cmd, "-Wstring-compare");
    cmd_append(&cmd, "-Wtype-limits");
    cmd_append(&cmd, "-Wuninitialized");
    cmd_append(&cmd, "-Wunused-parameter");
    cmd_append(&cmd, "-Wunused-but-set-parameter");
};

typedef enum {
    COMPILE_UP_TO_DATE,
    COMPILE_SUCCESS,
    COMPILE_FAILED,
} CompileResult;

CompileResult compile_cpp_file(char const *path_base, bool disable_sanitizer) {
    char const *in_path = temp_sprintf("%s.cpp", path_base);
    char const *out_path = temp_sprintf("%s/%s.obj", temp_dir, path_base);
    char const *dl_path = temp_sprintf("%s/%s.dl", temp_dir, path_base);

    char const *include_dirs[] = {
        "dep/tl/include",
    };

    if (!nobdc_needs_rebuild(in_path, dl_path, include_dirs, ARRAY_LEN(include_dirs)))
        return COMPILE_UP_TO_DATE;

    char const *cdir = nob_get_current_dir_temp();

    cmd_append(&cmd, "g++");
    cmd_append(&cmd, temp_sprintf("%s/%s", cdir, in_path)); // make path absolute for diagnostics messages.
    cmd_append(&cmd, "-c", "-o", out_path);
    append_default_cflags(disable_sanitizer);
    cmd_append(&cmd, "-fmax-errors=1");
    //cmd_append(&cmd, "-Wfatal-errors"); // This stops gcc from printing useful info after an error.
    cmd_append(&cmd, "-Wno-comment"); // Why does gcc care about my comments...
    cmd_append(&cmd, "-Wno-literal-suffix"); // I want to use them, I will use them.
    cmd_append(&cmd, "-Wno-unused-value"); // Might be useful occasionally, but very annoying. Triggers on every assert....
    cmd_append(&cmd, "-Wno-missing-field-initializers"); // Dude what do you mean.. default initialization is a warning now???
    cmd_append(&cmd, "-Wno-sign-compare"); // I mean... I know what happens, so...
    cmd_append(&cmd, "-Werror=return-type");
    cmd_append(&cmd, "-Werror=multichar");
    cmd_append(&cmd, "-Wno-parentheses");
    cmd_append(&cmd, temp_sprintf("-I%s/dep/tl/include", cdir));
    cmd_append(&cmd, temp_sprintf("-I%s/dep/imgui", cdir));
    cmd_append(&cmd, temp_sprintf("-I%s/dep/stb", cdir));

    if (!cmd_run_sync_and_reset(&cmd)) {
        delete_file(dl_path);
        return COMPILE_FAILED;
    }

    return COMPILE_SUCCESS;
}

bool dl_deleter(Walk_Entry entry) {
    if (entry.type == FILE_REGULAR) {
        if (sv_end_with(sv_from_cstr(entry.path), ".dl")) {
            delete_file(entry.path);
        }
    }
    return true;
}

typedef enum {
    PROJECT_TYPE_EXE,
    PROJECT_TYPE_DLL,
} ProjectType;

typedef struct {
    ProjectType type;
    Nob_File_Paths translation_units;
    bool needs_linking;
} Project;

bool compile_translation_unit_and_add_to_projects(char const *base, bool disable_sanitizer, Project **projects, size_t projects_count) {
    CompileResult r = compile_cpp_file(base, disable_sanitizer);
    if (r == COMPILE_FAILED)
        return false;
        
    for (Project **_p = projects; _p != projects + projects_count; ++_p) {
        Project *p = *_p;
        p->needs_linking |= r == COMPILE_SUCCESS;
        da_append(&p->translation_units, base);
    }
    return true;
}

void append_translation_units_to_link(Cmd *cmd, Project *p) {
    da_foreach(char const *, translation_unit, &p->translation_units) {
        cmd_append(cmd, temp_sprintf("%s/%s.obj", temp_dir, *translation_unit));
    }
}

bool link(Project *p, char const *name) {
    char const *output_path = temp_sprintf("bin-gcc-%s/%s.%s", debug ? "debug" : "release", name, p->type == PROJECT_TYPE_EXE ? "exe" : "dll");
    
    p->needs_linking |= !file_exists(output_path);

    if (!p->needs_linking)
        return true;

    cmd_append(&cmd, "g++");
    append_default_cflags(false);
    append_translation_units_to_link(&cmd, p);
    if (p->type == PROJECT_TYPE_DLL)
        cmd_append(&cmd, "-shared");
    cmd_append(&cmd, "-o", output_path);
    cmd_append(&cmd, "-Wl,--stack,16777216");
    cmd_append(&cmd
        , "-ldbghelp"
        , "-lwinmm"
        , "-lcomctl32"
        , "-luuid"
        , "-lshlwapi"
        , "-lole32"
        , "-lstdc++"
        , "-lgcc_eh"
        , "-lopengl32"
        , "-lgdi32"
    );
    if (!cmd_run_sync_and_reset(&cmd)) return false;
    return true;
}

int main(int argc, char **argv) {
    system("cls");

    setlocale(LC_ALL, "utf8");

    NOB_GO_REBUILD_URSELF_PLUS(argc, argv, "dep/nob.h", "dep/nobdc.h");

    bool force_rebuild = false;
    bool rebuild_deps = false;

    char *program_path = shift_args(&argc, &argv);
    (void)program_path;
    while (argc) {
        char *arg = shift_args(&argc, &argv);
        if (strcmp(arg, "debug") == 0) {
            debug = true;
        } else if (strcmp(arg, "release") == 0) {
            debug = false;
        } else if (strcmp(arg, "force") == 0) {
            force_rebuild = true;
        } else if (strcmp(arg, "dep") == 0) {
            rebuild_deps = true;
        } else {
            nob_log(WARNING, "Unknown argument %s", arg);
        }
    }

    char const *configurations[] = {
        "debug", "release"
    };

    for (size_t i = 0; i < ARRAY_LEN(configurations); ++i) {
        char const *configuration = configurations[i];
        mkdir_if_not_exists(temp_sprintf("bin-gcc-%s", configuration));
        mkdir_if_not_exists(temp_sprintf("bin-gcc-%s/targets", configuration));
        mkdir_if_not_exists(temp_sprintf("temp"));
        mkdir_if_not_exists(temp_sprintf("temp/%s", configuration));
        mkdir_if_not_exists(temp_sprintf("temp/%s/src", configuration));
        mkdir_if_not_exists(temp_sprintf("temp/%s/src/bytecode", configuration));
        mkdir_if_not_exists(temp_sprintf("temp/%s/src/targets", configuration));
        mkdir_if_not_exists(temp_sprintf("temp/%s/src/targets/x64", configuration));
        mkdir_if_not_exists(temp_sprintf("temp/%s/backends", configuration));
        mkdir_if_not_exists(temp_sprintf("temp/%s/backends/c", configuration));
        mkdir_if_not_exists(temp_sprintf("temp/%s/dep", configuration));
        mkdir_if_not_exists(temp_sprintf("temp/%s/dep/imgui", configuration));
        mkdir_if_not_exists(temp_sprintf("temp/%s/dep/imgui/backends", configuration));
        mkdir_if_not_exists(temp_sprintf("temp/%s/test_runner", configuration));
    }

    if (force_rebuild) {
        walk_dir(temp_sprintf("%s/src", temp_dir), dl_deleter);
    }
    if (rebuild_deps) {
        walk_dir(temp_sprintf("%s/dep", temp_dir), dl_deleter);
    }

    Project simplex = {.type = PROJECT_TYPE_EXE};
    Project c_backend = {.type = PROJECT_TYPE_DLL};
    Project test_runner = {.type = PROJECT_TYPE_EXE};

    #define C(base, disable_sanitizer, ...) \
        if (!compile_translation_unit_and_add_to_projects(base, disable_sanitizer, ((Project *[]){__VA_ARGS__}), ARRAY_LEN(((Project *[]){__VA_ARGS__})))) \
            return 1;

    C("src/bytecode/builder",     false, &simplex);
    C("src/bytecode/interpreter", true,  &simplex);
    C("src/bytecode/optimizer",   false, &simplex);
    C("src/copier",               false, &simplex);
    C("src/debug",                false, &simplex);
    C("src/do_all_paths_return",  false, &simplex);
    C("src/escape",               false, &simplex);
    C("src/fiber",                false, &simplex);
    C("src/get_constant_value",   false, &simplex);
    C("src/is_constant",          false, &simplex);
    C("src/is_mutable",           false, &simplex);
    C("src/lexer",                false, &simplex);
    C("src/main",                 false, &simplex);
    C("src/make_node",            false, &simplex);
    C("src/node_interpreter",     false, &simplex);
    C("src/parser",               false, &simplex);
    C("src/print_ast",            false, &simplex);
    C("src/token",                false, &simplex);
    C("src/typechecker",          false, &simplex);
    C("src/value",                false, &simplex);
    C("src/vectorize",            false, &simplex);
    
    C("src/binary_operation",     false, &simplex, &c_backend);
    C("src/mutability",           false, &simplex, &c_backend);
    C("src/nameable",             false, &simplex, &c_backend);
    C("src/nodes",                false, &simplex, &c_backend);
    C("src/reporter",             false, &simplex, &c_backend);
    C("src/type",                 false, &simplex, &c_backend);
    C("src/unary_operation",      false, &simplex, &c_backend);

    C("backends/c/c",             false, &c_backend);

    C("test_runner/tests_main",   false, &test_runner);

    if (!link(&simplex,     "simplex"  )) return 1;
    if (!link(&c_backend,   "targets/c")) return 1;
    if (!link(&test_runner, "tests"    )) return 1;

    return 0;
}