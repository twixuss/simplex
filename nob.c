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

    // Annoying because traps on integer overflow in noise generators.
    if (!disable_sanitizer) {
        cmd_append(&cmd, "-fsanitize=undefined");
        cmd_append(&cmd, "-fsanitize-undefined-trap-on-error");
    }
    
    if (debug) {
        cmd_append(&cmd, "-DBUILD_DEBUG=1");
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
    //cmd_append(&cmd, "-Werror=multichar");
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

bool compile_cpp_file(char const *path_base, bool disable_sanitizer) {
    char const *in_path = temp_sprintf("%s.cpp", path_base);
    char const *out_path = temp_sprintf("%s/%s.obj", temp_dir, path_base);
    char const *dl_path = temp_sprintf("%s/%s.dl", temp_dir, path_base);

    char const *include_dirs[] = {
        "dep/tl/include",
    };

    if (!nobdc_needs_rebuild(in_path, dl_path, include_dirs, ARRAY_LEN(include_dirs)))
        return true;

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
        return false;
    }

    return true;
}

bool dl_deleter(Walk_Entry entry) {
    if (entry.type == FILE_REGULAR) {
        if (sv_end_with(sv_from_cstr(entry.path), ".dl")) {
            delete_file(entry.path);
        }
    }
    return true;
}

typedef struct {
    char const *base;
    bool disable_sanitizer;
} TranslationUnit;

typedef struct {
    TranslationUnit *items;
    size_t count;
    size_t capacity;
} TranslationUnits;

bool append_translation_unit(Nob_Walk_Entry entry) {
    TranslationUnits *cpp_files = entry.data;
    switch (entry.type) {
        case FILE_REGULAR: {
            if (sv_end_with(sv_from_cstr(entry.path), ".cpp")) {
                char *noext = strdup(entry.path);
                noext[strlen(noext)-4] = '\0';
                da_append(cpp_files, ((TranslationUnit){
                    .base = noext,
                }));
            }
            break;
        }
        default:
            break;
    }
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

    TranslationUnits all_translation_units = {0};
    /* Compile all translation units */ {
        walk_dir("src",         append_translation_unit, .data = &all_translation_units);
        walk_dir("backends/c",  append_translation_unit, .data = &all_translation_units);
        walk_dir("test_runner", append_translation_unit, .data = &all_translation_units);

        // Remove some
        for (size_t i = 0; i < all_translation_units.count; ++i) {
            if (strcmp(all_translation_units.items[i].base, "src\\c2simplex") == 0) {
                da_remove_unordered(&all_translation_units, i);
                --i;
                continue;
            }
        }

        // Don't sanitize some
        for (size_t i = 0; i < all_translation_units.count; ++i) {
            if (strcmp(all_translation_units.items[i].base, "src\\bytecode\\interpreter") == 0) {
                all_translation_units.items[i].disable_sanitizer = true;
            }
        }

        for (size_t i = 0; i < all_translation_units.count; ++i) {
            if (!compile_cpp_file(all_translation_units.items[i].base, all_translation_units.items[i].disable_sanitizer))
                return 1;
        }
    }

    /* Compile Simplex */ {
        char const *simplex_exe_path = temp_sprintf("bin-gcc-%s/simplex.exe", debug ? "debug" : "release");
        cmd_append(&cmd, "g++");
        append_default_cflags(false);
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/bytecode/builder"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/bytecode/interpreter"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/bytecode/optimizer"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/binary_operation"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/copier"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/debug"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/do_all_paths_return"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/escape"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/fiber"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/get_constant_value"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/is_constant"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/is_mutable"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/lexer"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/main"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/make_node"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/mutability"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/nameable"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/nodes"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/node_interpreter"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/parser"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/print_ast"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/reporter"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/token"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/type"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/typechecker"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/unary_operation"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/value"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/vectorize"));
        cmd_append(&cmd, "-o", simplex_exe_path);
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
        if (!cmd_run_sync_and_reset(&cmd)) return 1;
    }

    /* Compile C Backend */ {
        char const *c_backend_dll_path = temp_sprintf("bin-gcc-%s/targets/c.dll", debug ? "debug" : "release");
        cmd_append(&cmd, "g++");
        append_default_cflags(false);
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/binary_operation"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/mutability"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/nameable"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/nodes"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/reporter"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/type"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "src/unary_operation"));
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "backends/c/c"));
        cmd_append(&cmd, "-o", c_backend_dll_path);
        cmd_append(&cmd, "-shared");
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
        if (!cmd_run_sync_and_reset(&cmd)) return 1;

    }

    /* Compile Test Runner */ {
        char const *test_runner_exe_path = temp_sprintf("bin-gcc-%s/tests.exe", debug ? "debug" : "release");
        cmd_append(&cmd, "g++");
        append_default_cflags(false);
        cmd_append(&cmd, temp_sprintf("%s/%s.obj", temp_dir, "test_runner/tests_main"));
        cmd_append(&cmd, "-o", test_runner_exe_path);
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
        if (!cmd_run_sync_and_reset(&cmd)) return 1;
    }

    return 0;
}