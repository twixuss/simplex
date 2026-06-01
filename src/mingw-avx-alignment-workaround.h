// Replace every aligned mov instruction because mingw is incapable of producing working code.
asm(
    ".macro vmovapd args:vararg\n"
    "    vmovupd \\args\n"
    ".endm\n"
    ".macro vmovaps args:vararg\n"
    "    vmovups \\args\n"
    ".endm\n"
    ".macro vmovdqa args:vararg\n"
    "    vmovdqu \\args\n"
    ".endm\n"
    ".macro vmovdqa32 args:vararg\n"
    "    vmovdqu32 \\args\n"
    ".endm\n"
    ".macro vmovdqa64 args:vararg\n"
    "    vmovdqu64 \\args\n"
    ".endm\n"
);