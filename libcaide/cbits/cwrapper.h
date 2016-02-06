#pragma once

void inlineCppCode(
    const char* temporaryDirectory,

    const char** clangCompilationOptions,
    int numClangOptions,

    const char** macrosToKeep,
    int numMacrosToKeep,

    int maxConsequentEmptyLines,

    const char** cppFilePaths,
    int numCppFiles,

    const char* outputFilePath);

