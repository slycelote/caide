#pragma once

int inlineCppCode(
    const char* temporaryDirectory,

    const char** clangCompilationOptions,
    int numClangOptions,

    const char** macrosToKeep,
    int numMacrosToKeep,

    const char** identifiersToKeep,
    int numIdentifiersToKeep,

    int maxConsequentEmptyLines,

    const char** cppFilePaths,
    int numCppFiles,

    const char* outputFilePath);

