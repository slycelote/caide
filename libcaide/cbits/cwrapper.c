#include "cwrapper.h"
#include "cpp-inliner/src/caideInliner.h"

void inlineCppCode(
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

    const char* outputFilePath)
{
    struct CaideCppInlinerOptions opts;
    opts.temporaryDirectory = temporaryDirectory;
    opts.clangCompilationOptions = clangCompilationOptions;
    opts.numClangOptions = numClangOptions;
    opts.macrosToKeep = macrosToKeep;
    opts.numMacrosToKeep = numMacrosToKeep;
    opts.identifiersToKeep = identifiersToKeep;
    opts.numIdentifiersToKeep = numIdentifiersToKeep;
    opts.maxConsequentEmptyLines = maxConsequentEmptyLines;

    caideInlineCppCode(&opts, cppFilePaths, numCppFiles, outputFilePath);
}

