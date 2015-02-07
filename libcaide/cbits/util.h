#pragma once

#include "clang/Basic/SourceLocation.h"
#include "clang/Tooling/CompilationDatabase.h"

namespace clang {
    class ASTContext;
    class Decl;
}

#include <vector>
#include <string>

clang::SourceLocation findSemiAfterLocation(clang::SourceLocation loc, clang::ASTContext& Ctx);
clang::SourceLocation findLocationAfterSemi(clang::SourceLocation loc, clang::ASTContext& Ctx);

clang::tooling::FixedCompilationDatabase* createCompilationDatabaseFromCommandLine(const std::vector<std::string> cmdLine);

std::string rangeToString(clang::SourceManager& sourceManager,
        const clang::SourceLocation& start, const clang::SourceLocation& end);

