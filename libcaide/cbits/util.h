#pragma once

#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/TokenKinds.h"
#include "clang/Tooling/CompilationDatabase.h"

namespace clang {
    class ASTContext;
    class Decl;
}

#include <vector>
#include <string>

clang::SourceLocation findTokenAfterLocation(clang::SourceLocation loc, clang::ASTContext& Ctx, clang::tok::TokenKind tokenType);
clang::SourceLocation findSemiAfterLocation(clang::SourceLocation loc, clang::ASTContext& Ctx);
clang::SourceLocation findLocationAfterSemi(clang::SourceLocation loc, clang::ASTContext& Ctx);

clang::tooling::FixedCompilationDatabase* createCompilationDatabaseFromCommandLine(const std::vector<std::string> cmdLine);

std::string rangeToString(clang::SourceManager& sourceManager,
        const clang::SourceLocation& start, const clang::SourceLocation& end);

std::string toString(clang::SourceManager& sourceManager, clang::SourceLocation loc);
std::string toString(clang::SourceManager& sourceManager, clang::SourceRange range);
std::string toString(clang::SourceManager& sourceManager, const clang::Decl* decl);

clang::SourceLocation getExpansionStart(clang::SourceManager& sourceManager,
        const clang::Decl* decl);
clang::SourceLocation getExpansionEnd(clang::SourceManager& sourceManager,
        const clang::Decl* decl);

clang::SourceRange getExpansionRange(clang::SourceManager& sourceManager,
        const clang::Decl* decl);

