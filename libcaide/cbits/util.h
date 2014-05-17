#pragma once

#include "clang/Basic/SourceLocation.h"
#include <string>

namespace clang {
    class ASTContext;
    class Decl;
}

clang::SourceLocation findSemiAfterLocation(clang::SourceLocation loc, clang::ASTContext& Ctx);
clang::SourceLocation findLocationAfterSemi(clang::SourceLocation loc, clang::ASTContext& Ctx);
std::string declToString(const clang::Decl* decl);
