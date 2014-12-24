#pragma once

#include "clang/Basic/SourceLocation.h"
#include <stdexcept>
#include <string>

namespace clang {
    class ASTContext;
    class Decl;
}

clang::SourceLocation findSemiAfterLocation(clang::SourceLocation loc, clang::ASTContext& Ctx);
clang::SourceLocation findLocationAfterSemi(clang::SourceLocation loc, clang::ASTContext& Ctx);
std::string declToString(const clang::Decl* decl);

template<typename T>
T* Z(T* p) {
    if (!p)
        throw std::runtime_error("Null pointer");
    return p;
}

template<typename T>
const T* Z(const T* p) {
    if (!p)
        throw std::runtime_error("Null pointer");
    return p;
}

