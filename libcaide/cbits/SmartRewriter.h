#pragma once

#include <vector>

#include "clang/Rewrite/Core/Rewriter.h"



class SmartRewriter {
public:
    explicit SmartRewriter(clang::Rewriter& _rewriter)
        : rewriter(_rewriter)
    {}

    bool canRemoveRange(const clang::SourceRange& range) const;
    bool removeRange(const clang::SourceRange& range, clang::Rewriter::RewriteOptions opts);

private:
    clang::Rewriter& rewriter;
    std::vector<clang::SourceRange> removed;
};

