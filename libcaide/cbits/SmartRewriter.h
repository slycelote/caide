#pragma once

#include <set>
#include <vector>

#include "clang/Rewrite/Core/Rewriter.h"


struct RewriteItem {
    clang::SourceRange range;
    clang::Rewriter::RewriteOptions opts;
};

struct SourceLocationComparer {
    bool operator() (const clang::SourceLocation& lhs, const clang::SourceLocation& rhs) const;
    clang::Rewriter* rewriter;
};

struct RewriteItemComparer {
    bool operator() (const RewriteItem& lhs, const RewriteItem& rhs) const;
    SourceLocationComparer cmp;
};


class SmartRewriter {
public:
    explicit SmartRewriter(clang::Rewriter& _rewriter);

    bool canRemoveRange(const clang::SourceRange& range) const;
    bool removeRange(const clang::SourceRange& range, clang::Rewriter::RewriteOptions opts);
    const clang::RewriteBuffer* getRewriteBufferFor(clang::FileID fileID) const;
    void applyChanges();

private:
    clang::Rewriter& rewriter;
    std::set<RewriteItem, RewriteItemComparer> removed;
    RewriteItemComparer comparer;
    bool changesApplied;
};

