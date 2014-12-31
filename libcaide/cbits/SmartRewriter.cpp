#include "SmartRewriter.h"

#include "clang/Basic/SourceManager.h"

using namespace clang;

bool SmartRewriter::removeRange(const SourceRange& range, Rewriter::RewriteOptions opts) {
    if (!canRemoveRange(range))
        return false;
    rewriter.RemoveText(range, opts);
    removed.push_back(range);
    return true;
}

bool SmartRewriter::canRemoveRange(const SourceRange& range) const {
    const SourceManager& srcManager = rewriter.getSourceMgr();
    const SourceLocation& b1 = range.getBegin(), e1 = range.getEnd();
    // FIXME do better than linear search
    for (size_t i = 0; i < removed.size(); ++i) {
        const SourceRange& r = removed[i];
        const SourceLocation& b2 = r.getBegin(), e2 = r.getEnd();
        if (!srcManager.isBeforeInTranslationUnit(e2, b1)
            && !srcManager.isBeforeInTranslationUnit(e1, b2))
        {
            return false;
        }
    }
    return true;
}

