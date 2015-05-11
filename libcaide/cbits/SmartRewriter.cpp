#include "SmartRewriter.h"

#include "clang/Basic/SourceManager.h"

#include <stdexcept>

using namespace clang;

bool SourceLocationComparer::operator() (const SourceLocation& lhs, const SourceLocation& rhs) const {
    return rewriter->getSourceMgr().isBeforeInTranslationUnit(lhs, rhs);
}

bool RewriteItemComparer::operator() (const RewriteItem& lhs, const RewriteItem& rhs) const {
    return cmp(lhs.range.getBegin(), rhs.range.getBegin());
}

SmartRewriter::SmartRewriter(clang::Rewriter& rewriter_)
    : rewriter(rewriter_)
    , changesApplied(false)
{
    comparer.cmp.rewriter = &rewriter_;
    removed = std::set<RewriteItem, RewriteItemComparer>(comparer);
}

bool SmartRewriter::removeRange(const SourceRange& range, Rewriter::RewriteOptions opts) {
    if (!canRemoveRange(range))
        return false;
    removed.insert({range, opts});
    return true;
}

bool SmartRewriter::canRemoveRange(const SourceRange& range) const {
    if (removed.empty())
        return true;

    RewriteItem ri;
    ri.range = range;

    auto i = removed.lower_bound(ri);
    // i->range.getBegin() >= range.getBegin()

    const SourceManager& srcManager = rewriter.getSourceMgr();

    if (i != removed.end() && !srcManager.isBeforeInTranslationUnit(range.getEnd(), i->range.getBegin()))
        return false;

    if (i == removed.begin())
        return true;

    --i;
    // i->range.getBegin() < range.getBegin()

    return srcManager.isBeforeInTranslationUnit(i->range.getEnd(), range.getBegin());
}

const RewriteBuffer* SmartRewriter::getRewriteBufferFor(FileID fileID) const {
    return rewriter.getRewriteBufferFor(fileID);
}

void SmartRewriter::applyChanges() {
    if (changesApplied)
        throw std::logic_error("Rewriter changes have already been applied");
    changesApplied = true;
    for (const RewriteItem& ri : removed)
        rewriter.RemoveText(ri.range, ri.opts);
}

