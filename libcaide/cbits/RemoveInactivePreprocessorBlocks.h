#pragma once

#include <map>
#include <set>
#include <string>
#include <vector>

#include "clang/Basic/SourceLocation.h"
#include "clang/Lex/PPCallbacks.h"

namespace clang {
    class SourceManager;
    class MacroDirective;
}

class SmartRewriter;

struct IfDefClause {
    // Locations of #if, #ifdef, #ifndef, #else, #elif tokens of this clause
    std::vector<clang::SourceLocation> locations;

    // Index of selected branch in locations list; -1 if no branch was selected
    int selectedBranch;

    bool keepAllBranches;

    explicit IfDefClause(const clang::SourceLocation& ifLocation);
};

struct Macro {
    std::string name;
    std::vector<clang::SourceRange> usages;
};

class RemoveInactivePreprocessorBlocks: public clang::PPCallbacks {
private:
    struct RemoveInactivePreprocessorBlocksImpl;
    RemoveInactivePreprocessorBlocksImpl* impl;

public:
    RemoveInactivePreprocessorBlocks(clang::SourceManager& sourceManager_,
           SmartRewriter& rewriter_, const std::set<std::string>& macrosToKeep_);
    ~RemoveInactivePreprocessorBlocks();

    void MacroDefined(const clang::Token& MacroNameTok, const clang::MacroDirective* MD) override;
    void MacroUndefined(const clang::Token& MacroNameTok, const clang::MacroDirective* MD) override;
    void MacroExpands(const clang::Token& /*MacroNameTok*/, const clang::MacroDirective* MD,
                      clang::SourceRange Range, const clang::MacroArgs* /*Args*/) override;

    void If(clang::SourceLocation Loc, clang::SourceRange ConditionRange, ConditionValueKind ConditionValue) override;
    void Ifdef(clang::SourceLocation Loc, const clang::Token& MacroNameTok, const clang::MacroDirective* /*MD*/) override;
    void Ifndef(clang::SourceLocation Loc, const clang::Token& MacroNameTok, const clang::MacroDirective* /*MD*/) override;
    void Elif(clang::SourceLocation Loc, clang::SourceRange ConditionRange, ConditionValueKind ConditionValue, clang::SourceLocation /*IfLoc*/ ) override;
    void Else(clang::SourceLocation Loc, clang::SourceLocation /*IfLoc*/) override;
    void Endif(clang::SourceLocation Loc, clang::SourceLocation /*IfLoc*/) override;

    // EndOfMainFile() is called too late; instead, we call this one manually in the consumer.
    void Finalize();
};

