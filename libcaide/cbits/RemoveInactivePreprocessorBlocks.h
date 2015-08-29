#pragma once

#include <set>
#include <string>

#include "clang/Basic/SourceLocation.h"
#include "clang/Lex/PPCallbacks.h"

namespace clang {
    class SourceManager;
    class MacroDirective;
}

class SmartRewriter;

class RemoveInactivePreprocessorBlocks: public clang::PPCallbacks {
private:
    struct RemoveInactivePreprocessorBlocksImpl;
    RemoveInactivePreprocessorBlocksImpl* impl;

public:
    RemoveInactivePreprocessorBlocks(clang::SourceManager& sourceManager_,
           SmartRewriter& rewriter_, const std::set<std::string>& macrosToKeep_);
    ~RemoveInactivePreprocessorBlocks();

    void MacroDefined(const clang::Token& MacroNameTok, const clang::MacroDirective* MD) override;
    void MacroUndefined(const clang::Token& MacroNameTok, const clang::MacroDefinition& MD) override;
    void MacroExpands(const clang::Token& /*MacroNameTok*/, const clang::MacroDefinition& MD,
                      clang::SourceRange Range, const clang::MacroArgs* /*Args*/) override;

    void If(clang::SourceLocation Loc, clang::SourceRange ConditionRange, ConditionValueKind ConditionValue) override;
    void Ifdef(clang::SourceLocation Loc, const clang::Token& MacroNameTok, const clang::MacroDefinition& /*MD*/) override;
    void Ifndef(clang::SourceLocation Loc, const clang::Token& MacroNameTok, const clang::MacroDefinition& /*MD*/) override;
    void Elif(clang::SourceLocation Loc, clang::SourceRange ConditionRange, ConditionValueKind ConditionValue, clang::SourceLocation /*IfLoc*/ ) override;
    void Else(clang::SourceLocation Loc, clang::SourceLocation /*IfLoc*/) override;
    void Endif(clang::SourceLocation Loc, clang::SourceLocation /*IfLoc*/) override;

    // EndOfMainFile() is called too late; instead, we call this one manually in the consumer.
    void Finalize();
};

