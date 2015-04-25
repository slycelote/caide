#include "RemoveInactivePreprocessorBlocks.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Preprocessor.h"

#include "SmartRewriter.h"

using namespace std;
using namespace clang;

IfDefClause::IfDefClause(const SourceLocation& ifLocation)
    : selectedBranch(-1)
    , keepAllBranches(false)
{
    locations.push_back(ifLocation);
}

struct RemoveInactivePreprocessorBlocks::RemoveInactivePreprocessorBlocksImpl {
private:
    SourceManager& sourceManager;
    SmartRewriter& rewriter;
    const set<string>& macrosToKeep;

    vector<IfDefClause> activeClauses;
    set<string> definedMacroNames;

    typedef map<const MacroDirective*, Macro> MacroUsages;
    MacroUsages definedMacros;

private:
    bool isWhitelistedMacro(const string& macroName) const {
        return macrosToKeep.find(macroName) != macrosToKeep.end();
    }

    bool tryRemoveMacroDefinition(MacroUsages::const_iterator it) {
        if (it == definedMacros.end() || isWhitelistedMacro(it->second.name))
            return false;

        bool isUsed = false;
        for (const SourceRange& usageRange : it->second.usages) {
            // Check whether the usage of the macro has been removed
            if (rewriter.canRemoveRange(usageRange)) {
                isUsed = true;
                break;
            }
        }

        if (!isUsed) {
            SourceLocation defLocation = it->first->getLocation();
            removeLine(defLocation);
            return true;
        }
        return false;
    }

    void removeLine(SourceLocation loc) {
        Rewriter::RewriteOptions opts;
        opts.RemoveLineIfEmpty = true;
        SourceLocation b = changeColumn(loc, 1);
        SourceLocation e = changeColumn(loc, 10000);
        rewriter.removeRange(SourceRange(b, e), opts);
    }

    bool isInMainFile(SourceLocation loc) const {
        // sourceManager.isInMainFile returns true for builtin defines
        return sourceManager.getFileID(loc) == sourceManager.getMainFileID();
    }

public:
    RemoveInactivePreprocessorBlocksImpl(
            SourceManager& sourceManager_, SmartRewriter& rewriter_,
            const set<string>& macrosToKeep_)
        : sourceManager(sourceManager_)
        , rewriter(rewriter_)
        , macrosToKeep(macrosToKeep_)
    {
    }

    void MacroDefined(const Token& MacroNameTok, const MacroDirective* MD) {
        definedMacroNames.insert(getTokenName(MacroNameTok));
        if (MD && isInMainFile(MD->getLocation()))
            definedMacros[MD].usages.clear();
    }

    void MacroUndefined(const Token& MacroNameTok, const MacroDirective* MD) {
        definedMacroNames.erase(getTokenName(MacroNameTok));

        if (!MD || !isInMainFile(MD->getLocation()))
            return;

        MacroUsages::const_iterator it = definedMacros.find(MD);
        if (tryRemoveMacroDefinition(it)) {
            // Removed the #define, so remove this #undef too
            SourceLocation undefLoc = MacroNameTok.getLocation();
            removeLine(undefLoc);
        }

        if (it != definedMacros.end())
            definedMacros.erase(it);
    }

    void MacroExpands(const Token& /*MacroNameTok*/, const MacroDirective* MD,
                      SourceRange Range, const MacroArgs* /*Args*/)
    {
        if (!MD || !isInMainFile(MD->getLocation()))
            return;

        definedMacros[MD].usages.push_back(Range);
    }

    void Finalize() {
        // Remove unused #defines that don't have a corresponding #undef
        for (auto it = definedMacros.begin(); it != definedMacros.end(); ++it)
            tryRemoveMacroDefinition(it);
    }

    void If(SourceLocation Loc, SourceRange ConditionRange, ConditionValueKind ConditionValue) {
        if (!isInMainFile(Loc))
            return;
        activeClauses.push_back(IfDefClause(Loc));
        if (ConditionValue == CVK_True)
            activeClauses.back().selectedBranch = 0;
        if (containsWhitelistedString(ConditionRange))
            activeClauses.back().keepAllBranches = true;
    }

    void Ifdef(SourceLocation Loc, const Token& MacroNameTok, const MacroDirective* /*MD*/) {
        if (!isInMainFile(Loc))
            return;
        activeClauses.push_back(IfDefClause(Loc));
        string macroName = getTokenName(MacroNameTok);
        if (definedMacroNames.find(macroName) != definedMacroNames.end())
            activeClauses.back().selectedBranch = 0;
        if (isWhitelistedMacro(macroName))
            activeClauses.back().keepAllBranches = true;
    }

    void Ifndef(SourceLocation Loc, const Token& MacroNameTok, const MacroDirective* /*MD*/) {
        if (!isInMainFile(Loc))
            return;
        activeClauses.push_back(IfDefClause(Loc));
        string macroName = getTokenName(MacroNameTok);
        if (definedMacroNames.find(macroName) == definedMacroNames.end())
            activeClauses.back().selectedBranch = 0;
        if (isWhitelistedMacro(macroName))
            activeClauses.back().keepAllBranches = true;
    }

    void Elif(SourceLocation Loc, SourceRange ConditionRange, ConditionValueKind ConditionValue, SourceLocation /*IfLoc*/ ) {
        if (!isInMainFile(Loc))
            return;
        if (ConditionValue == CVK_True)
            activeClauses.back().selectedBranch = activeClauses.back().locations.size();
        activeClauses.back().locations.push_back(Loc);
        if (containsWhitelistedString(ConditionRange))
            activeClauses.back().keepAllBranches = true;
    }

    void Else(SourceLocation Loc, SourceLocation /*IfLoc*/) {
        if (!isInMainFile(Loc))
            return;
        if (activeClauses.back().selectedBranch < 0)
            activeClauses.back().selectedBranch = activeClauses.back().locations.size();
        activeClauses.back().locations.push_back(Loc);
    }

    void Endif(SourceLocation Loc, SourceLocation /*IfLoc*/) {
        if (!isInMainFile(Loc))
            return;
        IfDefClause& clause = activeClauses.back();
        clause.locations.push_back(Loc);

        Rewriter::RewriteOptions opts;
        opts.RemoveLineIfEmpty = true;

        if (clause.keepAllBranches) {
            // do nothing
        } else if (clause.selectedBranch < 0) {
            // remove all branches
            SourceLocation b = changeColumn(clause.locations.front(), 1);
            SourceLocation e = changeColumn(clause.locations.back(), 10000);
            rewriter.removeRange(SourceRange(b, e), opts);
        } else {
            // remove all branches except selected
            SourceLocation b = changeColumn(clause.locations.front(), 1);
            SourceLocation e = changeColumn(clause.locations[clause.selectedBranch], 10000);
            rewriter.removeRange(SourceRange(b, e), opts);

            b = changeColumn(clause.locations[clause.selectedBranch + 1], 1);
            e = changeColumn(clause.locations.back(), 10000);
            rewriter.removeRange(SourceRange(b, e), opts);
        }

        activeClauses.pop_back();
    }


private:
    string getTokenName(const Token& token) const {
        const char* b = sourceManager.getCharacterData(token.getLocation(), 0);
        const char* e = b + token.getLength();
        return string(b, e);
    }

    SourceLocation changeColumn(SourceLocation loc, unsigned col) const {
        pair<FileID, unsigned> decomposedLoc = sourceManager.getDecomposedLoc(loc);
        FileID fileId = decomposedLoc.first;
        unsigned filePos = decomposedLoc.second;
        unsigned line = sourceManager.getLineNumber(fileId, filePos);
        return sourceManager.translateLineCol(fileId, line, col);
    }

    bool containsWhitelistedString(SourceRange range) const {
        const char* b = sourceManager.getCharacterData(range.getBegin());
        const char* e = sourceManager.getCharacterData(range.getEnd());
        string rangeContent(b, e);
        for (const auto& s: macrosToKeep)
            if (rangeContent.find(s) != string::npos)
                return true;
        return false;
    }
};


RemoveInactivePreprocessorBlocks::RemoveInactivePreprocessorBlocks(
        SourceManager& sourceManager, SmartRewriter& rewriter,
        const set<string>& macrosToKeep)
    : impl(new RemoveInactivePreprocessorBlocksImpl(sourceManager, rewriter, macrosToKeep))
{
}

RemoveInactivePreprocessorBlocks::~RemoveInactivePreprocessorBlocks() {
    delete impl;
}

void RemoveInactivePreprocessorBlocks::MacroDefined(
        const Token& MacroNameTok, const MacroDirective* MD)
{
    impl->MacroDefined(MacroNameTok, MD);
}

void RemoveInactivePreprocessorBlocks::MacroUndefined(
        const Token& MacroNameTok, const MacroDirective* MD)
{
    impl->MacroUndefined(MacroNameTok, MD);
}

void RemoveInactivePreprocessorBlocks::MacroExpands(
        const Token& MacroNameTok, const MacroDirective* MD,
        SourceRange Range, const MacroArgs* Args)
{
    impl->MacroExpands(MacroNameTok, MD, Range, Args);
}

void RemoveInactivePreprocessorBlocks::Finalize() {
    impl->Finalize();
}

void RemoveInactivePreprocessorBlocks::If(
        SourceLocation Loc, SourceRange ConditionRange,
        ConditionValueKind ConditionValue)
{
    impl->If(Loc, ConditionRange, ConditionValue);
}

void RemoveInactivePreprocessorBlocks::Ifdef(
        SourceLocation Loc, const Token& MacroNameTok, const MacroDirective* MD)
{
    impl->Ifdef(Loc, MacroNameTok, MD);
}

void RemoveInactivePreprocessorBlocks::Ifndef(
        SourceLocation Loc, const Token& MacroNameTok, const MacroDirective* MD)
{
    impl->Ifndef(Loc, MacroNameTok, MD);
}

void RemoveInactivePreprocessorBlocks::Elif(
        SourceLocation Loc, SourceRange ConditionRange,
        ConditionValueKind ConditionValue, SourceLocation IfLoc)
{
    impl->Elif(Loc, ConditionRange, ConditionValue, IfLoc);
}

void RemoveInactivePreprocessorBlocks::Else(SourceLocation Loc, SourceLocation IfLoc) {
    impl->Else(Loc, IfLoc);
}

void RemoveInactivePreprocessorBlocks::Endif(SourceLocation Loc, SourceLocation IfLoc) {
    impl->Endif(Loc, IfLoc);
}

