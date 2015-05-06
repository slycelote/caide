#include "RemoveInactivePreprocessorBlocks.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Preprocessor.h"

#include <map>
#include <vector>

#include "SmartRewriter.h"

using namespace std;
using namespace clang;

struct IfDefClause {
    // Locations of #if, #ifdef, #ifndef, #else, #elif tokens of this clause
    vector<SourceLocation> locations;

    // Index of selected branch in locations list; -1 if no branch was selected
    int selectedBranch;

    bool keepAllBranches;

    explicit IfDefClause(const SourceLocation& ifLocation)
        : selectedBranch(-1)
        , keepAllBranches(false)
    {
        locations.push_back(ifLocation);
    }
};

struct Macro {
    SourceRange definition;
    vector<SourceRange> usages;
    SourceLocation undefinition;
};

struct RemoveInactivePreprocessorBlocks::RemoveInactivePreprocessorBlocksImpl {
private:
    SourceManager& sourceManager;
    SmartRewriter& rewriter;
    const set<string>& macrosToKeep;

    vector<IfDefClause> activeClauses;
    set<string> definedMacroNames;

    // Currently defined macros
    map<const MacroDirective*, Macro> definedMacros;

    // Macros that were #defined, and then #undefined.
    // We need to keep track of them separately because corresponding MacroDirective
    // structure is released when the macro is undefined.
    vector<Macro> undefinedMacros;

    vector<SourceRange> inactiveBranches;

private:
    bool isWhitelistedMacro(const string& macroName) const {
        return macrosToKeep.find(macroName) != macrosToKeep.end();
    }

    void removeMacro(const Macro& macro) {
        for (const SourceRange& usageRange : macro.usages) {
            if (rewriter.canRemoveRange(usageRange)) {
                // The usage of the macro has not been removed, so
                // we can't remove the definition
                return;
            }
        }

        Rewriter::RewriteOptions opts;
        opts.RemoveLineIfEmpty = true;

        rewriter.removeRange(macro.definition, opts);

        if (macro.undefinition.isValid()) {
            SourceLocation b = changeColumn(macro.undefinition, 1);
            SourceLocation e = changeColumn(macro.undefinition, 10000);
            rewriter.removeRange(SourceRange(b, e), opts);
        }
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

        if (MD && isInMainFile(MD->getLocation())) {
            SourceLocation b = MD->getLocation(), e;
            if (const MacroInfo* info = MD->getMacroInfo())
                e = info->getDefinitionEndLoc();
            else
                e = b;

            b = changeColumn(b, 1);
            e = changeColumn(e, 10000);

            definedMacros[MD].definition = SourceRange(b, e);
        }
    }

    void MacroUndefined(const Token& MacroNameTok, const MacroDirective* MD) {
        definedMacroNames.erase(getTokenName(MacroNameTok));

        if (!MD || !isInMainFile(MD->getLocation()))
            return;

        auto it = definedMacros.find(MD);
        if (it != definedMacros.end()) {
            it->second.undefinition = MacroNameTok.getLocation();

            undefinedMacros.push_back(it->second);
            definedMacros.erase(it);
        }
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
            removeMacro(it->second);

        // Remove unused #define / #undef pairs
        for (const Macro& macro : undefinedMacros)
            removeMacro(macro);

        Rewriter::RewriteOptions opts;
        opts.RemoveLineIfEmpty = true;

        for (const SourceRange& range: inactiveBranches)
            rewriter.removeRange(range, opts);
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

        if (clause.keepAllBranches) {
            // do nothing
        } else if (clause.selectedBranch < 0) {
            // remove all branches
            SourceLocation b = changeColumn(clause.locations.front(), 1);
            SourceLocation e = changeColumn(clause.locations.back(), 10000);
            inactiveBranches.push_back(SourceRange(b, e));
        } else {
            // remove all branches except selected
            SourceLocation b = changeColumn(clause.locations.front(), 1);
            SourceLocation e = changeColumn(clause.locations[clause.selectedBranch], 10000);
            inactiveBranches.push_back(SourceRange(b, e));

            b = changeColumn(clause.locations[clause.selectedBranch + 1], 1);
            e = changeColumn(clause.locations.back(), 10000);
            inactiveBranches.push_back(SourceRange(b, e));
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

