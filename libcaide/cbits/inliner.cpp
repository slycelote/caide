#include <cstdio>
#include <iostream>
#include <stdexcept>
#include <string>
#include <sstream>

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetOptions.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Frontend/ASTConsumers.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/Utils.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Parse/ParseAST.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Tooling.h"

#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"

#include "util.h"
#include "inliner.h"

using namespace clang;
using namespace std;

struct IncludeReplacement {
    SourceRange includeDirectiveRange;
    std::string fileName;
    std::string replaceWith;
};

class TrackMacro: public PPCallbacks {
public:
    TrackMacro(SourceManager& _srcManager, std::set<std::string>& _includedHeaders,
               std::vector<IncludeReplacement>& _replacements)
        : srcManager(_srcManager)
        , includedHeaders(_includedHeaders)
        , replacementStack(_replacements)
    {
        // Setup a placeholder where the result for the whole CPP file will be stored
        replacementStack.resize(1);
        replacementStack.back().fileName = "<CPP>";
    }

    virtual void InclusionDirective(SourceLocation HashLoc,
                                    const Token& /*IncludeTok*/,
                                    StringRef FileName,
                                    bool /*IsAngled*/,
                                    CharSourceRange FilenameRange,
                                    const FileEntry *File,
                                    StringRef /*SearchPath*/,
                                    StringRef /*RelativePath*/,
                                    const Module* /*Imported*/)
    {
        if (FileName.empty())
            return;
        // Don't track system headers including each other
        // They may include the same file multiple times (no include guards) and do other crazy stuff
        if (!isUserFile(HashLoc))
            return;

        // Inclusion directive is encountered.
        // Setup a placeholder in inclusion stack where the result of this
        // directive will be stored. Initially assume the directive remains unchanged
        // (this is the case if it's a new system header).
        SourceLocation end = FilenameRange.getEnd();
        const char* s = srcManager.getCharacterData(HashLoc);
        const char* e = srcManager.getCharacterData(end);

        if (!File) {
            llvm::errs() << "Compilation error: " << FileName.data() << " not found\n";
            return;
        }

        IncludeReplacement rep;
        rep.includeDirectiveRange = SourceRange(HashLoc, end);
        rep.fileName = getCanonicalPath(srcManager.getFileEntryForID(srcManager.getFileID(HashLoc)));
        if (s && e)
            rep.replaceWith = string(s, e);
        else
            rep.replaceWith = "<Inliner error>\n";
        replacementStack.push_back(rep);
    }

    virtual void FileChanged(SourceLocation Loc, FileChangeReason Reason,
                             SrcMgr::CharacteristicKind /*FileType*/,
                             FileID PrevFID/* = FileID()*/)
    {
        const FileEntry* curEntry = srcManager.getFileEntryForID(PrevFID);
        if (Reason == PPCallbacks::ExitFile && curEntry) {
            // Don't track system headers including each other
            if (!isUserFile(Loc))
                return;
            // Rewind replacement stack and compute result of including current file.
            std::string currentFile = getCanonicalPath(curEntry);

            // - Search the stack for the topmost replacement belonging to another file.
            //   That's where we were included from.
            int includedFrom = int(replacementStack.size()) - 1;
            while (replacementStack[includedFrom].fileName == currentFile)
                --includedFrom;

            // - Mark this header as visited for future CPP files.
            if (!markAsIncluded(currentFile)) {
                // - If current header should be skipped, set empty replacement
                //llvm::errs() << currentFile << " was seen\n";
                replacementStack[includedFrom].replaceWith = "";
            } else if (isSystemHeader(PrevFID)) {
                // - This is a new system header. Leave include directive as is,
                //   i. e. do nothing.
            } else {
                // - This is a new user header. Apply all replacements from current file.
                replacementStack[includedFrom].replaceWith = calcReplacements(includedFrom, PrevFID);
            }

            // - Actually rewind.
            replacementStack.resize(includedFrom + 1);
        }
    }

    virtual void EndOfMainFile() {
        replacementStack[0].replaceWith = calcReplacements(0, srcManager.getMainFileID());
        replacementStack.resize(1);
    }

    // Documentation seems to be wrong: the first parameter is included file rather than parent
    virtual void FileSkipped(const FileEntry &IncludedFile, const Token &FilenameTok,
                             SrcMgr::CharacteristicKind /*FileType*/)
    {
        // Don't track system headers including each other
        if (!srcManager.isInSystemHeader(FilenameTok.getLocation())) {
            // File skipped as part of normal header guard optimization / #pragma once
            //
            // It's important to do a manual check here because in other versions of STL
            // the header may not have been included. In other words, we need to explicitly
            // include every file that we use.
            if (!markAsIncluded(IncludedFile))
                replacementStack.back().replaceWith = "";
        }
    }

    std::string getResult() const {
        if (replacementStack.size() != 1)
            return "C++ inliner error";
        else
            return replacementStack[0].replaceWith;
    }

    virtual ~TrackMacro() {
    }

private:
    SourceManager& srcManager;

    /*
     * Headers that we already included. Since we want to process
     * multiple CPP files, we need to track this information explicitly.
     */
    std::set<std::string>& includedHeaders;

    /*
     * A 'stack' of replacements, reflecting current include stack.
     * Replacements in the same file are ordered by their location.
     * Replacement string may be empty which means that we skip this include file.
     */
    std::vector<IncludeReplacement>& replacementStack;

private:

    /*
     * Unwinds inclusion stack and calculates the result of inclusion of current file
     */
    std::string calcReplacements(int includedFrom, FileID currentFID) const {
        std::ostringstream result;
        // We go over each #include directive in current file and replace it
        // with the result of inclusion.
        // The last value of i doesn't correspond to an include directive,
        // it's used to output the part of the file after the last include directive.
        for (int i = includedFrom + 1; i <= int(replacementStack.size()); ++i) {
            // First output the block before the #include directive.
            // Block start is immediately after the previous include directive;
            // block end is immediately before current include directive.
            SourceLocation blockStart, blockEnd;

            if (i == includedFrom + 1)
                blockStart = srcManager.getLocForStartOfFile(currentFID);
            else
                blockStart = replacementStack[i-1].includeDirectiveRange.getEnd();

            if (i == int(replacementStack.size()))
                blockEnd = srcManager.getLocForEndOfFile(currentFID);
            else
                blockEnd = replacementStack[i].includeDirectiveRange.getBegin();

            // skip cases when two include directives are adjacent
            //   or an include directive is in the beginning or end of file
            if (blockStart.isValid() && blockEnd.isValid() &&
                    srcManager.isBeforeInSLocAddrSpace(blockStart, blockEnd)) {
                bool invalid;
                const char* b = srcManager.getCharacterData(blockStart, &invalid);
                const char* e = 0;
                if (!invalid)
                    e = srcManager.getCharacterData(blockEnd, &invalid);
                if (invalid || !b || !e)
                    result << "<Inliner error>\n";
                else
                    result << std::string(b, e);
            }

            // Now output the result of file inclusion
            if (i != int(replacementStack.size()))
                result << replacementStack[i].replaceWith;
        }
        return result.str();
    }

    std::string getCanonicalPath(const FileEntry* entry) const {
        const DirectoryEntry* dirEntry = entry->getDir();
        StringRef strRef = srcManager.getFileManager().getCanonicalName(dirEntry);
        std::string res(strRef.data());
        res.push_back('/');
        std::string fname(entry->getName());
        int i = (int)fname.size() - 1;
        while (i >= 0 && fname[i] != '/' && fname[i] != '\\')
            --i;
        res += fname.substr(i+1);
        return res;
    }

    bool markAsIncluded(const FileEntry& entry) {
        std::string fname = getCanonicalPath(&entry);
        return markAsIncluded(fname);
    }

    bool markAsIncluded(const std::string& canonicalPath) {
        return includedHeaders.insert(canonicalPath).second;
    }

    bool isSystemHeader(FileID header) const {
        SourceLocation loc = srcManager.getLocForStartOfFile(header);
        return srcManager.isInSystemHeader(loc);
    }

    bool isSystemHeader(const FileEntry* entry) const {
        return isSystemHeader(srcManager.translateFile(entry));
    }

    bool isUserFile(SourceLocation loc) const {
        return !srcManager.isInSystemHeader(loc) && loc.isValid();
    }

    void debug() const {
        for (size_t i = 0; i < replacementStack.size(); ++i) {
            llvm::errs() << replacementStack[i].fileName << " " <<
                         replacementStack[i].replaceWith << "\n";
        }
    }
};

class InlinerVisitor: public RecursiveASTVisitor<InlinerVisitor> {
    // noop visitor, we only need PPCallbacks
};

class InlinerConsumer: public ASTConsumer {
private:
    InlinerVisitor visitor;

public:
    virtual void HandleTranslationUnit(ASTContext &Context) {
        visitor.TraverseDecl(Context.getTranslationUnitDecl());
    }
};

class InlinerFrontendAction : public ASTFrontendAction {
private:
    std::vector<IncludeReplacement>& replacementStack;
    std::set<std::string>& includedHeaders;

public:
    InlinerFrontendAction(vector<IncludeReplacement>& _replacementStack,
                          set<string>& _includedHeaders)
        : replacementStack(_replacementStack)
        , includedHeaders(_includedHeaders)
    {}

    virtual std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance& compiler, StringRef /*file*/) {
        compiler.getPreprocessor().addPPCallbacks(std::unique_ptr<TrackMacro>(new TrackMacro(
                compiler.getSourceManager(), includedHeaders, replacementStack)));

        return std::unique_ptr<InlinerConsumer>(new InlinerConsumer());
    }
};

class InlinerFrontendActionFactory: public tooling::FrontendActionFactory {
private:
    std::vector<IncludeReplacement>& replacementStack;
    std::set<std::string>& includedHeaders;

public:
    InlinerFrontendActionFactory(vector<IncludeReplacement>& _replacementStack,
                                 set<string>& _includedHeaders)
        : replacementStack(_replacementStack)
        , includedHeaders(_includedHeaders)
    {}
    FrontendAction* create() {
        return new InlinerFrontendAction(replacementStack, includedHeaders);
    }
};

Inliner::Inliner(const std::vector<std::string>& _cmdLineOptions)
    : cmdLineOptions(_cmdLineOptions)
{}

std::string Inliner::doInline(const std::string& cppFile) {
    std::auto_ptr<clang::tooling::FixedCompilationDatabase> compilationDatabase(
        createCompilationDatabaseFromCommandLine(cmdLineOptions));

    std::vector<std::string> sources(1);
    sources[0] = cppFile;

    std::vector<IncludeReplacement> replacementStack;
    InlinerFrontendActionFactory factory(replacementStack, includedHeaders);

    clang::tooling::ClangTool tool(*compilationDatabase, sources);

    int ret = tool.run(&factory);

    if (ret != 0)
        throw std::runtime_error("Compilation error");
    else if (replacementStack.size() != 1)
        throw std::logic_error("Caide inliner error");

    inlineResults.push_back(replacementStack[0].replaceWith);
    return inlineResults.back();
}

