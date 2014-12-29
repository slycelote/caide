#include <cstdio>
#include <iostream>
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
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/Utils.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Parse/ParseAST.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"

#include "util.h"
#include "inliner.h"

using namespace clang;
using namespace std;

class TrackMacro: public PPCallbacks {
public:
    TrackMacro(SourceManager& srcManager, std::set<std::string>& includedHeaders)
        : srcManager(srcManager)
        , includedHeaders(includedHeaders)
    {
        // Setup a placeholder where the result for the whole CPP file will be stored
        replacementStack.resize(1);
        replacementStack.back().fileName = "<CPP>";
    }

    virtual void InclusionDirective(SourceLocation HashLoc,
                                    const Token &IncludeTok,
                                    StringRef FileName,
                                    bool IsAngled,
                                    CharSourceRange FilenameRange,
                                    const FileEntry *File,
                                    StringRef SearchPath,
                                    StringRef RelativePath,
                                    const Module *Imported)
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
        rep.fileName = srcManager.getFilename(HashLoc).data();
        rep.replaceWith = string(Z(s), Z(e)) + "\n";
        replacementStack.push_back(rep);
    }

    virtual void FileChanged(SourceLocation Loc, FileChangeReason Reason,
                             SrcMgr::CharacteristicKind FileType,
                             FileID PrevFID/* = FileID()*/)
    {
        //llvm::errs() << "File changed to " << srcManager.getFilename(Loc) << "\n";
        const FileEntry* curEntry = srcManager.getFileEntryForID(PrevFID);
        if (Reason == PPCallbacks::ExitFile && curEntry) {
            // Don't track system headers including each other
            if (!isUserFile(Loc))
                return;
            // Rewind replacement stack and compute result of including current file.
            std::string currentFile = curEntry->getName();
//            llvm::errs() << "Exiting from " << currentFile << "\n";

            // - Search the stack for the topmost replacement belonging to another file.
            //   That's where we were included from.
            int includedFrom = int(replacementStack.size()) - 1;
            while (replacementStack[includedFrom].fileName == currentFile)
                --includedFrom;

            // - Mark this header as visited for future CPP files.
            if (!includedHeaders.insert(currentFile).second) {
                // - If current header should be skipped, set empty replacement
                //llvm::errs() << currentFile << " was seen\n";
                replacementStack[includedFrom].replaceWith = "";
            } else if (isSystemHeader(PrevFID)) {
                // - This is a new system header. Leave include directive as is,
                //   i. e. do nothing.
                //llvm::errs() << currentFile << " hasn't been seen " << srcManager.getFilename(Loc) << "\n";
            } else {
                // - This is a new user header. Apply all replacements from current file.
                replacementStack[includedFrom].replaceWith = calcReplacements(includedFrom, PrevFID);
            }

            //llvm::errs() << currentFile << "  --->  " << (includedFrom + 1) << "\n";
            // - Actually rewind.
            replacementStack.resize(includedFrom + 1);
        }
    }

    virtual void EndOfMainFile() {
        replacementStack[0].replaceWith = calcReplacements(0, srcManager.getMainFileID());
        replacementStack.resize(1);
    }

    // Documentation seems to be wrong: the first parameter is included file rather than parent
    // TODO: move detection of double inclusion to optimizer step?
    virtual void FileSkipped(const FileEntry &IncludedFile, const Token &FilenameTok,
                             SrcMgr::CharacteristicKind FileType)
    {
        // Don't track system headers including each other
        if (!srcManager.isInSystemHeader(FilenameTok.getLocation())) {
            // File skipped as part of normal header guard optimization / #pragma once
            //
            // It's important to do a manual check here because in other versions of STL
            // the header may not have been included. In other words, we need to explicitly
            // include every file that we use.
            if (!includedHeaders.insert(IncludedFile.getName()).second)
                replacementStack.back().replaceWith = "";
            //llvm::errs() << "Skip include of " << IncludedFile.getName() << " from " << srcManager.getFilename(FilenameTok.getLocation())<< "#" << srcManager.getSpellingLineNumber(FilenameTok.getLocation()) << " " << replacementStack.back().fileName << "\n";
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

    struct IncludeReplacement {
        SourceRange includeDirectiveRange;
        std::string fileName;
        std::string replaceWith;
    };

    /*
     * A 'stack' of replacements, reflecting current include stack.
     * Replacements in the same file are ordered by their location.
     * Replacement string may be empty which means that we skip this include file.
     */
    std::vector<IncludeReplacement> replacementStack;

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
                blockStart = replacementStack[i-1].includeDirectiveRange.getEnd().getLocWithOffset(1);

            if (i == int(replacementStack.size()))
                blockEnd = srcManager.getLocForEndOfFile(currentFID);
            else
                blockEnd = replacementStack[i].includeDirectiveRange.getBegin().getLocWithOffset(-1);

            /*
                        llvm::errs() << "Start: ";
                        blockStart.print(llvm::errs(), srcManager);
                        llvm::errs() << ", end: ";
                        blockEnd.print(llvm::errs(), srcManager);
                        llvm::errs() << "\n";
            */
            // skip cases when two include directives are adjacent
            //   or an include directive is in the beginning or end of file
            if (blockStart.isValid() && blockEnd.isValid() &&
                    srcManager.isBeforeInSLocAddrSpace(blockStart, blockEnd)) {
                bool invalid;
                const char* b = srcManager.getCharacterData(blockStart, &invalid);
                const char* e = srcManager.getCharacterData(blockEnd, &invalid);
                result << std::string(Z(b), Z(e)) << "\n";
            }

            // Now output the result of file inclusion
            if (i != int(replacementStack.size()))
                result << replacementStack[i].replaceWith;
        }
        return result.str();
    }

    bool isSystemHeader(FileID header) const {
        SourceLocation loc = srcManager.getLocForStartOfFile(header);
        return srcManager.isInSystemHeader(loc);
    }

    bool isSystemHeader(const FileEntry* entry) const {
        return isSystemHeader(srcManager.translateFile(Z(entry)));
    }

    bool isUserFile(SourceLocation loc) const {
        return !srcManager.isInSystemHeader(loc) && loc.isValid();
    }

    bool wasSeen(const std::string& filename) const {
        return includedHeaders.find(filename) != includedHeaders.end();
    }

    void debug() const {
        for (size_t i = 0; i < replacementStack.size(); ++i) {
            llvm::errs() << replacementStack[i].fileName << " " <<
                         replacementStack[i].replaceWith << "\n";
        }
    }
};


Inliner::Inliner(const std::vector<std::string>& systemHeadersDirectories,
                 const std::vector<std::string>& userHeadersDirectories)
    : systemHeadersDirectories(systemHeadersDirectories)
    , userHeadersDirectories(userHeadersDirectories)
{}

std::string Inliner::doInline(const std::string& cppFile) {
    // CompilerInstance will hold the instance of the Clang compiler for us,
    // managing the various objects needed to run the compiler.
    CompilerInstance compiler;
    compiler.createDiagnostics(0, false);

    // Initialize target info with the default triple for our platform.
    IntrusiveRefCntPtr<TargetOptions> TO(new TargetOptions);
    TO->Triple = llvm::sys::getDefaultTargetTriple();
    TargetInfo* TI = TargetInfo::CreateTargetInfo(compiler.getDiagnostics(), TO.getPtr());
    compiler.setTarget(TI);
    CompilerInvocation::setLangDefaults(compiler.getLangOpts(), IK_CXX);
    compiler.getLangOpts().CPlusPlus = 1;
    compiler.getLangOpts().CPlusPlus11 = 1;

    compiler.createFileManager();
    FileManager& fileMgr = compiler.getFileManager();
    compiler.createSourceManager(fileMgr);
    SourceManager& sourceMgr = compiler.getSourceManager();
    compiler.createPreprocessor();
    compiler.getPreprocessor().getBuiltinInfo().InitializeBuiltins(
        compiler.getPreprocessor().getIdentifierTable(),
        compiler.getPreprocessor().getLangOpts());
    TrackMacro* tracker = new TrackMacro(sourceMgr, includedHeaders);
    compiler.getPreprocessor().addPPCallbacks(tracker);

    llvm::IntrusiveRefCntPtr<clang::HeaderSearchOptions> hso(new clang::HeaderSearchOptions);
    HeaderSearch headerSearch(hso, sourceMgr, compiler.getDiagnostics(),
                              compiler.getLangOpts(), TI);
    for (size_t i = 0; i < systemHeadersDirectories.size(); ++i)
        hso->AddPath(systemHeadersDirectories[i], clang::frontend::System, false, false);
    for (size_t i = 0; i < userHeadersDirectories.size(); ++i)
        hso->AddPath(userHeadersDirectories[i], clang::frontend::Quoted, false, false);

    clang::InitializePreprocessor(compiler.getPreprocessor(), compiler.getPreprocessorOpts(),
                                  *hso, compiler.getFrontendOpts());

    compiler.createASTContext();

    // Set the main file handled by the source manager to the input file.
    const FileEntry* fileIn = fileMgr.getFile(cppFile.c_str());
    if (!fileIn)
        return cppFile + ": File doesn't exist";
    sourceMgr.createMainFileID(fileIn);
    compiler.getDiagnosticClient().BeginSourceFile(compiler.getLangOpts(), &compiler.getPreprocessor());

    // Create an AST consumer instance which is going to get called by ParseAST.
    ASTConsumer consumer;

    // Parse the file to AST, registering our consumer as the AST consumer.
    ParseAST(compiler.getPreprocessor(), &consumer, compiler.getASTContext());
    compiler.getDiagnosticClient().EndSourceFile();
    tracker->EndOfMainFile();

    inlineResults.push_back(tracker->getResult());
    return inlineResults.back();
}

