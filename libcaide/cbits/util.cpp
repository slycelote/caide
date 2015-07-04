#include "util.h"
#include <sstream>

#include "clang/AST/ASTContext.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/Utils.h"
#include "clang/Lex/Preprocessor.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"


using namespace clang;

// Copied from lib/ARCMigrate/Transforms.cpp

/// \brief \arg Loc is the end of a statement range. This returns the location
/// of the token of expected type following the statement.
/// If no token of this type is found or the location is inside a macro, the returned
/// source location will be invalid.
SourceLocation findTokenAfterLocation(SourceLocation loc, ASTContext& Ctx,
        tok::TokenKind tokenType)
{
    SourceManager &SM = Ctx.getSourceManager();
    if (loc.isMacroID()) {
        if (!Lexer::isAtEndOfMacroExpansion(loc, SM, Ctx.getLangOpts(), &loc))
            return SourceLocation();
    }
    loc = Lexer::getLocForEndOfToken(loc, /*Offset=*/0, SM, Ctx.getLangOpts());

    // Break down the source location.
    std::pair<FileID, unsigned> locInfo = SM.getDecomposedLoc(loc);

    // Try to load the file buffer.
    bool invalidTemp = false;
    StringRef file = SM.getBufferData(locInfo.first, &invalidTemp);
    if (invalidTemp)
        return SourceLocation();

    const char *tokenBegin = file.data() + locInfo.second;

    // Lex from the start of the given location.
    Lexer lexer(SM.getLocForStartOfFile(locInfo.first), Ctx.getLangOpts(),
            file.begin(), tokenBegin, file.end());
    Token tok;
    lexer.LexFromRawLexer(tok);
    if (tok.isNot(tokenType))
        return SourceLocation();

    return tok.getLocation();
}

SourceLocation findSemiAfterLocation(SourceLocation loc, ASTContext& Ctx) {
    return findTokenAfterLocation(loc, Ctx, tok::semi);
}

/// \brief 'Loc' is the end of a statement range. This returns the location
/// immediately after the semicolon following the statement.
/// If no semicolon is found or the location is inside a macro, the returned
/// source location will be invalid.
SourceLocation findLocationAfterSemi(SourceLocation loc, ASTContext &Ctx) {
    SourceLocation SemiLoc = findSemiAfterLocation(loc, Ctx);
    if (SemiLoc.isInvalid())
        return SourceLocation();
    return SemiLoc.getLocWithOffset(1);
}

clang::tooling::FixedCompilationDatabase* createCompilationDatabaseFromCommandLine(const std::vector<std::string> cmdLine)
{
    int argc = cmdLine.size() + 1;
    std::vector<const char*> argv(argc);
    argv[0] = "--";

    for (int i = 1; i < argc; ++i)
        argv[i] = cmdLine[i-1].c_str();

    return clang::tooling::FixedCompilationDatabase::loadFromCommandLine(argc, &argv[0]);
}

std::string rangeToString(SourceManager& sourceManager, const SourceLocation& start, const SourceLocation& end) {
    bool invalid;
    const char* b = sourceManager.getCharacterData(start, &invalid);
    if (invalid || !b)
        return "<invalid>";
    const char* e = sourceManager.getCharacterData(end, &invalid);
    if (invalid || !e)
        return "<invalid>";
    if (e < b + 30)
        return std::string(b, e);
    else
        return std::string(b, b+30) + "[...]";
}

std::string toString(SourceManager& sourceManager, SourceLocation loc) {
    //return loc.printToString(sourceManager);
    std::string fileName = sourceManager.getFilename(loc).str();
    if (fileName.length() > 30)
        fileName = fileName.substr(fileName.length() - 30);
    std::ostringstream os;
    os << fileName << ":" <<
        sourceManager.getExpansionLineNumber(loc) << ":" <<
        sourceManager.getExpansionColumnNumber(loc);
    return os.str();
}

std::string toString(SourceManager& sourceManager, SourceRange range) {
    return toString(sourceManager, range.getBegin()) + " -- " +
        toString(sourceManager, range.getEnd());
}

std::string toString(SourceManager& sourceManager, const Decl* decl) {
    if (!decl)
        return "<invalid>";
    SourceLocation start = sourceManager.getExpansionLoc(decl->getLocStart());
    bool invalid;
    const char* b = sourceManager.getCharacterData(start, &invalid);
    if (invalid || !b)
        return "<invalid>";
    SourceLocation end = sourceManager.getExpansionLoc(decl->getLocEnd());
    const char* e = sourceManager.getCharacterData(end, &invalid);
    if (invalid || !e)
        return "<invalid>";
    return std::string(b, std::min(b+30, e));
}

SourceLocation getExpansionStart(SourceManager& sourceManager, const Decl* decl) {
    SourceLocation start = decl->getLocStart();
    if (start.isMacroID())
        start = sourceManager.getExpansionRange(start).first;
    return start;
}

SourceLocation getExpansionEnd(SourceManager& sourceManager, const Decl* decl) {
    SourceLocation end = decl->getLocEnd();
    if (end.isMacroID())
        end = sourceManager.getExpansionRange(end).second;
    return end;
}

SourceRange getExpansionRange(SourceManager& sourceManager, const Decl* decl) {
    return SourceRange(getExpansionStart(sourceManager, decl),
            getExpansionEnd(sourceManager, decl));
}

