#include "util.h"

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
/// of the semicolon following the statement.
/// If no semicolon is found or the location is inside a macro, the returned
/// source location will be invalid.
SourceLocation findSemiAfterLocation(SourceLocation loc, ASTContext& Ctx) {
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
    if (tok.isNot(tok::semi))
        return SourceLocation();

    return tok.getLocation();
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
