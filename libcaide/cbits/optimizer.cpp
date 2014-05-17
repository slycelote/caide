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


#include "optimizer.h"
#include "util.h"

using namespace clang;
using namespace std;

typedef std::map<Decl*, std::set<Decl*> > References;

class DependenciesCollector : public RecursiveASTVisitor<DependenciesCollector> {
private:
    SourceManager& sourceManager;

    References& uses;
    Decl* currentDecl;

private:
    std::string toString(SourceLocation loc) const {
        return loc.printToString(sourceManager);
    }
    std::string toString(SourceRange range) const {
        return toString(range.getBegin()) + " -- " + toString(range.getEnd());
    }

    bool isUserFile(SourceLocation loc) const {
        return !sourceManager.isInSystemHeader(loc) && loc.isValid();
    }

    void insertReference(Decl* from, Decl* to) {
        from = from->getCanonicalDecl();
        to = to->getCanonicalDecl();
        if (to->getDeclContext()->isFunctionOrMethod()) {
            // reference to local variable
            return;
        }
        if (!isUserFile(to->getLocation())) {
            // reference to standard library
            return;
        }
        uses[from].insert(to);
        std::cerr << "Reference from <" << toString(from->getSourceRange()) << ">"
                  << " to <" << toString(to->getSourceRange())
                  << std::endl;
    }

public:
    DependenciesCollector(SourceManager& srcMgr, References& uses)
        : sourceManager(srcMgr)
        , uses(uses)
    {}

    bool VisitCallExpr(CallExpr* callExpr) {
        Expr* callee = callExpr->getCallee();
        Decl* calleeDecl = callExpr->getCalleeDecl();
        if (!isUserFile(calleeDecl->getCanonicalDecl()->getSourceRange().getBegin())) {
            return true;
        }
        insertReference(currentDecl, calleeDecl);
        return true;
    }

    bool VisitCXXConstructExpr(CXXConstructExpr* constructorExpr) {
        insertReference(currentDecl, constructorExpr->getConstructor());
        return true;
    }

    bool VisitDeclRefExpr(DeclRefExpr* ref) {
        std::cerr << "Visiting declref at " << toString(ref->getSourceRange()) << std::endl;
        insertReference(currentDecl, ref->getDecl());
        return true;
    }

    bool VisitCXXConstructorDecl(CXXConstructorDecl* constructorDecl) {
        // TODO
        return true;
    }

    bool VisitMemberExprDecl(MemberExpr* memberExpr) {
        insertReference(currentDecl, memberExpr->getMemberDecl());
        return true;
    }

    bool VisitFunctionDecl(FunctionDecl* f) {
        if (f->hasBody()) {
            currentDecl = f;
            Stmt* FuncBody = f->getBody();

            // Type name as string
            QualType QT = f->getResultType();
            string TypeStr = QT.getAsString();

            // Function name
            DeclarationName DeclName = f->getNameInfo().getName();
            string FuncName = DeclName.getAsString();

            std::cerr << "Moving to " << FuncName << " at " << toString(f->getLocation()) << std::endl;
        }
        return true;
    }

};


class OptimizerVisitor: public RecursiveASTVisitor<OptimizerVisitor> {
private:
    SourceManager& sourceManager;
    const std::set<Decl*>& used;
    std::set<Decl*> declared;
    Rewriter& rewriter;

public:
    OptimizerVisitor(SourceManager& srcManager, const std::set<Decl*>& used, Rewriter& rewriter)
        : sourceManager(srcManager)
        , used(used)
        , rewriter(rewriter)
    {}

    bool VisitFunctionDecl(FunctionDecl* functionDecl) {
        FunctionDecl* canonicalDecl = functionDecl->getCanonicalDecl();
        const bool funcIsUnused = used.find(canonicalDecl) == used.end();
        const bool thisIsRedeclaration = !functionDecl->doesThisDeclarationHaveABody() && declared.find(canonicalDecl) != declared.end();
        if (funcIsUnused || thisIsRedeclaration) {
            Rewriter::RewriteOptions opts;
            SourceLocation start = functionDecl->getLocStart();
            SourceLocation end = functionDecl->getLocEnd();
            SourceLocation semicolonAfterDefinition = findLocationAfterSemi(end, functionDecl->getASTContext());
            if (semicolonAfterDefinition.isValid())
                end = semicolonAfterDefinition;
            rewriter.RemoveText(SourceRange(start, end), opts);
        }
        declared.insert(canonicalDecl);
        return false;
    }
};

class OptimizerConsumer: public ASTConsumer {
public:
    explicit OptimizerConsumer(SourceManager& srcMgr, Rewriter& rewriter)
        : sourceManager(srcMgr)
        , rewriter(rewriter)
    {}

    virtual bool HandleTopLevelDecl(DeclGroupRef DR) {
        for (DeclGroupRef::iterator b = DR.begin(), e = DR.end(); b != e; ++b) {
            Decl* decl = *b;
            if (sourceManager.isInMainFile(decl->getLocStart())) {
                topLevelDecls.push_back(decl);
                DependenciesCollector visitor(sourceManager, uses);
                visitor.TraverseDecl(decl);
            }
        }
        return true;
    }

    virtual void HandleTranslationUnit(ASTContext& Ctx) {
        // Search for used decls
        std::set<Decl*> used;
        for (size_t i = 0; i < topLevelDecls.size(); ++i) {
            FunctionDecl* functionDecl = dyn_cast<FunctionDecl>(topLevelDecls[i]);
            if (functionDecl && functionDecl->isMain()) {
                std::set<Decl*> queue;
                queue.insert(functionDecl->getCanonicalDecl());
                while (!queue.empty()) {
                    Decl* decl = *queue.begin();
                    queue.erase(queue.begin());
                    if (used.insert(decl).second) {
                        queue.insert(uses[decl].begin(), uses[decl].end());
                    }
                }
                break;
            }
        }
        OptimizerVisitor visitor(sourceManager, used, rewriter);
        for (size_t i = 0; i < topLevelDecls.size(); ++i) {
            visitor.TraverseDecl(topLevelDecls[i]);
        }
    }

    std::string getResult() const {
        // At this point the rewriter's buffer should be full with the rewritten
        // file contents.
        const RewriteBuffer* RewriteBuf = rewriter.getRewriteBufferFor(sourceManager.getMainFileID());
        if (RewriteBuf)
            return std::string(RewriteBuf->begin(), RewriteBuf->end());
        else {
            // No changes
            bool invalid;
            const llvm::MemoryBuffer* buf = sourceManager.getBuffer(sourceManager.getMainFileID(), &invalid);
            if (buf && !invalid)
                return std::string(buf->getBufferStart(), buf->getBufferEnd());
            else
                return ""; // something's wrong
        }
    }

private:
    SourceManager& sourceManager;
    Rewriter& rewriter;
    References uses;
    std::vector<Decl*> topLevelDecls;
};

// <Warning!!> -- Platform Specific Code lives here
// This depends on A) that you're running linux and
// B) that you have the same GCC LIBs installed that
// I do.
// Search through Clang itself for something like this,
// go on, you won't find it. The reason why is Clang
// has its own versions of std* which are installed under
// /usr/local/lib/clang/<version>/include/
// See somewhere around Driver.cpp:77 to see Clang adding
// its version of the headers to its include path.
static const char* systemPaths[] = {
    "/usr/include",
    "/usr/include/i386-linux-gnu",
    "/usr/lib/gcc/i686-linux-gnu/4.6/include",
    "/usr/include/c++/4.6",
    "/usr/include/c++/4.6/i686-linux-gnu",
};
// </Warning!!> -- End of Platform Specific Code

Optimizer::Optimizer(const std::vector<std::string>& systemHeadersDirectories):
    systemHeadersDirectories(systemHeadersDirectories)
{}

std::string Optimizer::doOptimize(const std::string& cppFile) {
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
    FileManager& fileManager = compiler.getFileManager();
    compiler.createSourceManager(fileManager);
    SourceManager& sourceManager = compiler.getSourceManager();
    compiler.createPreprocessor();
    compiler.getPreprocessor().getBuiltinInfo().InitializeBuiltins(
        compiler.getPreprocessor().getIdentifierTable(),
        compiler.getPreprocessor().getLangOpts());

    llvm::IntrusiveRefCntPtr<clang::HeaderSearchOptions> hso(new clang::HeaderSearchOptions);
    HeaderSearch headerSearch(hso, sourceManager, compiler.getDiagnostics(), compiler.getLangOpts(), TI);
    for (size_t i = 0; i < systemHeadersDirectories.size(); ++i)
        hso->AddPath(systemHeadersDirectories[i], clang::frontend::System, false, false);

    clang::InitializePreprocessor(compiler.getPreprocessor(), compiler.getPreprocessorOpts(),
                                  *hso, compiler.getFrontendOpts());

    compiler.createASTContext();

    // Set the main file handled by the source manager to the input file.
    const FileEntry* inputFile = fileManager.getFile(cppFile.c_str());
    if (!inputFile)
        return cppFile + ": File doesn't exist";
    sourceManager.createMainFileID(inputFile);
    compiler.getDiagnosticClient().BeginSourceFile(compiler.getLangOpts(), &compiler.getPreprocessor());

    // Create an AST consumer instance which is going to get called by ParseAST.
    Rewriter rewriter;
    rewriter.setSourceMgr(sourceManager, compiler.getLangOpts());
    OptimizerConsumer consumer(sourceManager, rewriter);

    // Parse the file to AST, registering our consumer as the AST consumer.
    ParseAST(compiler.getPreprocessor(), &consumer, compiler.getASTContext());
    compiler.getDiagnosticClient().EndSourceFile();

    return consumer.getResult();
}

