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
        //return loc.printToString(sourceManager);
        std::ostringstream os;
        os << sourceManager.getSpellingLineNumber(loc) << ":" <<
              sourceManager.getSpellingColumnNumber(loc);
        return os.str();
    }
    std::string toString(SourceRange range) const {
        return toString(range.getBegin()) + " -- " + toString(range.getEnd());
    }

    bool isUserFile(SourceLocation loc) const {
        return !sourceManager.isInSystemHeader(loc) && loc.isValid();
    }

    void insertReference(Decl* from, Decl* to) {
        if (!from || !to)
            return;
        from = from->getCanonicalDecl();
        to = to->getCanonicalDecl();
        if (!isUserFile(to->getLocation())) {
            // reference to standard library
            return;
        }
        uses[from].insert(to);
        std::cerr << "Reference from <"
                  << declToString(from).substr(0, 20)
                  << ">" << toString(from->getSourceRange()) << " to <"
                  << declToString(to).substr(0, 20)
                  << ">" << toString(to->getSourceRange()) << "\n";
    }

public:
    DependenciesCollector(SourceManager& srcMgr, References& uses)
        : sourceManager(srcMgr)
        , uses(uses)
    {}
    bool shouldVisitImplicitCode() const { return true; }

    bool VisitCallExpr(CallExpr* callExpr) {
        Expr* callee = callExpr->getCallee();
        Decl* calleeDecl = callExpr->getCalleeDecl();
        if (isUserFile(calleeDecl->getCanonicalDecl()->getSourceRange().getBegin()))
            insertReference(currentDecl, calleeDecl);
        return true;
    }

    bool VisitCXXConstructExpr(CXXConstructExpr* constructorExpr) {
        insertReference(currentDecl, constructorExpr->getConstructor());
        // implicit constructor may no be visited; make sure we add dependency on its class
        insertReference(currentDecl, constructorExpr->getConstructor()->getParent());
        return true;
    }

    bool VisitDeclRefExpr(DeclRefExpr* ref) {
        //std::cerr << "Visiting declref at " << toString(ref->getSourceRange()) << std::endl;
        insertReference(currentDecl, ref->getDecl());
        return true;
    }

    bool VisitValueDecl(ValueDecl* valueDecl) {
        const Type* valueType = valueDecl->getType().getTypePtrOrNull();
        if (valueType)
            insertReference(valueDecl, valueType->getAsCXXRecordDecl());
        return true;
    }

    bool VisitCXXConstructorDecl(CXXConstructorDecl* constructorDecl) {
        // TODO
        cerr << "Visit constructor: " << declToString(constructorDecl) << endl;
        return true;
    }

    bool VisitMemberExpr(MemberExpr* memberExpr) {
        insertReference(currentDecl, memberExpr->getMemberDecl());
        return true;
    }

    bool VisitFieldDecl(FieldDecl* field) {
        insertReference(field, field->getParent());
        return true;
    }

    bool VisitFunctionDecl(FunctionDecl* f) {
        if (f->hasBody()) {
            currentDecl = f;

            // Function name
            DeclarationName DeclName = f->getNameInfo().getName();
            string FuncName = DeclName.getAsString();

            std::cerr << "Moving to " << FuncName << " at " << toString(f->getLocation()) << std::endl;
        }
        return true;
    }

    bool VisitCXXMethodDecl(CXXMethodDecl* method) {
        insertReference(method, method->getParent());
        return true;
    }

    bool VisitCXXRecordDecl(CXXRecordDecl* recordDecl) {
        // TODO dependencies on base classes?
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
        if (funcIsUnused || thisIsRedeclaration)
            removeDecl(functionDecl);
        declared.insert(canonicalDecl);
        return false;
    }

    bool VisitCXXRecordDecl(CXXRecordDecl* recordDecl) {
        CXXRecordDecl* canonicalDecl = recordDecl->getCanonicalDecl();
        const bool classIsUnused = used.find(canonicalDecl) == used.end();
        const bool thisIsRedeclaration = !recordDecl->isCompleteDefinition() && declared.find(canonicalDecl) != declared.end();

        if (classIsUnused || thisIsRedeclaration)
            removeDecl(recordDecl);
        declared.insert(canonicalDecl);
        return false;
    }

    // TODO remove #pragma once
    // TODO duplicate using directives
    // TODO remove member fields/methods in classes that are not used as template parameters of STL

private:
    void removeDecl(Decl* decl) {
        SourceLocation start = decl->getLocStart();
        SourceLocation end = decl->getLocEnd();
        SourceLocation semicolonAfterDefinition = findSemiAfterLocation(end, decl->getASTContext());
        std::cerr << "REMOVE: " << toString(start) << " " << toString(end)
           << " " << toString(semicolonAfterDefinition) << std::endl;
        if (semicolonAfterDefinition.isValid())
            end = semicolonAfterDefinition;
        Rewriter::RewriteOptions opts;
        opts.RemoveLineIfEmpty = true;
        rewriter.RemoveText(SourceRange(start, end), opts);
    }
    bool isDeclUsed(Decl* decl) const {
        return used.find(decl->getCanonicalDecl()) != used.end();
    }
    std::string toString(const SourceLocation& loc) const {
        return loc.printToString(sourceManager);
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

