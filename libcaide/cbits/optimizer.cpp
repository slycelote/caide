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

/*
#define CLANG_Z(p, file, line) {if (!p) {errorMessage = "Null pointer at " file " " line; return false;}}

#define Z(p) CLANG_Z(p, __FILE__, __LINE)
*/

class null_stream: public ostream {
    template<typename T> ostream& operator<<(const T&) { return *this; }
};

ostream& dbg() {
    static null_stream null; return null;
    //return std::cerr;
}

typedef std::map<Decl*, std::set<Decl*> > References;

class DependenciesCollector : public RecursiveASTVisitor<DependenciesCollector> {
private:
    SourceManager& sourceManager;

    References& uses;

    // Current function. FIXME: this doesn't handle top-level declarations correctly.
    Decl* currentDecl;
    
    FunctionDecl* mainFunctionDecl;

private:
    std::string toString(SourceLocation loc) const {
        //return loc.printToString(sourceManager);
        std::string fileName = sourceManager.getFilename(loc).str();
        if (fileName.length() > 30) {
            fileName = fileName.substr(fileName.length() - 30);
        }
        std::ostringstream os;
        os << fileName << ":" << 
            sourceManager.getSpellingLineNumber(loc) << ":" <<
            sourceManager.getSpellingColumnNumber(loc);
        return os.str();
    }
    std::string toString(SourceRange range) const {
        return toString(range.getBegin()) + " -- " + toString(range.getEnd());
    }
    std::string toString(const Decl* decl) const {
        if (!decl)
            return "<invalid>";
        bool invalid;
        const char* b = sourceManager.getCharacterData(decl->getLocStart(), &invalid);
        if (invalid || !b)
            return "<invalid>";
        const char* e = sourceManager.getCharacterData(decl->getLocEnd(), &invalid);
        if (invalid || !e)
            return "<invalid>";
        return std::string(b, std::max(b+30, e));
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
        dbg() << "Reference from <"
                  << toString(from).substr(0, 20)
                  << ">" << toString(from->getSourceRange()) << " to <"
                  << toString(to).substr(0, 20)
                  << ">" << toString(to->getSourceRange()) << "\n";
    }

public:
    FunctionDecl* getMainFunction() const {
        return mainFunctionDecl;
    }

    DependenciesCollector(SourceManager& srcMgr, References& uses)
        : sourceManager(srcMgr)
        , uses(uses)
        , currentDecl(0)
        , mainFunctionDecl(0)
    {}
    bool shouldVisitImplicitCode() const { return true; }
    bool shouldVisitTemplateInstantiations() const { return true; }

    bool VisitCallExpr(CallExpr* callExpr) {
        dbg() << __FUNCTION__ << std::endl;
        Expr* callee = callExpr->getCallee();
        Decl* calleeDecl = callExpr->getCalleeDecl();
        if (!callee || !calleeDecl || isa<UnresolvedMemberExpr>(callee) || isa<CXXDependentScopeMemberExpr>(callee))
            return true;
        if (isUserFile(calleeDecl->getCanonicalDecl()->getSourceRange().getBegin()))
            insertReference(currentDecl, calleeDecl);
        return true;
    }

    bool VisitCXXConstructExpr(CXXConstructExpr* constructorExpr) {
        dbg() << __FUNCTION__ << std::endl;
        insertReference(currentDecl, constructorExpr->getConstructor());
        // implicit constructor may not be visited; make sure we add dependency on its class
        // TODO: ???
        //insertReference(currentDecl, constructorExpr->getConstructor()->getParent());
        return true;
    }

    bool VisitDeclRefExpr(DeclRefExpr* ref) {
        //dbg() << __FUNCTION__ << std::endl;
        //dbg() << "Visiting declref at " << toString(ref->getSourceRange()) << std::endl;
        insertReference(currentDecl, ref->getDecl());
        return true;
    }

    bool VisitValueDecl(ValueDecl* valueDecl) {
        dbg() << __FUNCTION__ << std::endl;
        const Type* valueType = valueDecl->getType().getTypePtrOrNull();
        if (valueType)
            insertReference(valueDecl, valueType->getAsCXXRecordDecl());
        return true;
    }

    bool VisitCXXConstructorDecl(CXXConstructorDecl* constructorDecl) {
        dbg() << __FUNCTION__ << std::endl;
        // TODO
        return true;
    }

    bool VisitMemberExpr(MemberExpr* memberExpr) {
        dbg() << __FUNCTION__ << std::endl;
        insertReference(currentDecl, memberExpr->getMemberDecl());
        return true;
    }

    bool VisitFieldDecl(FieldDecl* field) {
        dbg() << __FUNCTION__ << std::endl;
        insertReference(field, field->getParent());
        return true;
    }

    bool VisitClassTemplateDecl(ClassTemplateDecl* templateDecl) {
        dbg() << __FUNCTION__ << std::endl;
        for (ClassTemplateDecl::spec_iterator i = templateDecl->spec_begin();
                i != templateDecl->spec_end(); ++i)
        {
            //insertReference(*i, templateDecl);
        }
        insertReference(templateDecl, templateDecl->getTemplatedDecl());
        return true;
    }

    bool VisitClassTemplateSpecializationDecl(ClassTemplateSpecializationDecl* specDecl) {
        dbg() << __FUNCTION__ << std::endl;
        insertReference(specDecl, specDecl->getSpecializedTemplate());
        return true;
    }

    bool VisitFunctionDecl(FunctionDecl* f) {
        if (f->isMain()) {
            mainFunctionDecl = f;
        }
        if (f->getTemplatedKind() == FunctionDecl::TK_FunctionTemplate) {
            // skip non-instantiated template function
            return true;
        }
        //dbg() << __FUNCTION__ << std::endl;
        FunctionTemplateSpecializationInfo* specInfo = f->getTemplateSpecializationInfo();
        if (specInfo)
            insertReference(f, specInfo->getTemplate());
        if (f->hasBody()) {
            currentDecl = f;

            DeclarationName DeclName = f->getNameInfo().getName();
            string FuncName = DeclName.getAsString();

            dbg() << "Moving to " << FuncName << " at " << toString(f->getLocation()) << std::endl;
        }
        return true;
    }

    bool VisitCXXMethodDecl(CXXMethodDecl* method) {
        dbg() << __FUNCTION__ << std::endl;
        insertReference(method, method->getParent());
        return true;
    }

    bool VisitCXXRecordDecl(CXXRecordDecl* recordDecl) {
        dbg() << __FUNCTION__ << " " << recordDecl->getTemplateSpecializationKind() << std::endl;
        // TODO dependencies on base classes?
        insertReference(recordDecl, recordDecl->getDescribedClassTemplate());
        return true;
    }
};


class OptimizerVisitor: public RecursiveASTVisitor<OptimizerVisitor> {
private:
    SourceManager& sourceManager;
    const std::set<Decl*>& used;
    std::set<Decl*> declared;
    std::set<NamespaceDecl*> usedNamespaces;
    Rewriter& rewriter;

public:
    OptimizerVisitor(SourceManager& srcManager, const std::set<Decl*>& used, Rewriter& rewriter)
        : sourceManager(srcManager)
        , used(used)
        , rewriter(rewriter)
    {}

    bool VisitFunctionDecl(FunctionDecl* functionDecl) {
        if (!sourceManager.isInMainFile(functionDecl->getLocStart()))
            return true;
        FunctionDecl* canonicalDecl = functionDecl->getCanonicalDecl();
        if (canonicalDecl->getTemplatedKind() != FunctionDecl::TK_NonTemplate) {
            // Will be processed as FunctionTemplateDecl
            return true;
        }
        const bool funcIsUnused = used.find(canonicalDecl) == used.end();
        const bool thisIsRedeclaration = !functionDecl->doesThisDeclarationHaveABody() && declared.find(canonicalDecl) != declared.end();
        if (funcIsUnused || thisIsRedeclaration) {
            dbg() << __FUNCTION__ << std::endl;
            removeDecl(functionDecl);
        }
        declared.insert(canonicalDecl);
        return true;
    }

    // TODO: unused explicit function template specializations are not removed
    bool VisitFunctionTemplateDecl(FunctionTemplateDecl* functionDecl) {
        if (!sourceManager.isInMainFile(functionDecl->getLocStart()))
            return true;
        dbg() << __FUNCTION__ << std::endl;
        if (used.find(functionDecl) == used.end())
            removeDecl(functionDecl);
    }

    // TODO: unused explicit class template specializations are removed twice
    bool VisitCXXRecordDecl(CXXRecordDecl* recordDecl) {
        if (!sourceManager.isInMainFile(recordDecl->getLocStart()))
            return true;
        bool isTemplated = recordDecl->getDescribedClassTemplate();
        TemplateSpecializationKind specKind = recordDecl->getTemplateSpecializationKind();
        dbg() << __FUNCTION__ << specKind << " " << std::endl;
        if (isTemplated && (specKind == TSK_ImplicitInstantiation || specKind == TSK_Undeclared))
            return true;
        CXXRecordDecl* canonicalDecl = recordDecl->getCanonicalDecl();
        const bool classIsUnused = used.find(canonicalDecl) == used.end();
        const bool thisIsRedeclaration = !recordDecl->isCompleteDefinition() && declared.find(canonicalDecl) != declared.end();

        if (classIsUnused || thisIsRedeclaration) {
            removeDecl(recordDecl);
        }
        declared.insert(canonicalDecl);
        return true;
    }

    bool VisitClassTemplateDecl(ClassTemplateDecl* templateDecl) {
        if (!sourceManager.isInMainFile(templateDecl->getLocStart()))
            return true;
        dbg() << __FUNCTION__ << std::endl;
        ClassTemplateDecl* canonicalDecl = templateDecl->getCanonicalDecl();
        const bool classIsUnused = used.find(canonicalDecl) == used.end();
        const bool thisIsRedeclaration = !templateDecl->isThisDeclarationADefinition() && declared.find(canonicalDecl) != declared.end();

        if (classIsUnused || thisIsRedeclaration) {
            removeDecl(templateDecl);
        }
        declared.insert(canonicalDecl);
    }

    bool VisitUsingDirectiveDecl(UsingDirectiveDecl* usingDecl) {
        if (!sourceManager.isInMainFile(usingDecl->getLocStart()))
            return true;
        NamespaceDecl* ns = usingDecl->getNominatedNamespace();
        if (ns && !usedNamespaces.insert(ns).second)
            removeDecl(usingDecl);
        return true;
    }

    // TODO remove member fields/methods in classes that are not used as template parameters of STL structures

private:
    void removeDecl(Decl* decl) {
        if (!decl)
            return;
        SourceLocation start = decl->getLocStart();
        SourceLocation end = decl->getLocEnd();
        SourceLocation semicolonAfterDefinition = findSemiAfterLocation(end, decl->getASTContext());
        dbg() << "REMOVE: " << toString(start) << " " << toString(end)
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
    
    virtual void HandleTranslationUnit(ASTContext& Ctx) {
        //cerr << "Build dependency graph" << std::endl;
        DependenciesCollector depsVisitor(sourceManager, uses);
        depsVisitor.TraverseDecl(Ctx.getTranslationUnitDecl());

        FunctionDecl* mainFunction = depsVisitor.getMainFunction();
        if (!mainFunction) {
            cerr << "Error: no main function in the file!\n";
            return;
        }
        
        //cerr << "Search for used decls" << std::endl;
        std::set<Decl*> used;
        std::set<Decl*> queue;
        queue.insert(mainFunction->getCanonicalDecl());
        while (!queue.empty()) {
            Decl* decl = *queue.begin();
            queue.erase(queue.begin());
            if (used.insert(decl).second) {
                queue.insert(uses[decl].begin(), uses[decl].end());
            }
        }

        //cerr << "Remove unused decls" << std::endl;
        OptimizerVisitor visitor(sourceManager, used, rewriter);
        visitor.TraverseDecl(Ctx.getTranslationUnitDecl());
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
                return "Inliner error"; // something's wrong
        }
    }

private:
    SourceManager& sourceManager;
    Rewriter& rewriter;
    References uses;
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
