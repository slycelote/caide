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


#include "optimizer.h"
#include "SmartRewriter.h"
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

#define CAIDE_FUNC ""
//#define CAIDE_FUNC __FUNCTION__ << endl

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

    bool isUserFile(SourceLocation loc) const {
        return !sourceManager.isInSystemHeader(loc) && loc.isValid();
    }

    void insertReference(Decl* from, Decl* to) {
        if (!from || !to)
            return;
        from = from->getCanonicalDecl();
        to = to->getCanonicalDecl();
        uses[from].insert(to);
        dbg() << "Reference from " << from->getDeclKindName() << " " << from << "<"
                  << toString(from).substr(0, 20)
                  << ">" << toString(from->getSourceRange())
                  << " to " << to->getDeclKindName() << " " << to << "<"
                  << toString(to).substr(0, 20)
                  << ">" << toString(to->getSourceRange()) << "\n";
    }

public:
    FunctionDecl* getMainFunction() const {
        return mainFunctionDecl;
    }

    DependenciesCollector(SourceManager& srcMgr, References& _uses)
        : sourceManager(srcMgr)
        , uses(_uses)
        , currentDecl(0)
        , mainFunctionDecl(0)
    {}
    bool shouldVisitImplicitCode() const { return true; }
    bool shouldVisitTemplateInstantiations() const { return true; }

    //bool VisitDecl(Decl* decl) { dbg() << decl->getDeclKindName() << " " << decl << endl; return true; }

    bool VisitCallExpr(CallExpr* callExpr) {
        dbg() << CAIDE_FUNC;
        Expr* callee = callExpr->getCallee();
        Decl* calleeDecl = callExpr->getCalleeDecl();
        if (!callee || !calleeDecl || isa<UnresolvedMemberExpr>(callee) || isa<CXXDependentScopeMemberExpr>(callee))
            return true;
        if (isUserFile(calleeDecl->getCanonicalDecl()->getSourceRange().getBegin()))
            insertReference(currentDecl, calleeDecl);
        return true;
    }

    bool VisitCXXConstructExpr(CXXConstructExpr* constructorExpr) {
        dbg() << CAIDE_FUNC;
        insertReference(currentDecl, constructorExpr->getConstructor());
        return true;
    }

    bool VisitDeclRefExpr(DeclRefExpr* ref) {
        dbg() << CAIDE_FUNC;
        insertReference(currentDecl, ref->getDecl());
        return true;
    }

    bool VisitValueDecl(ValueDecl* valueDecl) {
        dbg() << CAIDE_FUNC;
        const Type* valueType = valueDecl->getType().getTypePtrOrNull();
        if (valueType)
            insertReference(valueDecl, valueType->getAsCXXRecordDecl());
        return true;
    }

    bool VisitMemberExpr(MemberExpr* memberExpr) {
        dbg() << CAIDE_FUNC;
        insertReference(currentDecl, memberExpr->getMemberDecl());
        return true;
    }

    bool VisitFieldDecl(FieldDecl* field) {
        dbg() << CAIDE_FUNC;
        insertReference(field, field->getParent());
        return true;
    }

    bool VisitClassTemplateDecl(ClassTemplateDecl* templateDecl) {
        dbg() << CAIDE_FUNC;
        insertReference(templateDecl, templateDecl->getTemplatedDecl());
        return true;
    }

    bool VisitClassTemplateSpecializationDecl(ClassTemplateSpecializationDecl* specDecl) {
        dbg() << CAIDE_FUNC;
        llvm::PointerUnion<ClassTemplateDecl*, ClassTemplatePartialSpecializationDecl*>
            instantiatedFrom = specDecl->getSpecializedTemplateOrPartial();

        if (instantiatedFrom.is<ClassTemplateDecl*>())
            insertReference(specDecl, instantiatedFrom.get<ClassTemplateDecl*>());
        else if (instantiatedFrom.is<ClassTemplatePartialSpecializationDecl*>())
            insertReference(specDecl, instantiatedFrom.get<ClassTemplatePartialSpecializationDecl*>());

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
        //dbg() << CAIDE_FUNC;
        FunctionTemplateSpecializationInfo* specInfo = f->getTemplateSpecializationInfo();
        if (specInfo)
            insertReference(f, specInfo->getTemplate());
        if (FunctionDecl* instantiatedFrom = f->getInstantiatedFromMemberFunction()) {
            insertReference(f, instantiatedFrom);
        }
        if (f->hasBody()) {
            currentDecl = f;

            DeclarationName DeclName = f->getNameInfo().getName();
            string FuncName = DeclName.getAsString();

            if (sourceManager.isInMainFile(f->getLocStart()))
                dbg() << "Moving to " << FuncName << " at " << toString(f->getLocation()) << std::endl;
        }
        return true;
    }

    bool VisitCXXMethodDecl(CXXMethodDecl* method) {
        dbg() << CAIDE_FUNC;
        insertReference(method, method->getParent());
        return true;
    }

    bool VisitCXXRecordDecl(CXXRecordDecl* recordDecl) {
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
    SmartRewriter rewriter;

    std::string toString(const Decl* decl) const {
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

public:
    OptimizerVisitor(SourceManager& srcManager, const std::set<Decl*>& _used, Rewriter& _rewriter)
        : sourceManager(srcManager)
        , used(_used)
        , rewriter(_rewriter)
    {}

    /*
     Here's how template functions and classes are represented in the AST.
-FunctionTemplateDecl <-- the template
 |-TemplateTypeParmDecl
 |-FunctionDecl  <-- for each instantiation of the template
 | `-CompoundStmt
 |   `-...
-FunctionDecl   <-- non-template or full specialization of a template



|-ClassTemplateDecl <-- root template
| |-TemplateTypeParmDecl
| |-CXXRecordDecl  <-- non-specialized root template class
| | |-CXXRecordDecl
| | `-CXXMethodDecl...
| |-ClassTemplateSpecialization <-- non-instantiated explicit specialization (?)
| `-ClassTemplateSpecializationDecl <-- implicit instantiation of root template
|   |-TemplateArgument type 'double'
|   |-CXXRecordDecl
|   |-CXXMethodDecl...
|-ClassTemplatePartialSpecializationDecl <-- partial specialization
| |-TemplateArgument
| |-TemplateTypeParmDecl
| |-CXXRecordDecl
| `-CXXMethodDecl...
|-ClassTemplateSpecializationDecl <-- instantiation of explicit specialization
| |-TemplateArgument type 'int'
| |-CXXRecordDecl
| `-CXXMethodDecl...


     */
    // TODO: dependencies on types of template parameters
    bool VisitFunctionDecl(FunctionDecl* functionDecl) {
        if (!sourceManager.isInMainFile(functionDecl->getLocStart()))
            return true;
        if (functionDecl->getDescribedFunctionTemplate() != 0) {
            // An instantiation of a template function; will be processed as FunctionTemplateDecl
            return true;
        }
        if (functionDecl->getInstantiatedFromMemberFunction() != 0) {
            // A method in an instantiated class template; will be processed as a method in ClassTemplateDecl
            return true;
        }
        FunctionDecl* canonicalDecl = functionDecl->getCanonicalDecl();
        const bool funcIsUnused = used.find(canonicalDecl) == used.end();
        const bool thisIsRedeclaration = !functionDecl->doesThisDeclarationHaveABody() && declared.find(canonicalDecl) != declared.end();
        if (funcIsUnused || thisIsRedeclaration) {
            dbg() << CAIDE_FUNC;
            removeDecl(functionDecl);
        }
        declared.insert(canonicalDecl);
        return true;
    }

    bool VisitFunctionTemplateDecl(FunctionTemplateDecl* functionDecl) {
        if (!sourceManager.isInMainFile(functionDecl->getLocStart()))
            return true;
        dbg() << CAIDE_FUNC;
        if (used.find(functionDecl) == used.end())
            removeDecl(functionDecl);
        return true;
    }

    bool VisitCXXRecordDecl(CXXRecordDecl* recordDecl) {
        if (!sourceManager.isInMainFile(recordDecl->getLocStart()))
            return true;
        bool isTemplated = recordDecl->getDescribedClassTemplate() != 0;
        TemplateSpecializationKind specKind = recordDecl->getTemplateSpecializationKind();
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

    // TODO: dependencies on types of template parameters
    bool VisitClassTemplateDecl(ClassTemplateDecl* templateDecl) {
        if (!sourceManager.isInMainFile(templateDecl->getLocStart()))
            return true;
        dbg() << CAIDE_FUNC;
        ClassTemplateDecl* canonicalDecl = templateDecl->getCanonicalDecl();
        const bool classIsUnused = used.find(canonicalDecl) == used.end();
        const bool thisIsRedeclaration = !templateDecl->isThisDeclarationADefinition() && declared.find(canonicalDecl) != declared.end();

        if (classIsUnused || thisIsRedeclaration) {
            removeDecl(templateDecl);
        }
        declared.insert(canonicalDecl);
        return true;
    }

    bool VisitUsingDirectiveDecl(UsingDirectiveDecl* usingDecl) {
        if (!sourceManager.isInMainFile(usingDecl->getLocStart()))
            return true;
        NamespaceDecl* ns = usingDecl->getNominatedNamespace();
        if (ns && !usedNamespaces.insert(ns).second)
            removeDecl(usingDecl);
        return true;
    }

private:
    void removeDecl(Decl* decl) {
        if (!decl)
            return;
        SourceLocation start = decl->getLocStart();
        SourceLocation end = decl->getLocEnd();
        SourceLocation semicolonAfterDefinition = findSemiAfterLocation(end, decl->getASTContext());
        dbg() << "REMOVE " << decl->getDeclKindName() << " " << decl << ": " << toString(start) << " " << toString(end)
              << " " << toString(semicolonAfterDefinition) << std::endl;
        if (semicolonAfterDefinition.isValid())
            end = semicolonAfterDefinition;
        Rewriter::RewriteOptions opts;
        opts.RemoveLineIfEmpty = true;
        rewriter.removeRange(SourceRange(start, end), opts);
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
    explicit OptimizerConsumer(SourceManager& srcMgr, Rewriter& _rewriter, std::string& _result)
        : sourceManager(srcMgr)
        , rewriter(_rewriter)
        , result(_result)
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

        result = getResult();
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
        return std::string(b, std::min(b+30, e));
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
    std::string& result;
};


class OptimizerFrontendAction : public ASTFrontendAction {
private:
    Rewriter& rewriter;
    string& result;
public:
    OptimizerFrontendAction(Rewriter& _rewriter, string& _result)
        : rewriter(_rewriter)
        , result(_result)
    {}

    virtual std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance& compiler, StringRef /*file*/)
    {
        if (!compiler.hasSourceManager()) {
            throw "No source manager";
        }
        rewriter.setSourceMgr(compiler.getSourceManager(), compiler.getLangOpts());
        return std::unique_ptr<OptimizerConsumer>(new OptimizerConsumer(compiler.getSourceManager(), rewriter, result));
    }
};

class OptimizerFrontendActionFactory: public tooling::FrontendActionFactory {
private:
    Rewriter& rewriter;
    string& result;
public:
    OptimizerFrontendActionFactory(Rewriter& _rewriter, string& _result)
        : rewriter(_rewriter)
        , result(_result)
    {}
    FrontendAction* create() {
        return new OptimizerFrontendAction(rewriter, result);
    }
};

Optimizer::Optimizer(const std::vector<std::string>& _cmdLineOptions):
    cmdLineOptions(_cmdLineOptions)
{}

std::string Optimizer::doOptimize(const std::string& cppFile) {
    std::auto_ptr<tooling::FixedCompilationDatabase> compilationDatabase(
        createCompilationDatabaseFromCommandLine(cmdLineOptions));

    vector<string> sources;
    sources.push_back(cppFile);

    clang::tooling::ClangTool tool(*compilationDatabase, sources);

    Rewriter rewriter;
    string result;
    OptimizerFrontendActionFactory factory(rewriter, result);

    int ret = tool.run(&factory);
    if (ret != 0)
        throw std::runtime_error("Compilation error");

    return result;
}

