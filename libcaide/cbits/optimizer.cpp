#include <cstdio>
#include <iostream>
#include <stack>
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
#include "clang/Sema/Sema.h"

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

    vector<FunctionDecl*>& delayedParsedFunctions;

    FunctionDecl* mainFunctionDecl;

    /*
     'Declaration context' for Stmt's. Updated each time a Decl is visited.
     This works because a Stmt cannot be defined immediately after a Decl is exited.
     For Decl's, however, this won't work and we use getLexicalDeclContext() instead.
    */
    Decl* currentDecl;

private:
    FunctionDecl* getCurrentFunction(Decl* decl) const {
        DeclContext* declCtx = decl->getLexicalDeclContext();
        return declCtx ? dyn_cast<FunctionDecl>(declCtx) : 0;
    }

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

    DependenciesCollector(SourceManager& srcMgr, References& _uses, vector<FunctionDecl*>& delayedParsedFunctions_)
        : sourceManager(srcMgr)
        , uses(_uses)
        , delayedParsedFunctions(delayedParsedFunctions_)
        , mainFunctionDecl(0)
        , currentDecl(0)
    {}

    bool shouldVisitImplicitCode() const { return true; }
    bool shouldVisitTemplateInstantiations() const { return true; }

    bool VisitDecl(Decl* decl) {
        // Mark any function as depending on its content.
        // TODO: detect unused local variables.
        insertReference(getCurrentFunction(decl), decl);
        currentDecl = decl;
        return true;
    }

    bool VisitCallExpr(CallExpr* callExpr) {
        dbg() << CAIDE_FUNC;
        Expr* callee = callExpr->getCallee();
        Decl* calleeDecl = callExpr->getCalleeDecl();

        if (!callee || !calleeDecl || isa<UnresolvedMemberExpr>(callee) || isa<CXXDependentScopeMemberExpr>(callee))
            return true;

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
        if (!valueType)
            return true;

        insertReference(valueDecl, valueType->getAsTagDecl());

        if (const TypedefType* typedefType = dyn_cast<TypedefType>(valueType))
            insertReference(valueDecl, typedefType->getDecl());

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

    bool VisitTypedefDecl(TypedefDecl* typedefDecl) {
        dbg() << CAIDE_FUNC;
        const Type* underlyingType = typedefDecl->getUnderlyingType().getTypePtrOrNull();
        if (underlyingType) {
            insertReference(typedefDecl, underlyingType->getAsCXXRecordDecl());
            if (const TypedefType* typedefType = dyn_cast<TypedefType>(underlyingType))
                insertReference(typedefDecl, typedefType->getDecl());
        }
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
        if (f->isMain())
            mainFunctionDecl = f;

        if (sourceManager.isInMainFile(f->getLocStart()) && f->isLateTemplateParsed())
            delayedParsedFunctions.push_back(f);

        if (f->getTemplatedKind() == FunctionDecl::TK_FunctionTemplate) {
            // skip non-instantiated template function
            return true;
        }

        FunctionTemplateSpecializationInfo* specInfo = f->getTemplateSpecializationInfo();
        if (specInfo)
            insertReference(f, specInfo->getTemplate());

        insertReference(f, f->getInstantiatedFromMemberFunction());

        if (f->hasBody()) {
            DeclarationName DeclName = f->getNameInfo().getName();
            string FuncName = DeclName.getAsString();

            if (sourceManager.isInMainFile(f->getLocStart()))
                dbg() << "Moving to " << FuncName << " at " << toString(f->getLocation()) << std::endl;
        }

        return true;
    }

    bool VisitFunctionTemplateDecl(FunctionTemplateDecl* functionTemplate) {
        insertReference(functionTemplate,
                functionTemplate->getInstantiatedFromMemberTemplate());
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


struct IfDefClause {
    // Locations of #if, #ifdef, #ifndef, #else, #elif tokens of this clause
    vector<SourceLocation> locations;

    // Index of selected branch in locations list; -1 if no branch was selected
    int selectedBranch;

    explicit IfDefClause(const SourceLocation& ifLocation)
        : selectedBranch(-1)
    {
        locations.push_back(ifLocation);
    }
};

class RemoveInactivePreprocessorBlocks: public PPCallbacks {
private:
    SourceManager& sourceManager;
    SmartRewriter& rewriter;
    vector<IfDefClause> activeClauses;
    set<string> definedMacros;

    // maps MacroDirective corresponding to a macro definition to a set of its usages
    typedef map<const MacroDirective*, vector<SourceRange> > MacroUsages;
    MacroUsages usages;

private:
    bool tryRemoveMacroDefinition(MacroUsages::const_iterator it) {
        if (it == usages.end())
            return false;

        bool isUsed = false;
        for (const SourceRange& usageRange : it->second) {
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
    RemoveInactivePreprocessorBlocks(SourceManager& _sourceManager, SmartRewriter& _rewriter)
        : sourceManager(_sourceManager)
        , rewriter(_rewriter)
    {
    }

    void MacroDefined(const Token& MacroNameTok, const MacroDirective* MD) {
        definedMacros.insert(getTokenName(MacroNameTok));
        if (MD && isInMainFile(MD->getLocation()))
            usages[MD].clear();
    }

    void MacroUndefined(const Token& MacroNameTok, const MacroDirective* MD) {
        definedMacros.erase(getTokenName(MacroNameTok));

        if (!MD || !isInMainFile(MD->getLocation()))
            return;

        MacroUsages::const_iterator it = usages.find(MD);
        if (tryRemoveMacroDefinition(it)) {
            // Removed the #define, so remove this #undef too
            SourceLocation undefLoc = MacroNameTok.getLocation();
            removeLine(undefLoc);
        }

        if (it != usages.end()) {
            usages.erase(it);
        }
    }

    void MacroExpands(const Token& /*MacroNameTok*/, const MacroDirective* MD,
                      SourceRange Range, const MacroArgs* /*Args*/)
    {
        if (!MD || !isInMainFile(MD->getLocation()))
            return;

        usages[MD].push_back(Range);
    }

    // EndOfMainFile() is called too late; instead, we call this one manually in the consumer.
    void Finalize() {
        // Remove unused #defines that don't have a corresponding #undef
        for (auto it = usages.begin(); it != usages.end(); ++it)
            tryRemoveMacroDefinition(it);
    }

    void If(SourceLocation Loc, SourceRange /*ConditionRange*/, ConditionValueKind ConditionValue) {
        if (!isInMainFile(Loc))
            return;
        activeClauses.push_back(IfDefClause(Loc));
        if (ConditionValue == CVK_True)
            activeClauses.back().selectedBranch = 0;
    }

    void Ifdef(SourceLocation Loc, const Token& MacroNameTok, const MacroDirective* /*MD*/) {
        if (!isInMainFile(Loc))
            return;
        activeClauses.push_back(IfDefClause(Loc));
        if (definedMacros.find(getTokenName(MacroNameTok)) != definedMacros.end()) {
            activeClauses.back().selectedBranch = 0;
        }
    }

    void Ifndef(SourceLocation Loc, const Token& MacroNameTok, const MacroDirective* /*MD*/) {
        if (!isInMainFile(Loc))
            return;
        activeClauses.push_back(IfDefClause(Loc));
        if (definedMacros.find(getTokenName(MacroNameTok)) == definedMacros.end()) {
            activeClauses.back().selectedBranch = 0;
        }
    }

    void Elif(SourceLocation Loc, SourceRange /*ConditionRange*/, ConditionValueKind ConditionValue, SourceLocation /*IfLoc*/ ) {
        if (!isInMainFile(Loc))
            return;
        if (ConditionValue == CVK_True)
            activeClauses.back().selectedBranch = activeClauses.back().locations.size();
        activeClauses.back().locations.push_back(Loc);
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

        if (clause.selectedBranch < 0) {
            SourceLocation b = changeColumn(clause.locations.front(), 1);
            SourceLocation e = changeColumn(clause.locations.back(), 10000);
            rewriter.removeRange(SourceRange(b, e), opts);
        } else {
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
};

class OptimizerVisitor: public RecursiveASTVisitor<OptimizerVisitor> {
private:
    SourceManager& sourceManager;
    const std::set<Decl*>& used;
    std::set<Decl*> declared;
    std::set<NamespaceDecl*> usedNamespaces;
    SmartRewriter& rewriter;

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
    OptimizerVisitor(SourceManager& srcManager, const std::set<Decl*>& _used, SmartRewriter& _rewriter)
        : sourceManager(srcManager)
        , used(_used)
        , rewriter(_rewriter)
    {}

    bool VisitEmptyDecl(EmptyDecl* decl) {
        removeDecl(decl);
        return true;
    }

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

        if (CXXDestructorDecl* destructor = dyn_cast<CXXDestructorDecl>(functionDecl)) {
            // Destructor should be removed iff the class is unused
            CXXRecordDecl* classDecl = destructor->getParent();
            if (classDecl && used.find(classDecl->getCanonicalDecl()) == used.end()) {
                removeDecl(destructor);
            }
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

    bool VisitTypedefDecl(TypedefDecl* typedefDecl) {
        if (!sourceManager.isInMainFile(typedefDecl->getLocStart()))
            return true;

        Decl* canonicalDecl = typedefDecl->getCanonicalDecl();
        if (used.find(canonicalDecl) == used.end()) {
            removeDecl(typedefDecl);
        }

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
    explicit OptimizerConsumer(CompilerInstance& compiler_, SmartRewriter& _rewriter,
               RemoveInactivePreprocessorBlocks& ppCallbacks_, std::string& _result)
        : compiler(compiler_)
        , sourceManager(compiler.getSourceManager())
        , rewriter(_rewriter)
        , ppCallbacks(ppCallbacks_)
        , result(_result)
    {}

    virtual void HandleTranslationUnit(ASTContext& Ctx) {
        ppCallbacks.Finalize();

        //cerr << "Build dependency graph" << std::endl;
        vector<FunctionDecl*> delayedParsedFunctions;
        DependenciesCollector depsVisitor(sourceManager, uses, delayedParsedFunctions);
        depsVisitor.TraverseDecl(Ctx.getTranslationUnitDecl());

        // Source range of delayed-parsed template functions includes only declaration part.
        //     Force their parsing to get correct source ranges.
        // Note that this essentially disables -fdelayed-template-parsing flag for user code
        //     and may lead to incompatible results when, e.g., code compiles in VS
        //     but not in the inliner. TODO: find another way to get correct source ranges.
        for (auto it = delayedParsedFunctions.begin(); it != delayedParsedFunctions.end(); ++it) {
            clang::Sema& sema = compiler.getSema();
            clang::LateParsedTemplate* lpt = sema.LateParsedTemplateMap[*it];
            sema.LateTemplateParser(sema.OpaqueParser, *lpt);
        }

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

        rewriter.applyChanges();

        //cerr << "Done!" << std::endl;
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
    CompilerInstance& compiler;
    SourceManager& sourceManager;
    SmartRewriter& rewriter;
    References uses;
    RemoveInactivePreprocessorBlocks& ppCallbacks;
    std::string& result;
};


class OptimizerFrontendAction : public ASTFrontendAction {
private:
    Rewriter& rewriter;
    SmartRewriter& smartRewriter;
    string& result;
public:
    OptimizerFrontendAction(Rewriter& _rewriter, SmartRewriter& _smartRewriter, string& _result)
        : rewriter(_rewriter)
        , smartRewriter(_smartRewriter)
        , result(_result)
    {}

    virtual std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance& compiler, StringRef /*file*/)
    {
        if (!compiler.hasSourceManager()) {
            throw "No source manager";
        }
        rewriter.setSourceMgr(compiler.getSourceManager(), compiler.getLangOpts());
        auto ppCallbacks = std::unique_ptr<RemoveInactivePreprocessorBlocks>(
            new RemoveInactivePreprocessorBlocks(compiler.getSourceManager(), smartRewriter));
        auto consumer = std::unique_ptr<OptimizerConsumer>(new OptimizerConsumer(compiler, smartRewriter, *ppCallbacks, result));
        compiler.getPreprocessor().addPPCallbacks(std::move(ppCallbacks));
        return std::move(consumer);
    }
};

class OptimizerFrontendActionFactory: public tooling::FrontendActionFactory {
private:
    Rewriter& rewriter;
    SmartRewriter smartRewriter;
    string& result;
public:
    OptimizerFrontendActionFactory(Rewriter& _rewriter, string& _result)
        : rewriter(_rewriter)
        , smartRewriter(_rewriter)
        , result(_result)
    {}
    FrontendAction* create() {
        return new OptimizerFrontendAction(rewriter, smartRewriter, result);
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

