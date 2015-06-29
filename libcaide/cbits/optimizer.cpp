#include <cstdio>
#include <iostream>
#include <stack>
#include <stdexcept>
#include <string>
#include <sstream>

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Comment.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/Utils.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Parse/ParseAST.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Sema/Sema.h"

#include "clang/Tooling/Tooling.h"


#include "optimizer.h"
#include "RemoveInactivePreprocessorBlocks.h"
#include "SmartRewriter.h"
#include "StmtParentMap.h"
#include "util.h"

using namespace clang;
using namespace std;

//#define CAIDE_DEBUG_MODE

#ifdef CAIDE_DEBUG_MODE

#define dbg(vals) std::cerr << vals
#define CAIDE_FUNC __FUNCTION__ << endl

#else

#define dbg(vals)
#define CAIDE_FUNC ""

#endif

typedef std::map<Decl*, std::set<Decl*> > References;

// Contains information that DependenciesCollector passes to the next stage
struct SourceInfo {
    // key: Decl, value: what the key uses.
    References uses;

    // 'Roots of the dependency tree':
    // - int main(), or a special function (for Topcoder)
    // - static variables with possible side effects
    // - declarations marked with a comment /// caide keep
    set<Decl*> declsToKeep;

    // Delayed parsed functions.
    vector<FunctionDecl*> delayedParsedFunctions;

    // Declarations of static variables, grouped by their start location
    // (so comma separated declarations go into the same group).
    map<SourceLocation, vector<VarDecl*> > staticVariables;
};

class DependenciesCollector : public RecursiveASTVisitor<DependenciesCollector> {
private:
    SourceManager& sourceManager;
    SourceInfo& srcInfo;

    // There is no stmt->getParentDecl() method, so we keep track of it manually.
    // We maintain two items:
    // 1. Last visited Decl.
    // 2. A stack of FunctionDecl's together with Stmt trees of their bodies.
    // The closest Decl ancestor of a Stmt is either the last visited decl, or
    // one of the functions on the stack.
    //
    // Because there is no callback when a Decl is exited, top of the stack
    // may contain some extra functions. We account for it in getParentDecl(stmt)
    // and VisitFunctionDecl, where we rewind the stack.
    //
    Decl* lastVisitedDecl;
    std::vector<std::pair<FunctionDecl*, std::unique_ptr<StmtParentMap> > > functionLexicalStack;

    // Works only when the stmt is currently visited.
    Decl* getParentDecl(Stmt* stmt) {
        // There are two possibilities: top of the stack either contains the stmt or not.
        int stackSize = (int)functionLexicalStack.size();
        int i = stackSize - 1;
        while (i >= 0 && !functionLexicalStack[i].second->contains(stmt))
            --i;

        if (i < 0) {
            // We are inside a top level declaration.
            return lastVisitedDecl;
        }

        // Rewind the stack, if necessary.
        for (; stackSize > i + 1; --stackSize)
            functionLexicalStack.pop_back();

        // Check whether there are any intermediate decls between
        // containing function and the stmt.
        // If there are, lastVisitedDecl must be the last of them;
        // otherwise we return the function.
        // To check for intermediate decls, walk up the parent pointers and search
        // for a DeclStmt (which is an adapter class for Decl's)
        auto& parentMap = *functionLexicalStack.back().second;
        do {
            stmt = parentMap.getParent(stmt);
        } while (stmt && !isa<DeclStmt>(stmt));
        if (stmt) {
            // found a DeclStmt
            return lastVisitedDecl;
        } else {
            return functionLexicalStack.back().first;
        }
    }

private:
    FunctionDecl* getCurrentFunction(Decl* decl) const {
        DeclContext* declCtx = decl->getLexicalDeclContext();
        return dyn_cast_or_null<FunctionDecl>(declCtx);
    }

    std::string toString(SourceLocation loc) const {
        //return loc.printToString(sourceManager);
        std::string fileName = sourceManager.getFilename(loc).str();
        if (fileName.length() > 30) {
            fileName = fileName.substr(fileName.length() - 30);
        }
        std::ostringstream os;
        os << fileName << ":" <<
            sourceManager.getExpansionLineNumber(loc) << ":" <<
            sourceManager.getExpansionColumnNumber(loc);
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
        srcInfo.uses[from].insert(to);
        dbg("Reference   FROM    " << from->getDeclKindName() << " " << from << "<"
            << toString(from).substr(0, 30)
            << ">" << toString(from->getSourceRange())
            << "     TO     " << to->getDeclKindName() << " " << to << "<"
            << toString(to).substr(0, 30)
            << ">" << toString(to->getSourceRange()) << std::endl);
    }

    void insertReferenceToType(Decl* from, const Type* to,
            std::set<const Type*>& seen)
    {
        if (!to)
            return;

        if (!seen.insert(to).second)
            return;

        if (const ElaboratedType* elaboratedType = dyn_cast<ElaboratedType>(to)) {
            insertReferenceToType(from, elaboratedType->getNamedType(), seen);
            return;
        }

        if (const ParenType* parenType = dyn_cast<ParenType>(to))
            insertReferenceToType(from, parenType->getInnerType(), seen);

        insertReference(from, to->getAsTagDecl());

        if (const ArrayType* arrayType = dyn_cast<ArrayType>(to))
            insertReferenceToType(from, arrayType->getElementType(), seen);

        if (const PointerType* pointerType = dyn_cast<PointerType>(to))
            insertReferenceToType(from, pointerType->getPointeeType(), seen);

        if (const ReferenceType* refType = dyn_cast<ReferenceType>(to))
            insertReferenceToType(from, refType->getPointeeType(), seen);

        if (const TypedefType* typedefType = dyn_cast<TypedefType>(to))
            insertReference(from, typedefType->getDecl());

        if (const CXXRecordDecl* recordDecl = to->getAsCXXRecordDecl()) {
            if ((recordDecl = recordDecl->getDefinition())) {
                bool isTemplated = recordDecl->getDescribedClassTemplate() != 0;
                TemplateSpecializationKind specKind = recordDecl->getTemplateSpecializationKind();
                if (isTemplated && (specKind == TSK_ImplicitInstantiation || specKind == TSK_Undeclared)) {}
                else {
                    for (const CXXBaseSpecifier* base = recordDecl->bases_begin();
                         base != recordDecl->bases_end(); ++base)
                    {
                        insertReferenceToType(from, base->getType(), seen);
                    }
                }
            }
        }

        if (const TemplateSpecializationType* tempSpecType =
                dyn_cast<TemplateSpecializationType>(to))
        {
            for (unsigned i = 0; i < tempSpecType->getNumArgs(); ++i) {
                const TemplateArgument& arg = tempSpecType->getArg(i);
                if (arg.getKind() == TemplateArgument::Type)
                    insertReferenceToType(from, arg.getAsType(), seen);
            }
        }
    }

    void insertReferenceToType(Decl* from, QualType to,
            std::set<const Type*>& seen)
    {
        insertReferenceToType(from, to.getTypePtrOrNull(), seen);
    }

    void insertReferenceToType(Decl* from, QualType to)
    {
        std::set<const Type*> seen;
        insertReferenceToType(from, to, seen);
    }

    void insertReferenceToType(Decl* from, const Type* to)
    {
        std::set<const Type*> seen;
        insertReferenceToType(from, to, seen);
    }

public:
    DependenciesCollector(SourceManager& srcMgr, SourceInfo& srcInfo_)
        : sourceManager(srcMgr)
        , srcInfo(srcInfo_)
        , lastVisitedDecl(nullptr)
    {
    }

    bool shouldVisitImplicitCode() const { return true; }
    bool shouldVisitTemplateInstantiations() const { return true; }

    bool VisitDecl(Decl* decl) {
        // Mark dependence on enclosing class/namespace.
        Decl* ctx = dyn_cast_or_null<Decl>(decl->getDeclContext());
        if (ctx && !isa<FunctionDecl>(ctx))
            insertReference(decl, ctx);

        lastVisitedDecl = decl;

        if (comments::FullComment* comment =
            decl->getASTContext().getLocalCommentForDeclUncached(decl))
        {
            bool invalid = false;
            const char* beg = sourceManager.getCharacterData(comment->getLocStart(), &invalid);
            if (beg && !invalid) {
                const char* end =
                    sourceManager.getCharacterData(comment->getLocEnd(), &invalid);
                if (end && !invalid) {
                    static const string caideKeepComment = "caide keep";
                    StringRef haystack(beg, end - beg + 1);
                    StringRef needle(caideKeepComment);
                    if (haystack.find(needle) != StringRef::npos)
                        srcInfo.declsToKeep.insert(decl);
                }
            }
        }

        return true;
    }

    bool VisitCallExpr(CallExpr* callExpr) {
        dbg(CAIDE_FUNC);
        Expr* callee = callExpr->getCallee();
        Decl* calleeDecl = callExpr->getCalleeDecl();

        if (!callee || !calleeDecl || isa<UnresolvedMemberExpr>(callee) || isa<CXXDependentScopeMemberExpr>(callee))
            return true;

        insertReference(getParentDecl(callExpr), calleeDecl);
        return true;
    }

    bool VisitCXXConstructExpr(CXXConstructExpr* constructorExpr) {
        dbg(CAIDE_FUNC);
        insertReference(getParentDecl(constructorExpr), constructorExpr->getConstructor());
        return true;
    }

    bool VisitCXXTemporaryObjectExpr(CXXTemporaryObjectExpr* tempExpr) {
        if (TypeSourceInfo* tsi = tempExpr->getTypeSourceInfo())
            insertReferenceToType(getParentDecl(tempExpr), tsi->getType());
        return true;
    }

    bool VisitCXXNewExpr(CXXNewExpr* newExpr) {
        insertReferenceToType(getParentDecl(newExpr), newExpr->getAllocatedType());
        return true;
    }

    bool VisitDeclRefExpr(DeclRefExpr* ref) {
        dbg(CAIDE_FUNC);
        Decl* parent = getParentDecl(ref);
        insertReference(parent, ref->getDecl());
        NestedNameSpecifier* specifier = ref->getQualifier();
        while (specifier) {
            insertReferenceToType(parent, specifier->getAsType());
            specifier = specifier->getPrefix();
        }
        return true;
    }

    bool VisitCXXScalarValueInitExpr(CXXScalarValueInitExpr* initExpr) {
        if (TypeSourceInfo* tsi = initExpr->getTypeSourceInfo())
            insertReferenceToType(getParentDecl(initExpr), tsi->getType());
        return true;
    }

    bool VisitExplicitCastExpr(ExplicitCastExpr* castExpr) {
        insertReferenceToType(getParentDecl(castExpr), castExpr->getTypeAsWritten());
        return true;
    }

    bool VisitValueDecl(ValueDecl* valueDecl) {
        dbg(CAIDE_FUNC);
        // Mark any function as depending on its local variables.
        // TODO: detect unused local variables.
        insertReference(getCurrentFunction(valueDecl), valueDecl);

        insertReferenceToType(valueDecl, valueDecl->getType());
        return true;
    }

    bool VisitVarDecl(VarDecl* varDecl) {
        SourceLocation start = varDecl->getLocStart();
        if (start.isMacroID())
            start = sourceManager.getExpansionRange(start).first;
        if (!varDecl->isLocalVarDeclOrParm() && sourceManager.isInMainFile(start)) {
            srcInfo.staticVariables[start].push_back(varDecl);
            /*
            Technically, we cannot remove global static variables because
            their initializers may have side effects.
            The following code marks too many expressions as having side effects
            (e.g. it will mark an std::vector constructor as such):

            VarDecl* definition = varDecl->getDefinition();
            Expr* initExpr = definition ? definition->getInit() : nullptr;
            if (initExpr && initExpr->HasSideEffects(varDecl->getASTContext()))
                srcInfo.declsToKeep.insert(varDecl);

            Because RAII idiom is not common in competitive programming, we simply
            remove unreferenced global static variables unless they are marked with
            a '/// caide keep' comment.
            */
        }
        return true;
    }

    bool VisitMemberExpr(MemberExpr* memberExpr) {
        dbg(CAIDE_FUNC);
        insertReference(getParentDecl(memberExpr), memberExpr->getMemberDecl());
        return true;
    }

    bool VisitLambdaExpr(LambdaExpr* lambdaExpr) {
        dbg(CAIDE_FUNC);
        insertReference(getParentDecl(lambdaExpr), lambdaExpr->getCallOperator());
        return true;
    }

    bool VisitFieldDecl(FieldDecl* field) {
        dbg(CAIDE_FUNC);
        insertReference(field, field->getParent());
        return true;
    }

    bool VisitTypedefDecl(TypedefDecl* typedefDecl) {
        dbg(CAIDE_FUNC);
        insertReferenceToType(typedefDecl, typedefDecl->getUnderlyingType());
        return true;
    }

    bool VisitClassTemplateDecl(ClassTemplateDecl* templateDecl) {
        dbg(CAIDE_FUNC);
        insertReference(templateDecl, templateDecl->getTemplatedDecl());
        return true;
    }

    bool VisitClassTemplateSpecializationDecl(ClassTemplateSpecializationDecl* specDecl) {
        dbg(CAIDE_FUNC);
        llvm::PointerUnion<ClassTemplateDecl*, ClassTemplatePartialSpecializationDecl*>
            instantiatedFrom = specDecl->getSpecializedTemplateOrPartial();

        if (instantiatedFrom.is<ClassTemplateDecl*>())
            insertReference(specDecl, instantiatedFrom.get<ClassTemplateDecl*>());
        else if (instantiatedFrom.is<ClassTemplatePartialSpecializationDecl*>())
            insertReference(specDecl, instantiatedFrom.get<ClassTemplatePartialSpecializationDecl*>());

        return true;
    }

    bool VisitFunctionDecl(FunctionDecl* f) {
        dbg(CAIDE_FUNC);
        if (f->isMain())
            srcInfo.declsToKeep.insert(f);

        if (f->doesThisDeclarationHaveABody() && !f->isLateTemplateParsed()) {
            // first rewind function stack
            DeclContext* ctx = f;
            auto it = functionLexicalStack.end();

            do {
                ctx = ctx->getLexicalParent();
                if (ctx && isa<FunctionDecl>(ctx)) {
                    it = std::find_if(
                        functionLexicalStack.begin(), functionLexicalStack.end(),
                        [&](const std::pair<FunctionDecl*, std::unique_ptr<StmtParentMap>>& p) {
                            return p.first == ctx;
                        });
                }
            } while (ctx && it == functionLexicalStack.end());

            if (it != functionLexicalStack.end()) {
                ++it;
                functionLexicalStack.erase(it, functionLexicalStack.end());
            }

            // now push current function on top of the stack
            std::unique_ptr<StmtParentMap> parentMap(new StmtParentMap(f->getBody()));

            // Function arguments are not inside the body; need to add them to the parent map explicitly
            for (unsigned i = 0; i < f->getNumParams(); ++i) {
                ParmVarDecl* argument = f->getParamDecl(i);
                if (argument->hasDefaultArg() && !argument->hasUnparsedDefaultArg()
                    && !argument->hasUninstantiatedDefaultArg())
                {
                    parentMap->addStmt(argument->getDefaultArg());
                }

                // Add reference to the parameter
                insertReference(f, argument);
            }

            // Ditto for constructor initializers
            if (CXXConstructorDecl* ctorDecl = dyn_cast<CXXConstructorDecl>(f)) {
                for (auto init = ctorDecl->init_begin(); init != ctorDecl->init_end(); ++init) {
                    parentMap->addStmt((**init).getInit());
                }
            }

            functionLexicalStack.emplace_back(f, std::move(parentMap));
        }

        if (sourceManager.isInMainFile(f->getLocStart()) && f->isLateTemplateParsed())
            srcInfo.delayedParsedFunctions.push_back(f);

        if (f->getTemplatedKind() == FunctionDecl::TK_FunctionTemplate) {
            // skip non-instantiated template function
            return true;
        }

        FunctionTemplateSpecializationInfo* specInfo = f->getTemplateSpecializationInfo();
        if (specInfo)
            insertReference(f, specInfo->getTemplate());

        insertReferenceToType(f, f->getReturnType());

        insertReference(f, f->getInstantiatedFromMemberFunction());

        if (f->doesThisDeclarationHaveABody() &&
                sourceManager.isInMainFile(f->getLocStart()))
        {
            dbg("Moving to ";
                DeclarationName DeclName = f->getNameInfo().getName();
                string FuncName = DeclName.getAsString();
                cerr << FuncName << " at " << toString(f->getLocation()) << std::endl;
            );
        }

        return true;
    }

    bool VisitFunctionTemplateDecl(FunctionTemplateDecl* functionTemplate) {
        insertReference(functionTemplate,
                functionTemplate->getInstantiatedFromMemberTemplate());
        return true;
    }

    bool VisitCXXMethodDecl(CXXMethodDecl* method) {
        dbg(CAIDE_FUNC);
        insertReference(method, method->getParent());
        if (method->isVirtual()) {
            // Virtual methods may not be called directly. Assume that
            // if we need a class, we need all its virtual methods.
            // TODO: a more detailed analysis (walk the inheritance tree?)
            insertReference(method->getParent(), method);
        }
        return true;
    }

    bool VisitCXXRecordDecl(CXXRecordDecl* recordDecl) {
        insertReference(recordDecl, recordDecl->getDescribedClassTemplate());
        return true;
    }
};


class UsageInfo {
private:
    SourceManager& sourceManager;
    SourceRangeComparer cmp;
    std::set<Decl*> usedDecls;
    std::set<SourceRange, SourceRangeComparer> locationsOfUsedDecls;

public:
    UsageInfo(SourceManager& sourceManager_, Rewriter& rewriter_)
        : sourceManager(sourceManager_)
    {
        cmp.cmp.rewriter = &rewriter_;
        locationsOfUsedDecls = std::set<SourceRange, SourceRangeComparer>(cmp);
    }

    bool isUsed(Decl* decl) const {
        if (usedDecls.find(decl) != usedDecls.end())
            return true;

        SourceRange range = getSourceRange(decl);
        return locationsOfUsedDecls.find(range) != locationsOfUsedDecls.end();
    }

    void addIfInMainFile(Decl* decl) {
        SourceLocation start = decl->getLocStart();
        if (start.isMacroID())
            start = sourceManager.getExpansionRange(start).first;

        if (sourceManager.isInMainFile(start)) {
            SourceLocation end = decl->getLocEnd();
            if (end.isMacroID())
                end = sourceManager.getExpansionRange(end).second;
            usedDecls.insert(decl);
            locationsOfUsedDecls.insert(SourceRange(start, end));
        }
    }

private:
    SourceRange getSourceRange(Decl* decl) const {
        SourceLocation start = decl->getLocStart();
        if (start.isMacroID())
            start = sourceManager.getExpansionRange(start).first;

        SourceLocation end = decl->getLocEnd();
        if (end.isMacroID())
            end = sourceManager.getExpansionRange(end).second;

        return SourceRange(start, end);
    }
};

class OptimizerVisitor: public RecursiveASTVisitor<OptimizerVisitor> {
private:
    SourceManager& sourceManager;
    const UsageInfo usageInfo;
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
    OptimizerVisitor(SourceManager& srcManager, const UsageInfo& usageInfo_, SmartRewriter& rewriter_)
        : sourceManager(srcManager)
        , usageInfo(usageInfo_)
        , rewriter(rewriter_)
    {}

    // When we remove code, we're only interested in the real code,
    // so no implicit instantiantions.
    bool shouldVisitImplicitCode() const { return false; }
    bool shouldVisitTemplateInstantiations() const { return false; }

    bool VisitEmptyDecl(EmptyDecl* decl) {
        if (sourceManager.isInMainFile(decl->getLocStart()))
            removeDecl(decl);
        return true;
    }

    /*
    bool VisitStmt(Stmt* stmt) {
        cerr << stmt->getStmtClassName() << endl;
        return true;
    }
    */

    /*
     Here's how template functions and classes are represented in the AST.
-FunctionTemplateDecl <-- the template
 |-TemplateTypeParmDecl
 |-FunctionDecl  <-- general (non-specialized) case
 |-FunctionDecl  <-- for each implicit instantiation of the template
 | `-CompoundStmt
 |   `-...
-FunctionDecl   <-- non-template or full explicit specialization of a template



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

        // Correct source range (including all template<...> parts) for
        // method templates is in this decl; for function templates
        // it's in described template (FunctionTemplateDecl)
        if (functionDecl->getDescribedFunctionTemplate() != nullptr &&
            !isa<CXXMethodDecl>(functionDecl))
        {
            return true;
        }

        if (CXXDestructorDecl* destructor = dyn_cast<CXXDestructorDecl>(functionDecl)) {
            // Destructor should be removed iff the class is unused
            CXXRecordDecl* classDecl = destructor->getParent();
            if (classDecl && !usageInfo.isUsed(classDecl->getCanonicalDecl()))
                removeDecl(destructor);
            return true;
        }

        FunctionDecl* canonicalDecl = functionDecl->getCanonicalDecl();
        const bool funcIsUnused = !usageInfo.isUsed(canonicalDecl);
        const bool thisIsRedeclaration = !functionDecl->doesThisDeclarationHaveABody() && declared.find(canonicalDecl) != declared.end();
        if (funcIsUnused || thisIsRedeclaration) {
            dbg(CAIDE_FUNC);
            removeDecl(functionDecl);
        }
        declared.insert(canonicalDecl);
        return true;
    }

    bool VisitFunctionTemplateDecl(FunctionTemplateDecl* functionDecl) {
        if (!sourceManager.isInMainFile(functionDecl->getLocStart()))
            return true;
        dbg(CAIDE_FUNC);
        if (isa<CXXMethodDecl>(functionDecl->getTemplatedDecl())) {
            // Source range for this decl may not include all template parameters;
            // it will be processed as CXXMethodDecl instead.
            // See corresponding comment in VisitFunctionDecl.
            return true;
        }
        if (!usageInfo.isUsed(functionDecl->getCanonicalDecl()))
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
        const bool classIsUnused = !usageInfo.isUsed(canonicalDecl);
        const bool thisIsRedeclaration = !recordDecl->isCompleteDefinition() && declared.find(canonicalDecl) != declared.end();

        if (classIsUnused || thisIsRedeclaration)
            removeDecl(recordDecl);

        declared.insert(canonicalDecl);
        return true;
    }

    bool VisitClassTemplateDecl(ClassTemplateDecl* templateDecl) {
        if (!sourceManager.isInMainFile(templateDecl->getLocStart()))
            return true;
        dbg(CAIDE_FUNC);
        ClassTemplateDecl* canonicalDecl = templateDecl->getCanonicalDecl();
        const bool classIsUnused = !usageInfo.isUsed(canonicalDecl);
        const bool thisIsRedeclaration = !templateDecl->isThisDeclarationADefinition() && declared.find(canonicalDecl) != declared.end();

        if (classIsUnused || thisIsRedeclaration)
            removeDecl(templateDecl);

        declared.insert(canonicalDecl);
        return true;
    }

    bool VisitTypedefDecl(TypedefDecl* typedefDecl) {
        if (!sourceManager.isInMainFile(typedefDecl->getLocStart()))
            return true;

        Decl* canonicalDecl = typedefDecl->getCanonicalDecl();
        if (!usageInfo.isUsed(canonicalDecl))
            removeDecl(typedefDecl);

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
        if (start.isMacroID())
            start = sourceManager.getExpansionRange(start).first;
        SourceLocation end = decl->getLocEnd();
        if (end.isMacroID())
            end = sourceManager.getExpansionRange(end).second;

        SourceLocation semicolonAfterDefinition = findSemiAfterLocation(end, decl->getASTContext());
        dbg("REMOVE " << decl->getDeclKindName() << " "
            << decl << ": " << toString(start) << " " << toString(end)
            << " " << toString(semicolonAfterDefinition) << std::endl);
        if (semicolonAfterDefinition.isValid())
            end = semicolonAfterDefinition;
        Rewriter::RewriteOptions opts;
        opts.RemoveLineIfEmpty = true;
        rewriter.removeRange(SourceRange(start, end), opts);
    }

    std::string toString(const SourceLocation& loc) const {
        return loc.printToString(sourceManager);
    }
};

class OptimizerConsumer: public ASTConsumer {
public:
    explicit OptimizerConsumer(CompilerInstance& compiler_, SmartRewriter& smartRewriter_,
                Rewriter& rewriter_, RemoveInactivePreprocessorBlocks& ppCallbacks_,
                std::string& result_)
        : compiler(compiler_)
        , sourceManager(compiler.getSourceManager())
        , smartRewriter(smartRewriter_)
        , rewriter(rewriter_)
        , ppCallbacks(ppCallbacks_)
        , result(result_)
    {}

    virtual void HandleTranslationUnit(ASTContext& Ctx) {
        //cerr << "Build dependency graph" << std::endl;
        DependenciesCollector depsVisitor(sourceManager, srcInfo);
        depsVisitor.TraverseDecl(Ctx.getTranslationUnitDecl());

        // Source range of delayed-parsed template functions includes only declaration part.
        //     Force their parsing to get correct source ranges.
        //     Suppress error messages temporarily (it's OK for these functions
        //     to be malformed).
        clang::Sema& sema = compiler.getSema();
        sema.getDiagnostics().setSuppressAllDiagnostics(true);
        for (FunctionDecl* f : srcInfo.delayedParsedFunctions) {
            clang::LateParsedTemplate* lpt = sema.LateParsedTemplateMap[f];
            sema.LateTemplateParser(sema.OpaqueParser, *lpt);
        }
        sema.getDiagnostics().setSuppressAllDiagnostics(false);

        //cerr << "Search for used decls" << std::endl;
        UsageInfo usageInfo(sourceManager, rewriter);
        std::set<Decl*> used;
        std::set<Decl*> queue;
        for (Decl* decl : srcInfo.declsToKeep)
            queue.insert(decl->getCanonicalDecl());

        while (!queue.empty()) {
            Decl* decl = *queue.begin();
            queue.erase(queue.begin());
            if (used.insert(decl).second) {
                queue.insert(srcInfo.uses[decl].begin(), srcInfo.uses[decl].end());
                usageInfo.addIfInMainFile(decl);
            }
        }

        used.clear();

        //cerr << "Remove unused decls" << std::endl;
        OptimizerVisitor visitor(sourceManager, usageInfo, smartRewriter);
        visitor.TraverseDecl(Ctx.getTranslationUnitDecl());

        removeUnusedVariables(usageInfo, Ctx);

        ppCallbacks.Finalize();

        smartRewriter.applyChanges();

        //cerr << "Done!" << std::endl;
        result = getResult();
    }

private:
    // Variables are a special case because there may be many
    // comma separated variables in one definition.
    void removeUnusedVariables(const UsageInfo& usageInfo, ASTContext& ctx) {
        Rewriter::RewriteOptions opts;
        opts.RemoveLineIfEmpty = true;

        for (const auto& kv : srcInfo.staticVariables) {
            const vector<VarDecl*>& vars = kv.second;
            const size_t n = vars.size();
            vector<bool> isUsed(n, true);
            size_t lastUsed = n;
            for (size_t i = 0; i < n; ++i) {
                VarDecl* var = vars[i];
                isUsed[i] = usageInfo.isUsed(var->getCanonicalDecl());
                if (isUsed[i])
                    lastUsed = i;
            }

            SourceLocation startOfType = kv.first;
            SourceLocation endOfLastVar = vars.back()->getSourceRange().getEnd();
            if (endOfLastVar.isMacroID())
                endOfLastVar = sourceManager.getExpansionRange(endOfLastVar).second;
            SourceLocation semiColon = findSemiAfterLocation(endOfLastVar, ctx);

            if (lastUsed == n) {
                // all variables are unused
                SourceRange range(startOfType, semiColon);
                smartRewriter.removeRange(range, opts);
            } else {
                for (size_t i = 0; i < lastUsed; ++i) if (!isUsed[i]) {
                    // beginning of variable name
                    SourceLocation beg = vars[i]->getLocation();

                    // end of initializer
                    SourceLocation end = vars[i]->getSourceRange().getEnd();
                    if (end.isMacroID())
                        end = sourceManager.getExpansionRange(end).second;

                    if (i+1 < n) {
                        // comma
                        end = findTokenAfterLocation(end, ctx, tok::comma);
                    }

                    if (beg.isValid() && end.isValid()) {
                        SourceRange range(beg, end);
                        smartRewriter.removeRange(range, opts);
                    }
                }
                if (lastUsed + 1 != n) {
                    // clear all remaining variables, starting with comma
                    SourceLocation end = vars[lastUsed]->getSourceRange().getEnd();
                    if (end.isMacroID())
                        end = sourceManager.getExpansionRange(end).second;
                    SourceLocation comma = findTokenAfterLocation(end, ctx, tok::comma);
                    SourceRange range(comma, endOfLastVar);
                    smartRewriter.removeRange(range, opts);
                }
            }
        }
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
        if (const RewriteBuffer* rewriteBuf =
                smartRewriter.getRewriteBufferFor(sourceManager.getMainFileID()))
            return std::string(rewriteBuf->begin(), rewriteBuf->end());

        // No changes
        bool invalid;
        const llvm::MemoryBuffer* buf = sourceManager.getBuffer(sourceManager.getMainFileID(), &invalid);
        if (buf && !invalid)
            return std::string(buf->getBufferStart(), buf->getBufferEnd());
        else
            return "Inliner error"; // something's wrong
    }

private:
    CompilerInstance& compiler;
    SourceManager& sourceManager;
    SmartRewriter& smartRewriter;
    Rewriter& rewriter;
    RemoveInactivePreprocessorBlocks& ppCallbacks;
    std::string& result;
    SourceInfo srcInfo;
};


class OptimizerFrontendAction : public ASTFrontendAction {
private:
    Rewriter& rewriter;
    SmartRewriter& smartRewriter;
    string& result;
    const set<string>& macrosToKeep;
public:
    OptimizerFrontendAction(Rewriter& rewriter_, SmartRewriter& smartRewriter_,
           string& result_, const set<string>& macrosToKeep_)
        : rewriter(rewriter_)
        , smartRewriter(smartRewriter_)
        , result(result_)
        , macrosToKeep(macrosToKeep_)
    {}

    virtual std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance& compiler, StringRef /*file*/)
    {
        if (!compiler.hasSourceManager())
            throw "No source manager";
        rewriter.setSourceMgr(compiler.getSourceManager(), compiler.getLangOpts());
        auto ppCallbacks = std::unique_ptr<RemoveInactivePreprocessorBlocks>(
            new RemoveInactivePreprocessorBlocks(compiler.getSourceManager(), smartRewriter, macrosToKeep));
        auto consumer = std::unique_ptr<OptimizerConsumer>(new OptimizerConsumer(compiler, smartRewriter, rewriter, *ppCallbacks, result));
        compiler.getPreprocessor().addPPCallbacks(std::move(ppCallbacks));
        return std::move(consumer);
    }
};

class OptimizerFrontendActionFactory: public tooling::FrontendActionFactory {
private:
    Rewriter& rewriter;
    SmartRewriter smartRewriter;
    string& result;
    const set<string>& macrosToKeep;
public:
    OptimizerFrontendActionFactory(Rewriter& rewriter_, string& result_,
                const set<string>& macrosToKeep_)
        : rewriter(rewriter_)
        , smartRewriter(rewriter_)
        , result(result_)
        , macrosToKeep(macrosToKeep_)
    {}
    FrontendAction* create() {
        return new OptimizerFrontendAction(rewriter, smartRewriter, result, macrosToKeep);
    }
};



Optimizer::Optimizer(const std::vector<std::string>& cmdLineOptions_,
                     const std::vector<std::string>& macrosToKeep_)
    : cmdLineOptions(cmdLineOptions_)
    , macrosToKeep(macrosToKeep_.begin(), macrosToKeep_.end())
{}

std::string Optimizer::doOptimize(const std::string& cppFile) {
    std::auto_ptr<tooling::FixedCompilationDatabase> compilationDatabase(
        createCompilationDatabaseFromCommandLine(cmdLineOptions));

    vector<string> sources;
    sources.push_back(cppFile);

    clang::tooling::ClangTool tool(*compilationDatabase, sources);

    Rewriter rewriter;
    string result;
    OptimizerFrontendActionFactory factory(rewriter, result, macrosToKeep);

    int ret = tool.run(&factory);
    if (ret != 0)
        throw std::runtime_error("Compilation error");

    return result;
}

