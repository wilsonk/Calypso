// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CALYPSO_H
#define DMD_CPP_CALYPSO_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "../mars.h"
#include "../import.h"
#include "../gen/cgforeign.h"

#include <memory>
#include "llvm/IR/DataLayout.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/CodeGen/ModuleBuilder.h"
#include "clang/Frontend/CodeGenOptions.h"
#include "clang/lib/CodeGen/CodeGenModule.h"

class Identifier;

namespace clang
{
class IdentifierInfo;
class CodeGenFunction;
class Sema;
class ModuleMap;
}

namespace cpp
{

class ClassDeclaration;
class BuiltinTypes;
class TemplateInstance;

namespace reclang { class ASTUnit; }
using reclang::ASTUnit;

namespace clangCG = clang::CodeGen;

Identifier *fromIdentifier(const clang::IdentifierInfo *II);
Identifier *getIdentifier(const clang::NamedDecl *D, const char **op = nullptr);
Identifier *getIdentifierOrNull(const clang::NamedDecl *D, const char **op = nullptr);
Identifier *getExtendedIdentifier(const clang::NamedDecl *D); // will return the name of the non-templated method for operators, same than getIdentifier() for other Decls

Loc fromLoc(clang::SourceLocation L);

// This collects the *new* function instances that a template instance depends upon, they need to be emitted
struct InstantiationCollector : public clang::ASTConsumer
{
    std::stack<TemplateInstance *> tempinsts;

    bool HandleTopLevelDecl(clang::DeclGroupRef DG) override;
};

struct PCH
{
    Strings headers; // array of all C/C++ header names with the "" or <>, required as long as we're using a PCH
            // the array is initialized at the first Modmap::semantic and kept in sync with a cache file named 'fringed_cache.list'
            // TODO: it's currently pretty basic and dumb and doesn't check whether the same header might be named differently or is already included by another
    bool needEmit = false;
    ASTUnit *AST = nullptr;
    InstantiationCollector instCollector;
    clang::IntrusiveRefCntPtr<clang::DiagnosticsEngine> Diags;
    
    clang::ModuleMap *MMap = nullptr;

    void init(); // load the list of headers already cached in the PCH
    void add(StringRef header);

    void update(); // re-emit the PCH if needed, and update the cached list
};

class LangPlugin : public ::LangPlugin, public ::ForeignCodeGen
{
public:
    // ==== LangPlugin ====
    // returns -1 if said lang isn't handled by this plugin, or its id number
    // to be passed to createImport otherwise
    int doesHandleModmap(const utf8_t *lang) override;

    ::Modmap *createModmap(int langId,
        Loc loc, Expression *arg) override;

    // returns -1 if said tree isn't handled by this plugin, or its id number
    // to be passed to createImport otherwise
    int doesHandleImport(const utf8_t *tree) override;

    ::Import *createImport(int treeId,
        Loc loc, Identifiers *packages, Identifier *id,
        Identifier *aliasId, int isstatic) override;

    Expression *getRightThis(Loc loc, Scope *sc, ::AggregateDeclaration *ad,
        Expression *e1, Declaration *var, int flag = 0) override;

    ::FuncDeclaration *buildDtor(::AggregateDeclaration *ad, Scope *sc) override;
    ::FuncDeclaration *buildCpCtor(::StructDeclaration *sd, Scope *sc) override;

    // ==== CodeGen ====
    ForeignCodeGen *codegen() { return this; }

    std::stack<clangCG::CodeGenFunction *> CGFStack;
    inline clangCG::CodeGenFunction *CGF() { return CGFStack.top(); }

    void enterModule(llvm::Module *m) override;
    void leaveModule() override;

    void enterFunc(::FuncDeclaration *fd) override;
    void leaveFunc() override;

    LLType *toType(Type *t) override;

    llvm::Constant *createInitializerConstant(IrAggr *irAggr,
        const IrAggr::VarInitMap& explicitInitializers,
        llvm::StructType* initializerType = 0) override;

    void toResolveFunction(::FuncDeclaration* fdecl) override;
    void toDefineFunction(::FuncDeclaration* fdecl) override;
    void toDeclareVariable(::VarDeclaration* vd) override;
    void toDefineVariable(::VarDeclaration* vd) override;
    void toDefineTemplateInstance(::TemplateInstance *tempinst) override;

    LLValue *toVirtualFunctionPointer(DValue* inst, ::FuncDeclaration* fdecl, char* name) override;
    DValue* toCallFunction(Loc& loc, Type* resulttype, DValue* fnval,
                                   Expressions* arguments, llvm::Value *retvar) override;

    void buildGEPIndices(IrTypeAggr *irTyAgrr, VarGEPIndices &varGEPIndices) override;
    void addBaseClassData(AggrTypeBuilder &builder, ::AggregateDeclaration *base) override;
    void emitAdditionalClassSymbols(::ClassDeclaration *cd) override;
    void toInitClass(TypeClass* tc, LLValue* dst) override;
    void toPostNewClass(Loc& loc, TypeClass* tc, DValue* val) override;
         
    // ==== ==== ====
    PCH pch;

    BuiltinTypes &builtinTypes;

    // settings
    const char *cachePrefix = "calypso_cache"; // prefix of cached files (list of headers, PCH)

    std::unique_ptr<clangCG::CodeGenModule> CGM;  // selectively emit external C++ declarations, template instances, ...

    LangPlugin();
    void init();
    ASTUnit *getASTUnit() { return pch.AST; }
    clang::ASTContext &getASTContext();
    
private:
    void updateCGFInsertPoint();    // CGF has its own IRBuilder, it's not an issue if we set its insert point correctly
};

extern LangPlugin calypso;
bool isCPP(Type* t);
bool isCPP(Dsymbol *s);
cpp::ClassDeclaration *isDCXX(Dsymbol *s);

}

#define CALYPSO_LANGPLUGIN \
    ::LangPlugin *langPlugin() override { return &calypso; }

#endif /* DMD_CPP_CALYPSO_H */
