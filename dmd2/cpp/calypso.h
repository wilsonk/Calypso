// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CALYPSO_H
#define DMD_CPP_CALYPSO_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "cpp/cpptypes.h"
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
    class ASTUnit;
    class CodeGenModule;
    class CodeGenFunction;
    class Sema;
}

namespace cpp
{

class ClassDeclaration;

Identifier *toIdentifier(clang::IdentifierInfo *II);
Identifier *getIdentifier(const clang::NamedDecl* D);
Loc toLoc(clang::SourceLocation L);

struct PCH
{
    Strings headers; // array of all C/C++ header names with the "" or <>, required as long as we're using a PCH
            // the array is initialized at the first Modmap::semantic and kept in sync with a cache file named 'fringed_cache.list'
            // TODO: it's currently pretty basic and dumb and doesn't check whether the same header might be named differently or is already included by another
    bool needEmit = false;
    clang::ASTUnit *AST = nullptr;
    clang::IntrusiveRefCntPtr<clang::DiagnosticsEngine> Diags;

    void init(); // load the list of headers already cached in the PCH
    void add(StringRef header);

    void update(); // re-emit the PCH if needed, and update the cached list
};

// Copy and pasted from clang::CodeGenImpl, because that one is declared inside an anonymous namespace in ModuleBuilder.cpp
// Avoiding the redundancy would mean altering Clang's source code, so to keep Calypso compatible with vanilla binaries it seems more reasonable to handle it like that
// FIXME that class might not be as useful as I first thought, calling CGM methods directly would be cleaner and simpler
class AssistBuilder : public clang::ASTConsumer {
    clang::DiagnosticsEngine &Diags;
    std::unique_ptr<const llvm::DataLayout> TD;
    clang::ASTContext *Ctx;
    const clang::CodeGenOptions CodeGenOpts;  // Intentionally copied in.
protected:
    std::unique_ptr<clang::CodeGen::CodeGenModule> Builder;
public:
    AssistBuilder( clang::DiagnosticsEngine& diags, llvm::Module* M, const clang::CodeGenOptions& CGO, llvm::LLVMContext& C );
    virtual ~AssistBuilder();

    virtual void Initialize(clang::ASTContext &Context);
    virtual void HandleCXXStaticMemberVarInstantiation(clang::VarDecl *VD);
    virtual bool HandleTopLevelDecl(clang::DeclGroupRef DG);
    /// HandleTagDeclDefinition - This callback is invoked each time a TagDecl
    /// to (e.g. struct, union, enum, class) is completed. This allows the
    /// client hack on the type, which can occur at any point in the file
    /// (because these can be defined in declspecs).
    virtual void HandleTagDeclDefinition(clang::TagDecl *D);
    virtual void HandleTagDeclRequiredDefinition(const clang::TagDecl *D) LLVM_OVERRIDE;
    virtual void HandleTranslationUnit(clang::ASTContext &Ctx);
    virtual void CompleteTentativeDefinition(clang::VarDecl *D);
    virtual void HandleVTable(clang::CXXRecordDecl *RD, bool DefinitionRequired);
    virtual void HandleLinkerOptionPragma(llvm::StringRef Opts);
    virtual void HandleDetectMismatch(llvm::StringRef Name,
                                        llvm::StringRef Value);
    virtual void HandleDependentLibrary(llvm::StringRef Lib);

    // CALYPSO additions
public:
    llvm::Module* M;

    clang::CodeGen::CodeGenModule *CGM() { return Builder.get(); }
    llvm::Constant *GetAddrOfGlobal(clang::GlobalDecl GD);
};

class LangPlugin : public ::LangPlugin, public CodeGen
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
    CodeGen *codegen() { return this; }

    clang::CodeGen::CodeGenFunction *CGF = nullptr; // FIXME: won't work with nested funcs, need stack

    void enterModule(llvm::Module *m) override;
    void leaveModule() override;

    void enterFunc(::FuncDeclaration *fd) override;
    void leaveFunc() override;

    llvm::Type *toType(Type *t) override;
    llvm::Constant *createInitializerConstant(IrAggr *irAggr,
        const IrAggr::VarInitMap& explicitInitializers,
        llvm::StructType* initializerType = 0) override;
        
    void buildGEPIndices(IrTypeAggr *irTyAgrr, VarGEPIndices &varGEPIndices) override;
        
    void toInitClass(TypeClass* tc, LLValue* dst) override;
    
    LLValue *toVirtualFunctionPointer(DValue* inst, ::FuncDeclaration* fdecl, char* name) override;

    DValue* toCallFunction(Loc& loc, Type* resulttype, DValue* fnval, Expressions* arguments, llvm::Value *retvar) override;

    void toResolveFunction(::FuncDeclaration* fdecl) override;

    void addBaseClassData(AggrTypeBuilder &builder, ::AggregateDeclaration *base) override;

    void emitAdditionalClassSymbols(::ClassDeclaration *cd) override;

    void toPostNewClass(Loc& loc, TypeClass* tc, DValue* val) override;

    void toDeclareVariable(::VarDeclaration* vd) override;
         
    // ==== ==== ====
    PCH pch;

    BuiltinTypes builtinTypes;

    clang::QualType fromType(Type *t);

    // settings
    const char *cachePrefix = "calypso_cache"; // prefix of cached files (list of headers, PCH)

    std::unique_ptr<AssistBuilder> AB;  // selective emit external C++ declarations, expressions, template instances, ...

    void init();
    clang::ASTUnit *getASTUnit() { return pch.AST; }
    clang::ASTContext &getASTContext();
    
private:
    void updateCGFInsertPoint();    // CGF has its own IRBuilder, it's not an issue if we set its insert point correctly
};

extern LangPlugin calypso;
bool isCPP(Dsymbol *s);
cpp::ClassDeclaration *isDCXX(Dsymbol *s);

}

#define CALYPSO_LANGPLUGIN \
    ::LangPlugin *langPlugin() override { return &calypso; }

#endif /* DMD_CPP_CALYPSO_H */
