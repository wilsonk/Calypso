// Contributed by Elie Morisse, same license DMD uses
#include "cpp/calypso.h"
#include "cpp/cppexpression.h"
#include "cpp/cppaggregate.h"

#include "gen/dvalue.h"
#include "gen/functions.h"
#include "gen/irstate.h"
#include "ir/irfunction.h"
#include "ir/irtype.h"

#include "clang/CodeGen/ModuleBuilder.h"
#include "clang/lib/CodeGen/CGDebugInfo.h"
#include "clang/lib/CodeGen/CodeGenFunction.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/Expr.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Frontend/ASTUnit.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include <memory>

namespace cpp
{

using namespace clang;

AssistantBuilder::AssistantBuilder(DiagnosticsEngine &diags,
                    llvm::Module *M,
                    const CodeGenOptions &CGO, llvm::LLVMContext& C)
    : Diags(diags), CodeGenOpts(CGO), M(M){}

AssistantBuilder::~AssistantBuilder() {}

void AssistantBuilder::Initialize(ASTContext &Context) {
    Ctx = &Context;

    TD.reset(new llvm::DataLayout(Ctx->getTargetInfo().getTargetDescription()));
    Builder.reset(new CodeGen::CodeGenModule(Context, CodeGenOpts, *M, *TD,
                                            Diags));

    for (size_t i = 0, e = CodeGenOpts.DependentLibraries.size(); i < e; ++i)
    HandleDependentLibrary(CodeGenOpts.DependentLibraries[i]);
}

void AssistantBuilder::HandleCXXStaticMemberVarInstantiation(VarDecl *VD) {
    if (Diags.hasErrorOccurred())
    return;

    Builder->HandleCXXStaticMemberVarInstantiation(VD);
}

bool AssistantBuilder::HandleTopLevelDecl(DeclGroupRef DG) {
    if (Diags.hasErrorOccurred())
    return true;

    // Make sure to emit all elements of a Decl.
    for (DeclGroupRef::iterator I = DG.begin(), E = DG.end(); I != E; ++I)
    Builder->EmitTopLevelDecl(*I);
    return true;
}

/// HandleTagDeclDefinition - This callback is invoked each time a TagDecl
/// to (e.g. struct, union, enum, class) is completed. This allows the
/// client hack on the type, which can occur at any point in the file
/// (because these can be defined in declspecs).
void AssistantBuilder::HandleTagDeclDefinition(TagDecl *D) {
    if (Diags.hasErrorOccurred())
    return;

    Builder->UpdateCompletedType(D);

    // In C++, we may have member functions that need to be emitted at this
    // point.
    if (Ctx->getLangOpts().CPlusPlus && !D->isDependentContext()) {
    for (DeclContext::decl_iterator M = D->decls_begin(),
                                    MEnd = D->decls_end();
            M != MEnd; ++M)
        if (CXXMethodDecl *Method = dyn_cast<CXXMethodDecl>(*M))
        if (Method->doesThisDeclarationHaveABody() &&
            (Method->hasAttr<UsedAttr>() ||
                Method->hasAttr<ConstructorAttr>()))
            Builder->EmitTopLevelDecl(Method);
    }
}

void AssistantBuilder::HandleTagDeclRequiredDefinition(const TagDecl *D) LLVM_OVERRIDE {
    if (Diags.hasErrorOccurred())
        return;

    if (CodeGen::CGDebugInfo *DI = Builder->getModuleDebugInfo())
    if (const RecordDecl *RD = dyn_cast<RecordDecl>(D))
        DI->completeRequiredType(RD);
}

void AssistantBuilder::HandleTranslationUnit(ASTContext &Ctx) {
    if (Diags.hasErrorOccurred())
        return;

    if (Builder)
    Builder->Release();
}

void AssistantBuilder::CompleteTentativeDefinition(VarDecl *D) {
    if (Diags.hasErrorOccurred())
        return;

    Builder->EmitTentativeDefinition(D);
}

void AssistantBuilder::HandleVTable(CXXRecordDecl *RD, bool DefinitionRequired) {
    if (Diags.hasErrorOccurred())
        return;

    Builder->EmitVTable(RD, DefinitionRequired);
}

void AssistantBuilder::HandleLinkerOptionPragma(llvm::StringRef Opts) {
    Builder->AppendLinkerOptions(Opts);
}

void AssistantBuilder::HandleDetectMismatch(llvm::StringRef Name,
                                    llvm::StringRef Value) {
    Builder->AddDetectMismatch(Name, Value);
}

void AssistantBuilder::HandleDependentLibrary(llvm::StringRef Lib) {
    Builder->AddDependentLib(Lib);
}

// CALYPSO additions

llvm::Constant* AssistantBuilder::GetAddrOfGlobal(GlobalDecl GD)
{
    return Builder->GetAddrOfGlobal(GD);
}

}

//////////////////////////////////////////////////////////////////////////////////////////

namespace cpp
{

void LangPlugin::enterModule(llvm::Module *m)
{
    clang::CodeGenOptions CGO;
    auto& AB = calypso.AB;
    auto& ASTContext = ASTUnit()->getASTContext();

    AB = new AssistantBuilder(*pch.Diags, m, CGO, llvm::getGlobalContext());
    AB->Initialize(ASTContext);
    AB->HandleTranslationUnit(ASTContext);
}

void LangPlugin::enterFunc(::FuncDeclaration *fd)
{//FIXME nested
//     auto CGM = AB->CGM();
//     CGF = new clang::CodeGen::CodeGenFunction(*CGM, true);
}

void LangPlugin::leaveFunc()
{
//     delete CGF;
//     CGF = nullptr;
}

LLType *LangPlugin::toType(::Type *t)
{
    auto CGM = AB->CGM();

    switch(t->ty)
    {
        case Tstruct:
        {
            auto ts = static_cast<TypeStruct*>(t);
            auto c_sd = static_cast<StructDeclaration*>(ts->sym);
            return CGM->getTypes().ConvertTypeForMem(
                clang::QualType(c_sd->RD->getTypeForDecl(),0));
        }
        default:
            ::error(Loc(), "toType failed");
            fatal();
    }

    return nullptr;
}

llvm::Constant * LangPlugin::createInitializerConstant(IrAggr *irAggr,
        const IrAggr::VarInitMap& explicitInitializers,
        llvm::StructType* initializerType)
{
    auto& cppContext = ASTUnit()->getASTContext();
    auto CGM = AB->CGM();

    auto c_sd = static_cast<StructDeclaration*>(irAggr->aggrdecl);
    auto RD = c_sd->RD;

    if (initializerType)
    {
        assert(initializerType->isOpaque());

        auto ts = static_cast<llvm::StructType*>(toType(irAggr->type));
        initializerType->setBody(llvm::ArrayRef<llvm::Type*>(ts->element_begin(), ts->element_end()));
    }

    clang::QualType DestType(RD->getTypeForDecl(), 0);
    DestType.addConst();

    llvm::ArrayRef<clang::Expr*> initExprs;
    auto ILE = new (cppContext) clang::InitListExpr(
            cppContext, clang::SourceLocation(),
            initExprs, clang::SourceLocation());
    ILE->setType(DestType);

    auto c = CGM->EmitConstantExpr(ILE, DestType, CGF);
    auto c_ts = llvm::cast<llvm::StructType>(c->getType());
    assert(c_ts->isLayoutIdentical(initializerType));
    c->mutateType(initializerType);  // dangerous?
    return c;
}

// IMPORTANT NOTE: Clang emits forward decls lazily, which means that no function prototype is actually emitted to LLVM after making a CodeGenerator consume the ASTContext.
// One reason for that is that types need to be complete for a prototype to be emitted (this used to cause obscure segfaults on my language Tales a while back).
// This is why we must keep access to the CodeGenModule so that we can emit them and then copy the LLVM infos.
// (as a side not similarly LLVM lazily puts aggregate types into its context type map, but it doesn't matter here afaik).

void FuncDeclaration::toResolveFunction()
{
    if (ir.isResolved()) return;
    ir.setResolved();
    ir.setDeclared();

    DtoFunctionType(this);

    auto sym = llvm::cast<llvm::Function>
                (calypso.AB->GetAddrOfGlobal(FD));

    auto irFunc = getIrFunc(this, true);
    irFunc->func = llvm::cast<llvm::Function>
                (gIR->module->getOrInsertFunction(
                        sym->getName(),
                        sym->getFunctionType(),
                        sym->getAttributes())
                );
}

} // namespace cpp

