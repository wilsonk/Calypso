#include "cpp/calypso.h"

#include "clang/Frontend/ASTUnit.h"
#include "clang/lib/CodeGen/CGDebugInfo.h"

namespace cpp
{

using namespace clang;
using namespace clang::CodeGen;

AssistBuilder::AssistBuilder(DiagnosticsEngine &diags,
                    llvm::Module *M,
                    const CodeGenOptions &CGO, llvm::LLVMContext& C)
    : Diags(diags), CodeGenOpts(CGO), M(M){}

AssistBuilder::~AssistBuilder() {}

void AssistBuilder::Initialize(ASTContext &Context) {
    Ctx = &Context;

    TD.reset(new llvm::DataLayout(Ctx->getTargetInfo().getTargetDescription()));
    Builder.reset(new CodeGenModule(Context, CodeGenOpts, *M, *TD,
                                            Diags));

    for (size_t i = 0, e = CodeGenOpts.DependentLibraries.size(); i < e; ++i)
    HandleDependentLibrary(CodeGenOpts.DependentLibraries[i]);
}

void AssistBuilder::HandleCXXStaticMemberVarInstantiation(VarDecl *VD) {
    if (Diags.hasErrorOccurred())
    return;

    Builder->HandleCXXStaticMemberVarInstantiation(VD);
}

bool AssistBuilder::HandleTopLevelDecl(DeclGroupRef DG) {
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
void AssistBuilder::HandleTagDeclDefinition(TagDecl *D) {
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

void AssistBuilder::HandleTagDeclRequiredDefinition(const TagDecl *D) {
    if (Diags.hasErrorOccurred())
        return;

    if (CGDebugInfo *DI = Builder->getModuleDebugInfo())
    if (const RecordDecl *RD = dyn_cast<RecordDecl>(D))
        DI->completeRequiredType(RD);
}

void AssistBuilder::HandleTranslationUnit(ASTContext &Ctx) {
    if (Diags.hasErrorOccurred())
        return;

    if (Builder)
    Builder->Release();
}

void AssistBuilder::CompleteTentativeDefinition(VarDecl *D) {
    if (Diags.hasErrorOccurred())
        return;

    Builder->EmitTentativeDefinition(D);
}

void AssistBuilder::HandleVTable(CXXRecordDecl *RD, bool DefinitionRequired) {
    if (Diags.hasErrorOccurred())
        return;

    Builder->EmitVTable(RD, DefinitionRequired);
}

void AssistBuilder::HandleLinkerOptionPragma(llvm::StringRef Opts) {
    Builder->AppendLinkerOptions(Opts);
}

void AssistBuilder::HandleDetectMismatch(llvm::StringRef Name,
                                    llvm::StringRef Value) {
    Builder->AddDetectMismatch(Name, Value);
}

void AssistBuilder::HandleDependentLibrary(llvm::StringRef Lib) {
    Builder->AddDependentLib(Lib);
}

// CALYPSO additions

llvm::Constant* AssistBuilder::GetAddrOfGlobal(GlobalDecl GD)
{
    return Builder->GetAddrOfGlobal(GD);
}

}