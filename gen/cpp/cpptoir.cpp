// Contributed by Elie Morisse, same license DMD uses
#include "cpp/astunit.h"
#include "cpp/calypso.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppaggregate.h"
#include "cpp/cpptemplate.h"

#include "mtype.h"
#include "target.h"
#include "gen/dvalue.h"
#include "gen/functions.h"
#include "gen/logger.h"
#include "gen/irstate.h"
#include "ir/irfunction.h"
#include "gen/llvmhelpers.h"
#include "ir/irtype.h"
#include "ir/irtypeaggr.h"

#include "clang/CodeGen/CGFunctionInfo.h"
#include "clang/CodeGen/ModuleBuilder.h"
#include "clang/lib/CodeGen/CGCXXABI.h"
#include "clang/lib/CodeGen/CGRecordLayout.h"
#include "clang/lib/CodeGen/CodeGenFunction.h"
#include "clang/lib/CodeGen/CodeGenTypes.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Expr.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/ABI.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/TargetInfo.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include <memory>

//////////////////////////////////////////////////////////////////////////////////////////

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

namespace clangCG = clang::CodeGen;

void LangPlugin::enterModule(llvm::Module *m)
{
    if (!getASTUnit())
        return;

    auto& Context = getASTContext();

    auto CGO = new clang::CodeGenOptions;
    CGM.reset(new clangCG::CodeGenModule(Context,
                            *CGO, *m, *gDataLayout, *pch.Diags));
}

void LangPlugin::leaveModule()
{
    if (!getASTUnit())
        return;

    auto& Context = getASTContext();
    CGM->Release();
    CGM.reset();
}

void LangPlugin::enterFunc(::FuncDeclaration *fd)
{
    if (!getASTUnit())
        return;

    IrFunction *irFunc = getIrFunc(fd);

    CGFStack.push(new clangCG::CodeGenFunction(*CGM, true));
    CGF()->CurCodeDecl = nullptr;
    CGF()->AllocaInsertPt = irFunc->allocapoint;
}

void LangPlugin::leaveFunc()
{
    if (!getASTUnit())
        return;
    
    CGF()->AllocaInsertPt = nullptr;
    delete CGF();
    CGFStack.pop();
}

void LangPlugin::updateCGFInsertPoint()
{
    auto BB = gIR->scope().begin;
    CGF()->Builder.SetInsertPoint(BB);
}

struct ResolvedFunc
{
    llvm::Function *Func;
    llvm::FunctionType *Ty;

    static ResolvedFunc get(clangCG::CodeGenModule &CGM, const clang::FunctionDecl *FD)
    {
        ResolvedFunc result;
        const clangCG::CGFunctionInfo *FInfo;

        auto MD = dyn_cast<const clang::CXXMethodDecl>(FD);

        if (MD)
        {
            if (isa<const clang::CXXConstructorDecl>(FD) || isa<const clang::CXXDestructorDecl>(FD))
                FInfo = &CGM.getTypes().arrangeCXXStructorDeclaration(MD, clangCG::StructorType::Complete);
            else
                FInfo = &CGM.getTypes().arrangeCXXMethodDeclaration(MD);

            result.Ty = CGM.getTypes().GetFunctionType(*FInfo);
        }
        else
            result.Ty = CGM.getTypes().GetFunctionType(FD);

        llvm::Constant *GV;
        if (isa<const clang::CXXConstructorDecl>(FD) || isa<const clang::CXXDestructorDecl>(FD))
            GV = CGM.getAddrOfCXXStructor(MD, clangCG::StructorType::Complete, FInfo, result.Ty);
        else
            GV = CGM.GetAddrOfFunction(FD, result.Ty);
        result.Func = cast<llvm::Function>(GV);

        return result;
    }
};

llvm::Type *LangPlugin::toType(::Type *t)
{
    auto& Context = getASTContext();

    auto RD = getRecordDecl(t);

    if (!RD->getDefinition())
        return nullptr;
    else
        return CGM->getTypes().ConvertTypeForMem(
                    Context.getRecordType(RD));
}

llvm::FunctionType *LangPlugin::toFunctionType(::FuncDeclaration *fdecl)
{
    auto irFunc = getIrFunc(fdecl, true);

    auto FD = getFD(fdecl);
    auto Resolved = ResolvedFunc::get(*CGM, FD);

    irFunc->irFty.funcType = Resolved.Ty;
    return Resolved.Ty;
}

static llvm::Constant *buildAggrNullConstant(::AggregateDeclaration *decl,
        const IrAggr::VarInitMap& explicitInitializers)
{
    auto& Context = calypso.getASTContext();
    auto& CGM = calypso.CGM;

    auto RD = getRecordDecl(decl);

    if (!RD->getDefinition())
        return nullptr;

    auto DestType = Context.getRecordType(RD).withConst();

    // TODO: What we probably want is to have a default value if there's no constructor or a default trivial one,
    // but a null value if that's not the case i.e when context matters.
//     llvm::ArrayRef<clang::Expr*> initExprs;
//     auto ILE = new (Context) clang::InitListExpr(
//             Context, clang::SourceLocation(),
//             initExprs, clang::SourceLocation());
//     ILE->setType(DestType);
//
//     clang::Expr::EvalResult Result;
//     ILE->EvaluateAsLValue(Result, Context);

    return CGM->EmitNullConstant(DestType);  // NOTE: neither EmitConstantExpr nor EmitConstantValue will work with CXXConstructExpr
}

llvm::Constant *LangPlugin::createInitializerConstant(IrAggr *irAggr,
        const IrAggr::VarInitMap& explicitInitializers,
        llvm::StructType* initializerType)
{
    auto C = buildAggrNullConstant(irAggr->aggrdecl, explicitInitializers);  // NOTE: neither EmitConstantExpr nor EmitConstantValue will work with CXXConstructExpr

    if (!C)
        return nullptr;

    if (initializerType)
    {
        auto irSt = cast<llvm::StructType>(C->getType());
        assert(initializerType->isOpaque());
        initializerType->setBody(llvm::ArrayRef<llvm::Type*>(irSt->element_begin(),
                                irSt->element_end()),  irSt->isPacked());
    }

    // Reconstruct the constant with LDC's type, not Clang's
    // NOTE: Constant::mutateType is too dangerous because EmitNullConstant might return
    // the same constant e.g for empty records.

    if (isa<llvm::ConstantAggregateZero>(C))
        return llvm::ConstantAggregateZero::get(initializerType);
    else if (auto CS = dyn_cast<llvm::ConstantStruct>(C))
    {
        llvm::SmallVector<llvm::Constant *, 8> AggrElts;
        for (unsigned i = 0; i < initializerType->getNumElements(); i++)
            AggrElts.push_back(CS->getAggregateElement(i));
        return llvm::ConstantStruct::get(initializerType, AggrElts);
    }

    llvm_unreachable("Unhandled null constant");
}

// Required by DCXX classes, which may contain exotic fields such as class values.
void LangPlugin::addFieldInitializers(llvm::SmallVectorImpl<llvm::Constant*>& constants,
            const IrAggr::VarInitMap& explicitInitializers, ::AggregateDeclaration* decl,
            unsigned& offset, bool populateInterfacesWithVtbls)
{
    auto C = buildAggrNullConstant(decl, explicitInitializers);

    if (!C)
        return; // forward decl

    constants.push_back(C);
    offset += gDataLayout->getTypeStoreSize(C->getType());
}

void LangPlugin::buildGEPIndices(IrTypeAggr *irTyAgrr,
                                 VarGEPIndices &varGEPIndices)
{
    auto& CGTypes = CGM->getTypes();
    
    auto t = irTyAgrr->getDType();
    AggregateDeclaration *ad;
    
    if (t->ty == Tstruct)
        ad = static_cast<TypeStruct*>(t)->sym;
    else if (t->ty == Tclass)
        ad = static_cast<TypeClass*>(t)->sym;

    auto RD = getRecordDecl(ad);
    assert(RD->getDefinition());
    
    auto& CGRL = CGTypes.getCGRecordLayout(RD);
    
    for (auto vd: ad->fields)
    {
        auto VD = static_cast<cpp::VarDeclaration*>(vd)->VD;
        varGEPIndices[vd] = CGRL.getLLVMFieldNo(llvm::cast<clang::FieldDecl>(VD));
    }
}

void LangPlugin::toInitClass(TypeClass* tc, LLValue* dst)
{
    auto& Context = getASTContext();

    uint64_t const dataBytes = tc->sym->structsize;
    if (dataBytes == 0)
        return;

    LLValue* initsym = getIrAggr(tc->sym)->getInitSymbol();
    initsym = DtoBitCast(initsym, DtoType(tc));

    DtoMemCpy(dst, initsym, DtoConstSize_t(dataBytes));
}

LLValue *LangPlugin::toVirtualFunctionPointer(DValue* inst, 
                                              ::FuncDeclaration* fdecl, char* name)
{
    auto MD = cast<clang::CXXMethodDecl>(getFD(fdecl));
    LLValue* vthis = inst->getRVal();
    auto Ty = toFunctionType(fdecl);
    
    return CGM->getCXXABI().getVirtualFunctionPointer(*CGF(), MD, vthis, Ty);
}

static const clangCG::CGFunctionInfo &arrangeFunctionCall(
                    clangCG::CodeGenModule *CGM,
                    const clang::FunctionDecl *FD,
                    clang::CodeGen::CallArgList &Args)
{
    auto FPT = FD->getType()->castAs<clang::FunctionProtoType>();
    auto MD = llvm::dyn_cast<const clang::CXXMethodDecl>(FD);

    if (MD && !MD->isStatic())
    {
        clangCG::RequiredArgs required =
            clangCG::RequiredArgs::forPrototypePlus(FPT, Args.size());

        return CGM->getTypes().arrangeCXXMethodCall(Args, FPT, required);
    }
    else
        return CGM->getTypes().arrangeFreeFunctionCall(Args, FPT, false);
}

DValue* LangPlugin::toCallFunction(Loc& loc, Type* resulttype, DValue* fnval, 
                                   Expressions* arguments, llvm::Value *retvar)
{
    updateCGFInsertPoint();
    
    DFuncValue* dfnval = fnval->isFunc();
    auto fd = dfnval->func;

    // get function type info
    IrFuncTy &irFty = DtoIrTypeFunction(fnval);
    TypeFunction* tf = DtoTypeFunction(fnval);

    // get callee llvm value
    LLValue* callable = DtoCallableValue(fnval);
    LLFunctionType* callableTy = DtoExtractFunctionType(callable->getType());
    assert(callableTy);

    clangCG::RValue RV;
    clangCG::CallArgList Args;

    auto FD = getFD(fd);
    auto MD = llvm::dyn_cast<const clang::CXXMethodDecl>(FD);

    auto This = MD ? dfnval->vthis : nullptr;

    const clang::FunctionProtoType *FPT = FD->getType()->castAs<clang::FunctionProtoType>();

    auto Dtor = llvm::dyn_cast<clang::CXXDestructorDecl>(FD);
    if (Dtor && Dtor->isVirtual())
    {
        CGM->getCXXABI().EmitVirtualDestructorCall(*CGF(), Dtor, clang::Dtor_Complete,
                                        This, nullptr);

        return new DImValue(nullptr, nullptr); // WARNING ldc never does that, it returns the instruction of the call site instead
    }

    // Push the this ptr.
    if (MD && !MD->isStatic())
    {
        if (MD->isVirtual()) {
            This = CGM->getCXXABI().adjustThisArgumentForVirtualFunctionCall(
                *CGF(), MD, This, true);
        }

        Args.add(clangCG::RValue::get(This),
                 MD->getThisType(getASTContext()));
    }

    size_t n = Parameter::dim(tf->parameters);
    for (size_t i=0; i<n; ++i) {
        Parameter* fnarg = Parameter::getNth(tf->parameters, i);
        assert(fnarg);
        DValue* argval = DtoArgument(fnarg, arguments->data[i]);

        auto ArgTy = TypeMapper().toType(loc, fnarg->type,
                                        fd->scope, fnarg->storageClass);
        if (fnarg->type->ty == Tvalueof)
        {
//             llvm::Value *tmp = CGF()->CreateMemTemp(type);
//             CGF()->EmitAggregateCopy(tmp, L.getAddress(), type, /*IsVolatile*/false,
//                                 L.getAlignment());
//             Args.add(clangCG::RValue::getAggregate(tmp), type);
            Args.add(clangCG::RValue::getAggregate(argval->getRVal()),
                     ArgTy, /*NeedsCopy*/true);
        }
        else
            Args.add(clangCG::RValue::get(argval->getRVal()), ArgTy);
    }

//     clangCG::ReturnValueSlot ReturnValue(retvar, false);
    assert(!retvar); // since LDC doesn't know we're using sret for C++ calls
            // we could make it aware though (TODO)
    
    auto &FInfo = arrangeFunctionCall(CGM.get(), FD, Args);
    RV = CGF()->EmitCall(FInfo, callable, clangCG::ReturnValueSlot(), Args, FD);

    if (tf->isref)
    {
        assert(RV.isScalar());
        return new DVarValue(resulttype, RV.getScalarVal());
    }

    if (RV.isScalar())
        return new DImValue(resulttype, RV.getScalarVal());
    else if (RV.isAggregate())
        return new DVarValue(resulttype, RV.getAggregateAddr());

    llvm_unreachable("Complex RValue FIXME");
}

void LangPlugin::toResolveFunction(::FuncDeclaration* fdecl)
{
    auto FD = getFD(fdecl);

    auto irFunc = getIrFunc(fdecl, true);
    auto &irFty = irFunc->irFty;

    fdecl->ir.setDeclared();

    auto resolved = ResolvedFunc::get(*CGM, FD);
    irFunc->func = resolved.Func;
    irFty.funcType = resolved.Ty;
}

class DiscardableODREmitter : public clang::RecursiveASTVisitor<DiscardableODREmitter>
{
    clang::ASTContext &Context;
    clangCG::CodeGenModule &CGM;

    llvm::DenseSet<const clang::FunctionDecl *> Emitted;
public:
    DiscardableODREmitter(clang::ASTContext &Context,
                        clangCG::CodeGenModule &CGM) : Context(Context), CGM(CGM) {}
    bool VisitCallExpr(const clang::CallExpr *E);
};

bool DiscardableODREmitter::VisitCallExpr(const clang::CallExpr *E)
{
    auto Callee = E->getDirectCallee();
    const clang::FunctionDecl *Def;

    if (!Callee || !Callee->hasBody(Def))
        return true;

    auto FPT = Callee->getType()->getAs<clang::FunctionProtoType>();
    if (FPT->getExceptionSpecType() == clang::EST_Unevaluated)
        return true;

    auto resolved = ResolvedFunc::get(CGM, Callee);

    if (Context.GetGVALinkageForFunction(Callee) != clang::GVA_DiscardableODR || Emitted.count(Def))
        return true;

    Emitted.insert(Def);

    if (resolved.Func->isDeclaration())
        CGM.EmitTopLevelDecl(const_cast<clang::FunctionDecl*>(Def));

    TraverseStmt(Def->getBody());
    return true;
}

void LangPlugin::toDefineFunction(::FuncDeclaration* fdecl)
{
    auto& Context = getASTContext();

    auto FD = getFD(fdecl);
    const clang::FunctionDecl *Def;

    if (FD->hasBody(Def) && getIrFunc(fdecl)->func->isDeclaration())
    {
        CGM->EmitTopLevelDecl(const_cast<clang::FunctionDecl*>(Def)); // TODO remove const_cast

        // Emit inline functions this function depends upon
        DiscardableODREmitter(Context, *CGM).TraverseStmt(Def->getBody());
    }
}

void LangPlugin::addBaseClassData(AggrTypeBuilder &b, ::AggregateDeclaration *base)
{
    const clang::RecordDecl *RD;
    if (base->isClassDeclaration())
        RD = static_cast<cpp::ClassDeclaration*>(base)->RD;
    else
        RD  = static_cast<cpp::StructDeclaration*>(base)->RD;

    auto& CGRL = CGM->getTypes().getCGRecordLayout(RD);

    auto BaseTy = CGRL.getBaseSubobjectLLVMType();
    auto BaseLayout = gDataLayout->getStructLayout(BaseTy);

    for (auto I = BaseTy->element_begin(), E = BaseTy->element_end();
         I != E; ++I)
    {
        b.m_defaultTypes.push_back(*I);
        ++b.m_fieldIndex;
    }

    for (auto vd: base->fields)
    {
        auto VD = static_cast<cpp::VarDeclaration*>(vd)->VD;
        b.m_varGEPIndices[vd] = CGRL.getLLVMFieldNo(llvm::cast<clang::FieldDecl>(VD));
    }

    b.m_offset += BaseLayout->getSizeInBytes();
}

void LangPlugin::toDeclareVariable(::VarDeclaration* vd)
{
    auto VD = llvm::cast<clang::VarDecl>(
            static_cast<cpp::VarDeclaration*>(vd)->VD);

//     updateCGFInsertPoint();

    LLValue *v;

    // If it's thread_local, emit a call to its wrapper function instead.
//     if (VD->getTLSKind() == clang::VarDecl::TLS_Dynamic)
//         v = CGM.getCXXABI().EmitThreadLocalVarDeclLValue(*CGF, VD, VD->getType()).getAddress();
//     else
        v = CGM->GetAddrOfGlobalVar(VD);

    getIrGlobal(vd)->value = v;
}

void LangPlugin::toDefineVariable(::VarDeclaration* vd)
{
    // FIXME: initialize static variables from template instantiations
}

void LangPlugin::toDefineTemplateInstance(::TemplateInstance *inst)
{
    auto& Context = getASTContext();

    auto c_ti = static_cast<cpp::TemplateInstance *>(inst);

    if (auto CTSD = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(c_ti->Inst))
        CGM->UpdateCompletedType(CTSD);

    for (auto D: c_ti->Dependencies)
        CGM->EmitTopLevelDecl(D);
}

}
