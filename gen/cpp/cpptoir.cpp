// Contributed by Elie Morisse, same license DMD uses
#include "cpp/calypso.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppaggregate.h"

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
#include "clang/AST/Expr.h"
#include "clang/Basic/ABI.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Frontend/ASTUnit.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include <memory>

//////////////////////////////////////////////////////////////////////////////////////////

namespace cpp
{

void LangPlugin::enterModule(llvm::Module *m)
{
    if (!getASTUnit())
        return;

    auto& Context = getASTContext();

    clang::CodeGenOptions CGO;
    AB.reset(new AssistBuilder(*pch.Diags, m,
                                  CGO, llvm::getGlobalContext()));
    AB->Initialize(Context);
}

void LangPlugin::leaveModule()
{
    if (!getASTUnit())
        return;

    auto& Context = getASTContext();
    AB->HandleTranslationUnit(Context);
}

void LangPlugin::enterFunc(::FuncDeclaration *fd)
{//FIXME nested
    if (!getASTUnit())
        return;
    
    auto& CGM = *(AB->CGM());
    CGF = new clang::CodeGen::CodeGenFunction(CGM, true);
}

void LangPlugin::leaveFunc()
{
    if (!getASTUnit())
        return;
    
    delete CGF;
    CGF = nullptr;
}

void LangPlugin::updateCGFInsertPoint()
{
    auto BB = gIR->scope().begin;
    CGF->Builder.SetInsertPoint(BB);
}

llvm::Type *LangPlugin::toType(::Type *t)
{
    auto CGM = AB->CGM();
    auto& Context = getASTContext();

    switch(t->ty)
    {
        case Tstruct:
        {
            auto ts = static_cast<TypeStruct*>(t);
            auto c_sd = static_cast<StructDeclaration*>(ts->sym);
            return CGM->getTypes().ConvertTypeForMem(
                Context.getRecordType(c_sd->RD));
        }
        case Tclass:
        {
            auto tc = static_cast<TypeClass*>(t);
            auto c_cd = static_cast<ClassDeclaration*>(tc->sym);
            return CGM->getTypes().ConvertTypeForMem(
                Context.getRecordType(c_cd->RD));
        }
        default:
            assert(false && "Unhandled");
    }

    return nullptr;
}

llvm::Constant *LangPlugin::createInitializerConstant(IrAggr *irAggr,
        const IrAggr::VarInitMap& explicitInitializers,
        llvm::StructType* initializerType)
{
    auto& Context = getASTContext();
    auto CGM = AB->CGM();

    const clang::RecordDecl *RD;
    if (auto sd = irAggr->aggrdecl->isStructDeclaration())
        RD = static_cast<StructDeclaration*>(sd)->RD;
    else if (auto cd = irAggr->aggrdecl->isClassDeclaration())
        RD = static_cast<ClassDeclaration*>(cd)->RD;
     else
        assert(false && "Unknown aggregate decl type?");

    auto DestType = Context.getRecordType(RD).withConst();
    llvm::Constant *c;

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

    c = CGM->EmitNullConstant(DestType);  // NOTE: neither EmitConstantExpr nor EmitConstantValue will work with CXXConstructExpr

    auto irSt = llvm::cast<llvm::StructType>(c->getType());

    if (initializerType)
    {
        assert(initializerType->isOpaque());
        initializerType->setBody(llvm::ArrayRef<llvm::Type*>(irSt->element_begin(),
                                                             irSt->element_end()),
                                                             irSt->isPacked());
    }
    
    c->mutateType(initializerType);  // dangerous?
    return c;
}

void LangPlugin::buildGEPIndices(IrTypeAggr *irTyAgrr,
                                 VarGEPIndices &varGEPIndices)
{
    auto& CGM = *AB->CGM();
    auto& CGTypes = CGM.getTypes();
    
    auto t = irTyAgrr->getDType();
    AggregateDeclaration *ad;
    const clang::RecordDecl *RD;
    
    if (t->ty == Tstruct)
    {
        auto ts = static_cast<TypeStruct*>(t);
        ad = ts->sym;
        RD = static_cast<cpp::StructDeclaration*>(ts->sym)->RD;
    }
    else if (t->ty == Tclass)
    {
        auto tc = static_cast<TypeClass*>(t);
        ad = tc->sym;
        RD = static_cast<cpp::ClassDeclaration*>(tc->sym)->RD;
    }
    
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
    auto& CGM = *AB->CGM();

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
    auto& CGM = *AB->CGM();
    auto c_fd = static_cast<FuncDeclaration*>(fdecl);
    
    auto MD = llvm::cast<const clang::CXXMethodDecl>(c_fd->FD);

    // get instance
    LLValue* vthis = inst->getRVal();

    const clang::CodeGen::CGFunctionInfo *FInfo = nullptr;
    if (const clang::CXXDestructorDecl *Dtor = llvm::dyn_cast<clang::CXXDestructorDecl>(MD))
        FInfo = &CGM.getTypes().arrangeCXXDestructor(Dtor,
                                                    clang::Dtor_Complete);
    else if (const clang::CXXConstructorDecl *Ctor = llvm::dyn_cast<clang::CXXConstructorDecl>(MD))
        FInfo = &CGM.getTypes().arrangeCXXConstructorDeclaration(Ctor,
                                                                clang::Ctor_Complete);
    else
        FInfo = &CGM.getTypes().arrangeCXXMethodDeclaration(MD);
  
    llvm::FunctionType *Ty = CGM.getTypes().GetFunctionType(*FInfo);
    
    return CGM.getCXXABI().getVirtualFunctionPointer(*CGF, MD, vthis, Ty);
}

DValue* LangPlugin::toCallFunction(Loc& loc, Type* resulttype, DValue* fnval, 
                                   Expressions* arguments, llvm::Value *retvar)
{
    auto& CGM = *AB->CGM();

    updateCGFInsertPoint();
    
    DFuncValue* dfnval = fnval->isFunc();
    auto c_fd = static_cast<FuncDeclaration*>(dfnval->func);

    // get function type info
    IrFuncTy &irFty = DtoIrTypeFunction(fnval);
    TypeFunction* tf = DtoTypeFunction(fnval);

    // get callee llvm value
    LLValue* callable = DtoCallableValue(fnval);
    LLFunctionType* callableTy = DtoExtractFunctionType(callable->getType());
    assert(callableTy);

    clang::CodeGen::RValue RV;
    clang::CodeGen::ReturnValueSlot ReturnValue(retvar, false);
    clang::CodeGen::CallArgList Args;

    auto FD = c_fd->FD;
    auto MD = llvm::dyn_cast<const clang::CXXMethodDecl>(FD);

    auto This = MD ? dfnval->vthis : nullptr;

    const clang::FunctionProtoType *FPT = FD->getType()->castAs<clang::FunctionProtoType>();

    auto Dtor = llvm::dyn_cast<clang::CXXDestructorDecl>(FD);
    if (Dtor && Dtor->isVirtual())
    {
        CGM.getCXXABI().EmitVirtualDestructorCall(*CGF, Dtor, clang::Dtor_Complete,
                                        clang::SourceLocation(), This);

        return new DImValue(nullptr, nullptr); // WARNING ldc never does that, it returns the instruction of the call site instead
    }

    // Push the this ptr.
    if (MD && !MD->isStatic())
    {
        if (MD->isVirtual()) {
            This = CGM.getCXXABI().adjustThisArgumentForVirtualFunctionCall(
                *CGF, MD, This, true);
        }

        Args.add(clang::CodeGen::RValue::get(This),
                 MD->getThisType(getASTContext()));
    }

    size_t n = Parameter::dim(tf->parameters);
    for (size_t i=0; i<n; ++i) {
        Parameter* fnarg = Parameter::getNth(tf->parameters, i);
        assert(fnarg);
        DValue* argval = DtoArgument(fnarg,
                        static_cast<::Expression*>(arguments->data[i]));
        Args.add(clang::CodeGen::RValue::get(argval->getRVal()),
                 fromType(fnarg->type));
    }
    
    if (MD && !MD->isStatic())
    {
        clang::CodeGen::RequiredArgs required =
            clang::CodeGen::RequiredArgs::forPrototypePlus(FPT, Args.size());
        
        RV = CGF->EmitCall(CGM.getTypes().arrangeCXXMethodCall(Args, FPT, required),
                    callable, ReturnValue, Args, MD);
    }
    else
    {
        RV = CGF->EmitCall(CGM.getTypes().arrangeFreeFunctionCall(Args, FPT),
                    callable, ReturnValue, Args, FD);
    }

    return new DImValue(resulttype, RV.getScalarVal());
}

// IMPORTANT NOTE: Clang emits forward decls lazily, which means that no function prototype is actually emitted to LLVM after making a CodeGenerator consume the ASTContext.
// One reason for that is that types need to be complete for a prototype to be emitted (this used to cause obscure segfaults on my language Tales a while back).
// This is why we must keep access to the CodeGenModule so that we can emit them and then copy the LLVM infos.
// (as a side not similarly LLVM lazily puts aggregate types into its context type map as well, but that doesn't matter here afaik).

void LangPlugin::toResolveFunction(::FuncDeclaration* fdecl)
{
    auto CGM = AB->CGM();
    auto FD = static_cast<FuncDeclaration*>(fdecl)->FD;

    DtoFunctionType(fdecl);

    fdecl->ir.setDeclared();

    llvm::Constant *Callee = nullptr;
    const clang::CodeGen::CGFunctionInfo *FInfo;
    llvm::FunctionType *Ty;

    if (auto MD = llvm::dyn_cast<const clang::CXXMethodDecl>(FD))
    {
        if (auto Dtor = llvm::dyn_cast<const clang::CXXDestructorDecl>(MD))
            FInfo = &CGM->getTypes().arrangeCXXDestructor(Dtor,
                                                        clang::Dtor_Complete);
        else if (auto Ctor = llvm::dyn_cast<const clang::CXXConstructorDecl>(MD))
            FInfo = &CGM->getTypes().arrangeCXXConstructorDeclaration(Ctor,
                                                                    clang::Ctor_Complete);
        else
            FInfo = &CGM->getTypes().arrangeCXXMethodDeclaration(MD);

        Ty = CGM->getTypes().GetFunctionType(*FInfo);
    }
    else
        Ty = CGM->getTypes().GetFunctionType(FD);


    if (auto Dtor = llvm::dyn_cast<const clang::CXXDestructorDecl>(FD))
        Callee = CGM->GetAddrOfCXXDestructor(Dtor, clang::Dtor_Complete,
                                                FInfo, Ty);
    else if (auto Ctor = llvm::dyn_cast<const clang::CXXConstructorDecl>(FD))
        Callee = CGM->GetAddrOfFunction(
            clang::GlobalDecl(Ctor, clang::Ctor_Complete), Ty);
    else
        Callee = CGM->GetAddrOfFunction(FD, Ty);

    auto irFunc = getIrFunc(fdecl, true);
    irFunc->func = llvm::cast<llvm::Function>(Callee);
}

void LangPlugin::addBaseClassData(AggrTypeBuilder &b, ::ClassDeclaration *base)
{
    auto& CGM = *AB->CGM();

    auto RD = static_cast<cpp::ClassDeclaration*>(base)->RD;
    auto& CGRL = CGM.getTypes().getCGRecordLayout(RD);

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

// A D class derived from a C++ one overriding virtual C++ methods must,
// after the last C++ ctor call, make the C++ vptrs point to a modified vtable.
// The trick is to make Clang recreate a VTable initializer for the most derived C++ base
// and then hack on it by replacing the thunk addresses with new ones.

struct DCXXVTableInfo // "DCXX" for D-C++ class "hybrid"
{
    bool isDCXX = false;

    cpp::ClassDeclaration *mostDerivedCXXBase;
    const clang::CXXRecordDecl* MostDerivedBase;
    const clang::VTableLayout *VTLayout;
    llvm::ArrayType *VTArrayType;

    static DCXXVTableInfo *get(::ClassDeclaration *cd)
    {
        auto result = new DCXXVTableInfo;

        auto& Context = calypso.getASTContext();
        assert(!Context.getVTableContext()->isMicrosoft() &&
                "Microsoft VTableContext not supported yet");

        if (isCPP(cd))
            return result;  // Pure C++ class

        auto cd2 = cd->baseClass;
        while (cd2 && !isCPP(cd2))
            cd2 = cd2->baseClass;
        if (!cd2)
            return result;  // Pure D class

        result->isDCXX = true;

        auto& CGM = *calypso.AB->CGM();
        auto& CGVT = CGM.getVTables();
        auto& VTableContext =
            *static_cast<clang::ItaniumVTableContext *>(Context.getVTableContext());

        result->mostDerivedCXXBase = static_cast<cpp::ClassDeclaration *>(cd2);
        result->MostDerivedBase = result->mostDerivedCXXBase->RD;

        result->VTLayout =
            &VTableContext.getVTableLayout(result->MostDerivedBase);
        result->VTArrayType =
            llvm::ArrayType::get(CGM.Int8PtrTy,
                                 result->VTLayout->getNumVTableComponents());

        return result;
    }
};

static llvm::GlobalVariable *getDCXXVTable(::ClassDeclaration *cd,
                                           DCXXVTableInfo *dcxxInfo = nullptr)
{
    std::string initname("_D");
    initname.append(mangle(cd));
    initname.append("9__VtblCXXZ");

    if (!dcxxInfo)
        dcxxInfo = DCXXVTableInfo::get(cd);

    assert(dcxxInfo->isDCXX);

    auto dcxxVTable = getOrCreateGlobal(cd->loc,
        *gIR->module, dcxxInfo->VTArrayType, false,
        llvm::GlobalValue::ExternalLinkage, NULL, initname);

    return dcxxVTable;
}

static void mangleNumber(llvm::raw_ostream &Out,
                         int64_t Number)
{
  //  <number> ::= [n] <non-negative decimal integer>
  if (Number < 0) {
    Out << 'n';
    Number = -Number;
  }

  Out << Number;
}

// HACK? We're taking advantage of DMD semantic caps to generate C++ thunk-like functions
// except at codegen time! Since they are final they shouldn't affect the aggregate they're member of, but still...
//
// Pro: this avoids redundant "manual" function code generation (even though thunks are simple)
// Con: Clang's way of doing things avoids meddling with the AST, doesn't have any hard-to-foresee consequence
// hmm calling conventions
::FuncDeclaration *getDCXXThunk(::FuncDeclaration *callee,
                             const clang::ThunkInfo &Thunk)
{
    auto loc = callee->loc;

    llvm::SmallString<256> thunkName;
    llvm::raw_svector_ostream Out(thunkName);

    Out << "_DCXT";
    mangleNumber(Out, Thunk.This.NonVirtual);
    Out << '_';
    Out << callee->ident->toChars();
    Out.flush();

    auto thunkId = Lexer::idPool(thunkName.c_str());

    auto agg = callee->isThis();
    if (auto s = agg->search(loc, thunkId))
        return static_cast<::FuncDeclaration*>(s);

    auto calleeTf = static_cast<::TypeFunction*>(callee->getType());
    Type *tf = new TypeFunction(calleeTf->parameters,
                                calleeTf->next, 0, LINKd, STCfinal);

    auto fthunk = new ::FuncDeclaration(loc, loc,
                    thunkId, STCfinal, tf);

    Expression *e = new DotVarExp(loc, new ThisExp(loc), callee);
    e = new CallExp(loc, e);
    fthunk->fbody = new ReturnStatement(loc, e);

//     fthunk->importAll(callee->scope);
    fthunk->semantic(callee->scope);
    fthunk->semantic2(callee->scope);
    fthunk->semantic3(callee->scope);
    Declaration_codegen(fthunk);

    return fthunk;
}

// Emit the modified vtbl for the most derived C++ class
void LangPlugin::emitAdditionalClassSymbols(::ClassDeclaration *cd)
{
    auto dcxxInfo = DCXXVTableInfo::get(cd);
    if (!dcxxInfo->isDCXX)
        return;

    auto VTLayout = dcxxInfo->VTLayout;

    auto& Context = getASTContext();
    auto& CGM = *AB->CGM();
    auto& CGVT = CGM.getVTables();

    auto RTTI = CGM.GetAddrOfRTTIDescriptor(
        Context.getTagDeclType(dcxxInfo->MostDerivedBase));

    auto OldVTableInit = CGVT.CreateVTableInitializer(
      dcxxInfo->MostDerivedBase, VTLayout->vtable_component_begin(),
      VTLayout->getNumVTableComponents(), VTLayout->vtable_thunk_begin(),
      VTLayout->getNumVTableThunks(), RTTI);

    // Copy the operands into a new array
    // NOTE: replaceUsesOfWithOnConstant isn't suitable because it might replace
    // the C++ class vtable by the DCXX one.
    std::vector<llvm::Constant*> Inits(OldVTableInit->getNumOperands());
    for (unsigned i = 0; i < OldVTableInit->getNumOperands(); i++)
        Inits[i] = llvm::cast<llvm::Constant>(OldVTableInit->getOperand(i));  // std::copy fails because the cast is required

    // search for virtual C++ methods overriden by the D class
    for (auto s: cd->vtbl)
    {
        auto md = s->isFuncDeclaration();
        if (!md)
            continue;

        cpp::FuncDeclaration *cxxmd = nullptr;
        for (auto s2: dcxxInfo->mostDerivedCXXBase->vtbl)
        {
            auto md2 = s2->isFuncDeclaration();
            if (!md2)
                continue;

            for (auto overmd: md->foverrides) {
                if (overmd == md2) {
                    cxxmd = static_cast<cpp::FuncDeclaration*>(md2);
                    break;
                }
            }

            if (cxxmd)
                break;
        }

        if (!cxxmd)
            continue;   // md isn't overriding a C++ method

        auto MD = llvm::cast<clang::CXXMethodDecl>(
            cxxmd->FD->getCanonicalDecl());

        // Replace calls to MD with calls to md properly adjusted by thunks
        auto Components = VTLayout->vtable_component_begin();
        for (unsigned I = 0; I != VTLayout->getNumVTableComponents(); ++I)
        {
            auto& Component = Components[I];

            if (Component.getKind() !=
                    clang::VTableComponent::CK_FunctionPointer)
                continue;

            if (Component.getFunctionDecl()->getCanonicalDecl() != MD)
                continue;

            clang::ThunkInfo NewThunk;
            NewThunk.This.NonVirtual = -2 * Target::ptrsize;
            NewThunk.Return.NonVirtual = 2 * Target::ptrsize;

            for (auto T = VTLayout->vtable_thunk_begin(),
                    TE = VTLayout->vtable_thunk_begin(); T != TE; T++)
            {
                if (T->first != I)
                    continue;

                auto& OldThunk = T->second;
                NewThunk.This.NonVirtual += OldThunk.This.NonVirtual;
                NewThunk.Return.NonVirtual += OldThunk.Return.NonVirtual;
            }

            auto thunkFd = getDCXXThunk(md, NewThunk);
            Inits[I] = getIrFunc(thunkFd)->func;
        }
    }

    auto VTableInit = llvm::ConstantArray::get(dcxxInfo->VTArrayType, Inits);

    auto linkage = DtoLinkage(cd);
    auto vtableZ = getDCXXVTable(cd, dcxxInfo);
    vtableZ->setInitializer(VTableInit);
    vtableZ->setLinkage(linkage);
}

}
