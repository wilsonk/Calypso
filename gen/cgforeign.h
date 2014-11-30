#pragma once

#include "llvm.h"
#include "../dmd2/declaration.h"

#include "ir/iraggr.h"
#include "ir/irtypeaggr.h"

class Type;
class VarDeclaration;

class CodeGen
{
public:
    virtual void enterModule(llvm::Module *m) = 0;
    virtual void leaveModule() = 0;

//     virtual void SetInsertPoint(llvm::BasicBlock *TheBB) = 0;
    virtual void enterFunc(FuncDeclaration *fd) = 0;
    virtual void leaveFunc() = 0;

    virtual llvm::Type *toType(Type *t) = 0;

    virtual llvm::Constant *createInitializerConstant(IrAggr *irAggr,
        const IrAggr::VarInitMap& explicitInitializers,
        llvm::StructType* initializerType = 0) = 0;
        
    virtual void buildGEPIndices(IrTypeAggr *irTyAgrr, VarGEPIndices &varGEPIndices) = 0;
        
    virtual void toInitClass(TypeClass* tc, LLValue* dst) = 0;
    
    virtual LLValue *toVirtualFunctionPointer(DValue* inst, FuncDeclaration* fdecl, char* name) = 0;
    
    virtual DValue* toCallFunction(Loc& loc, Type* resulttype, DValue* fnval, Expressions* arguments, llvm::Value *retvar) = 0;

    virtual void toResolveFunction(FuncDeclaration* fdecl) = 0;

    virtual void addBaseClassData(AggrTypeBuilder &builder, ClassDeclaration *base) = 0;

    virtual void emitAdditionalClassSymbols(ClassDeclaration *cd) = 0;
};
