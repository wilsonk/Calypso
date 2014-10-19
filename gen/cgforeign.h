#pragma once

#include "llvm.h"
// #include "llvm/IR/BasicBlock.h"
// #include "llvm/IR/Type.h"
#include "../dmd2/declaration.h"

#include "ir/iraggr.h"

class Type;
class VarDeclaration;

class CodeGen
{
public:
    virtual void enterModule(llvm::Module *m) = 0;

//     virtual void SetInsertPoint(llvm::BasicBlock *TheBB) = 0;
    virtual void enterFunc(FuncDeclaration *fd) = 0;
    virtual void leaveFunc() = 0;

    virtual llvm::Type *toType(Type *t) = 0;
    virtual llvm::Constant *createInitializerConstant(IrAggr *irAggr,
        const IrAggr::VarInitMap& explicitInitializers,
        llvm::StructType* initializerType = 0) = 0;
};
