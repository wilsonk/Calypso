// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPDECLARATION_H
#define DMD_CPP_CPPDECLARATION_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root.h"
#include "dsymbol.h"

#include "../declaration.h"

namespace clang
{
    class ValueDecl;
    class FunctionDecl;
    class FunctionProtoType;
}

namespace cpp
{

class VarDeclaration : public ::VarDeclaration
{
public:
    const clang::ValueDecl *VD;

    VarDeclaration(Loc loc, Identifier *id, const clang::ValueDecl *VD, Type *t);
};

class FuncDeclaration : public ::FuncDeclaration
{
public:
    const clang::FunctionDecl *FD;

    FuncDeclaration(Loc loc, Identifier *id, const clang::FunctionDecl *FD, TypeFunction *ft);

#if IN_LLVM
    void toResolveFunction();
#endif
};

}

#endif /* DMD_CPP_CPPDECLARATION_H */
