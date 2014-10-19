// Contributed by Elie Morisse, same license DMD uses

#include "calypso.h"
#include "cppdeclaration.h"

#include "clang/AST/Decl.h"

namespace cpp
{

VarDeclaration::VarDeclaration(Loc loc, Identifier *id,
                               const clang::ValueDecl *VD, Type *t)
    : ::VarDeclaration(loc, t, id, nullptr)
{
    this->VD = VD;
}

FuncDeclaration::FuncDeclaration(Loc loc, Identifier *id,
                                 const clang::FunctionDecl *FD, TypeFunction *ft)
    : ::FuncDeclaration(loc, loc, id, STCundefined, ft)
{
    this->FD = FD;
}


}
