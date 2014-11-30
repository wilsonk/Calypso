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

FuncDeclaration::FuncDeclaration(Loc loc, Identifier *id, StorageClass storage_class,
                    Type* type, const clang::FunctionDecl *FD)
    : ::FuncDeclaration(loc, loc, id, storage_class, type)
{
    this->FD = FD;
}

CtorDeclaration::CtorDeclaration(Loc loc, StorageClass storage_class,
                                 Type* type, const clang::CXXConstructorDecl* CCD)
    : ::CtorDeclaration(loc, loc, storage_class, type)
{
    this->CCD = CCD;
}

DtorDeclaration::DtorDeclaration(Loc loc, StorageClass storage_class,
                                 Identifier* id, const clang::CXXDestructorDecl* CDD)
    : ::DtorDeclaration(loc, loc, storage_class, id)
{
    this->CDD = CDD;
}

}
