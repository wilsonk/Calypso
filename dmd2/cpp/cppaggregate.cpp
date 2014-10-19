// Contributed by Elie Morisse, same license DMD uses

#include "cppaggregate.h"

#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"

namespace cpp
{

StructDeclaration::StructDeclaration(Loc loc, Identifier *id,
                                     const clang::RecordDecl *RD)
    : ::StructDeclaration(loc, id)
{
    this->RD = RD;
}

ClassDeclaration::ClassDeclaration(Loc loc, Identifier *id, BaseClasses *baseclasses,
                                   const clang::CXXRecordDecl *RD)
    : ::ClassDeclaration(loc, id, baseclasses)
{
    this->RD = RD;
}

}
