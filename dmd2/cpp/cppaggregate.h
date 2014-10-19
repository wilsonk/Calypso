// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPAGGREGATE_H
#define DMD_CPP_CPPAGGREGATE_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root.h"
#include "calypso.h"
#include "../aggregate.h"

namespace clang
{
class RecordDecl;
class CXXRecordDecl; // NOTE: will disappear in a future version of Clang
}

namespace cpp
{

// All POD C++ aggregate types, it doesn't matter whether "struct" or "class"
// was used and whether another aggregate inherit from it
struct StructDeclaration : ::StructDeclaration
{
    CALYPSO_LANGPLUGIN

    const clang::RecordDecl *RD;

    StructDeclaration(Loc loc, Identifier *id,
                      const clang::RecordDecl *RD);
};

// The rest, i.e anything involving inheritance, virtual functions.
struct ClassDeclaration : ::ClassDeclaration
{
    CALYPSO_LANGPLUGIN

    const clang::CXXRecordDecl *RD;

    ClassDeclaration(Loc loc, Identifier *id, BaseClasses *baseclasses,
                     const clang::CXXRecordDecl *RD);
};

}

#endif /* DMD_CPP_CPPAGGREGATE_H */
