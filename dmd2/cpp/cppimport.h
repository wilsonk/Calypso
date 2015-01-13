// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPIMPORT_H
#define DMD_CPP_CPPIMPORT_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root.h"
#include "dsymbol.h"

#include "../import.h"

namespace clang
{
    class TranslationUnitDecl;
    class ASTUnit;
}

namespace cpp {

using clang::ASTUnit;

class Import : public ::Import
{
public:
    Import(Loc loc, Identifiers *packages, Identifier *id, Identifier *aliasId,
        int isstatic);

    ::Module *loadModule(Loc loc, Identifiers *packages, Identifier *id);
};

struct Modmap : public ::Modmap
{
public:
    Modmap(Loc loc, StringExp *arg);

    void importAll(Scope *sc);
    void semantic(Scope *sc);
};

}

#endif /* DMD_CPP_CPPIMPORT_H */
