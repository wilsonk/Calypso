// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPIMPORT_H
#define DMD_CPP_CPPIMPORT_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root.h"
#include "dsymbol.h"
#include "cpp/calypso.h"

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
    CALYPSO_LANGPLUGIN

    bool fromCpp;

    Import(Loc loc, Identifiers *packages, Identifier *id, Identifier *aliasId,
        int isstatic, bool fromCpp = true);
    void load(Scope *sc) override;

    ::Module *loadModule(Loc loc, Identifiers *packages, Identifier *id);
};

struct Modmap : public ::Modmap
{
public:
    CALYPSO_LANGPLUGIN

    Modmap(Loc loc, StringExp *arg);

    void importAll(Scope *sc);
    void semantic(Scope *sc);
};

}

#endif /* DMD_CPP_CPPIMPORT_H */
