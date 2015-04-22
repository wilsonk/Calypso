// Contributed by Elie Morisse, same license DMD uses

#include "cpp/astunit.h"
#include "cpp/cppimport.h"
#include "cpp/cppmodule.h"
#include "cpp/calypso.h"
#include "expression.h"

#include "llvm/Support/FileSystem.h" // shall be replaced by std.file
#include "clang/Basic/Diagnostic.h"

namespace cpp
{

Import::Import(Loc loc, Identifiers *packages, Identifier *id, Identifier *aliasId,
        int isstatic, bool fromCpp)
    : ::Import(loc, packages, id, aliasId, isstatic)
{
    // add "cpp" as leftmost package to avoid name clashes
    if (!this->packages)
        this->packages = new Identifiers;
    this->packages->shift(Lexer::idPool("cpp"));  // any better idea ?

    this->fromCpp = fromCpp;
    if (!fromCpp) // do not change the ident to "cpp" if the import is inside a C++ module as this makes symbol mapping a lot more complicated
        setSymIdent();
}

void Import::load(Scope *sc)
{
    ::Import::load(sc);

    if (fromCpp)
    {
        auto leftmost = packages->dim > 1 ? (*packages)[1] : id;
        pkg = static_cast<Package*>(
                pkg->symtab->lookup(leftmost));
        assert(pkg);
    }
}

::Module* Import::loadModule(Loc loc, Identifiers* packages, Identifier* id)
{
    calypso.pch.update();
    
    return Module::load(loc, packages, id);
}

Modmap::Modmap(Loc loc, StringExp *arg)
    : ::Modmap(loc, arg)
{
}

void Modmap::importAll(Scope *sc)
{
    assert(arg->sz == 1); // FIXME
    StringRef headerName((const char *) arg->string, arg->len);
    calypso.pch.add(headerName);
}

void Modmap::semantic(Scope* sc)
{
}

}
