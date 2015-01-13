#include "cppimport.h"
#include "cppmodule.h"
#include "calypso.h"
#include "../expression.h"

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#include "llvm/Support/FileSystem.h" // shall be replaced by std.file
#include "clang/Basic/Diagnostic.h"
#include "clang/Frontend/ASTUnit.h"

namespace cpp
{

Import::Import(Loc loc, Identifiers *packages, Identifier *id, Identifier *aliasId,
        int isstatic)
    : ::Import(loc, packages, id, aliasId, isstatic)
{
    // add "cpp" as leftmost package to avoid name clashes
    if (!packages)
        packages = new Identifiers;
    packages->shift(Lexer::idPool("cpp"));  // any better idea ?

    setSymIdent();
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
