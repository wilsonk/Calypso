// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPTEMPLATE_H
#define DMD_CPP_CPPTEMPLATE_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root.h"
#include "cpp/calypso.h"
#include "template.h"

namespace clang
{
class Decl;
}

namespace cpp
{

class TemplateInstance : public ::TemplateInstance
{
public:
    CALYPSO_LANGPLUGIN // temporary: we don't atm instantiate templates and store the already instantiated decl from Sema

    const clang::Decl *Instantiated;
    TemplateInstance(Loc loc, Identifier *temp_id, const clang::Decl *Instantiated);
    TemplateInstance(const TemplateInstance&);
    Dsymbol *syntaxCopy(Dsymbol *) override;
};

}

#endif
