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

    clang::Decl *Instantiated;
    ::Module *instantiatingModuleCpp; // Clang is lazier than DMD when it comes to template instantiation, a PCH might have references or pointers to a template specialization but that specialization although declared might not be defined and codegen'd, whereas DMD expects template specializations to be defined anywhere they appear even as pointers/refs

    TemplateInstance(Loc loc, Identifier *temp_id,
                     clang::Decl *Instantiated, ::Module *instantiatingModuleCpp);
    TemplateInstance(const TemplateInstance&);
    Dsymbol *syntaxCopy(Dsymbol *) override;
};

}

#endif
