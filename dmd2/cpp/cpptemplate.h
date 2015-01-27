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
class TemplateInstance;

class TemplateDeclaration : public ::TemplateDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::NamedDecl *TempOrSpec;  // NOTE: we consider the primary template an explicit specialization as well

    TemplateDeclaration(Loc loc, Identifier *id, TemplateParameters *parameters,
                        Dsymbols *decldefs, const clang::NamedDecl *TempOrSpec);
    TemplateDeclaration(const TemplateDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *) override;
    MATCH matchWithInstance(Scope *sc, ::TemplateInstance *ti, Objects *atypes, Expressions *fargs, int flag) override;
    ::TemplateInstance *foreignInstance(::TemplateInstance *tithis, Scope *sc) override;

    clang::RedeclarableTemplateDecl *getPrimaryTemplate();
    static bool isForeignInstance(::TemplateInstance *ti);
    void correctTempDecl(TemplateInstance *ti);
};

class TemplateInstance : public ::TemplateInstance
{
public:
    CALYPSO_LANGPLUGIN

    llvm::SmallDenseMap<Identifier*, clang::Decl*, 1> Instances; // NOTE: not sure how cpp::TemplateInstance would ever get more than one member though
    bool instantiatedByD = false; // Clang is lazier than DMD when it comes to template instantiation, a PCH might have references or pointers to a template specialization but that specialization although declared might not be defined and codegen'd, whereas DMD expects template specializations to be defined anywhere they appear even as pointers/refs

    clang::Decl* mainInst(); // for gdb

    Objects *origTiargs = nullptr; // needed for deco mangling

    TemplateInstance(Loc loc, Identifier *temp_id);
    TemplateInstance(const TemplateInstance&);
    Dsymbol *syntaxCopy(Dsymbol *) override;
    Identifier *getIdent() override;

    void completeInst();
    void correctTiargs();
};

}

#endif
