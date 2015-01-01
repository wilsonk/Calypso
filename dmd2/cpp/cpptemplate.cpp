// Contributed by Elie Morisse, same license DMD uses

#include "cpp/cpptemplate.h"
#include "cpp/cppdeclaration.h"

#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/Sema/Sema.h"

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

TemplateDeclaration::TemplateDeclaration(Loc loc, Identifier* id,
                    TemplateParameters* parameters, Dsymbols* decldefs)
    : ::TemplateDeclaration(loc, id, parameters, nullptr, decldefs)
{
}

::TemplateInstance* TemplateDeclaration::foreignInstance(::TemplateInstance* tithis,
                                                       Scope* sc)
{
    return nullptr;
}


TemplateInstance::TemplateInstance(Loc loc, Identifier* temp_id)
    : ::TemplateInstance(loc, temp_id)
{
}

TemplateInstance::TemplateInstance(const TemplateInstance& o)
    : TemplateInstance(o.loc, o.name)
{
    Instances = o.Instances;
    instantiatingModuleCpp = o.instantiatingModuleCpp;
}

Dsymbol *TemplateInstance::syntaxCopy(Dsymbol *s)
{
    if (!s)
        s = new cpp::TemplateInstance(*this);
    else
    {
        auto ti = static_cast<cpp::TemplateInstance*>(s);
        ti->Instances = Instances;
        ti->instantiatingModuleCpp = instantiatingModuleCpp;
    }

    return ::TemplateInstance::syntaxCopy(s);
}

void TemplateInstance::completeInst(::Module *instMod)
{
    auto& Context = calypso.pch.AST->getASTContext();
    auto& S = calypso.pch.AST->getSema();

    for (auto D: Instances)
    {
        auto CTSD = cast<clang::ClassTemplateSpecializationDecl>(D.second);

        if (!CTSD->hasDefinition())
        {
            auto Ty = Context.getRecordType(CTSD);

            if (S.RequireCompleteType(CTSD->getLocation(), Ty, 0))
                assert(false && "Sema::RequireCompleteType() failed on template specialization");

            // if the definition of the class template specialization wasn't present in the PCH
            // there's a chance the code wasn't emitted in the C++ libraries, so we do it ourselves.
            instantiatingModuleCpp = instMod;
            assert(instantiatingModuleCpp);
        }
    }
}

}