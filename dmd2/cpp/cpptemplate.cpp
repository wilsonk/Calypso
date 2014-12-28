// Contributed by Elie Morisse, same license DMD uses

#include "cpp/cpptemplate.h"
#include "cpp/cppdeclaration.h"

#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

TemplateInstance::TemplateInstance(Loc loc, Identifier* temp_id,
                                clang::Decl* Instantiated, ::Module* instantiatingModuleCpp)
    : ::TemplateInstance(loc, temp_id)
{
    this->Instantiated = Instantiated;
    this->instantiatingModuleCpp = instantiatingModuleCpp;
}

TemplateInstance::TemplateInstance(const TemplateInstance& o)
    : TemplateInstance(loc, name, Instantiated, instantiatingModuleCpp)
{
}

Dsymbol *TemplateInstance::syntaxCopy(Dsymbol *s)
{
    if (!s)
        s = new cpp::TemplateInstance(*this);

    auto ti = static_cast<cpp::TemplateInstance*>(s);
    ti->Instantiated = Instantiated;
    ti->instantiatingModuleCpp = instantiatingModuleCpp;

    return ::TemplateInstance::syntaxCopy(s);
}

}