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

IMPLEMENT_syntaxCopy(TemplateInstance)

}