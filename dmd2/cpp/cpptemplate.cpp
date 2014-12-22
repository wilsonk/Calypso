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

TemplateInstance::TemplateInstance(Loc loc, Identifier* temp_id, const clang::Decl* Instantiated)
    : ::TemplateInstance(loc, temp_id)
{
    this->Instantiated = Instantiated;
}

TemplateInstance::TemplateInstance(const TemplateInstance& o)
    : TemplateInstance(loc, name, Instantiated)
{
}

IMPLEMENT_syntaxCopy(TemplateInstance)

}