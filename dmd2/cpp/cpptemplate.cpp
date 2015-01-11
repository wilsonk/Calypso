// Contributed by Elie Morisse, same license DMD uses

#include "cpp/cpptemplate.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppexpression.h"
#include "scope.h"

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
                    TemplateParameters* parameters,
                    Dsymbols* decldefs,
                    const clang::NamedDecl *TempOrSpec)
    : ::TemplateDeclaration(loc, id, parameters, nullptr, decldefs)
{
    this->TempOrSpec = TempOrSpec;
}

TemplateDeclaration::TemplateDeclaration(const TemplateDeclaration &o)
    : TemplateDeclaration(o.loc, o.ident, o.parameters, o.members, o.TempOrSpec)
{
}

IMPLEMENT_syntaxCopy(TemplateDeclaration, TempOrSpec)

MATCH TemplateDeclaration::matchWithInstance(Scope *sc, ::TemplateInstance *ti,
                                             Objects *atypes, Expressions *fargs, int flag)
{
    // Give only the primary template a chance to match
    // The "best matching" is done dy Sema, and then foreignInstance corrects ti->tempdecl
    if (!isa<clang::RedeclarableTemplateDecl>(TempOrSpec))
    {
        // Check if we're not calling matchWithInstance from a foreignInstance with the
        // tempdecl already determined, which might be a specialization.
        if (!isForeignInstance(ti))
            return MATCHnomatch;
    }

    return ::TemplateDeclaration::matchWithInstance(sc, ti, atypes, fargs, flag);
}

bool TemplateDeclaration::isForeignInstance(::TemplateInstance *ti)
{
    return isCPP(ti) && ti->havetempdecl;
}

clang::RedeclarableTemplateDecl *TemplateDeclaration::getPrimaryTemplate()
{
    if (auto RTD = dyn_cast<clang::RedeclarableTemplateDecl>(TempOrSpec))
        return const_cast<clang::RedeclarableTemplateDecl*>(RTD);

    auto CTSD = cast<clang::ClassTemplateSpecializationDecl>(TempOrSpec);
    return CTSD->getSpecializedTemplate();
}

::TemplateInstance* TemplateDeclaration::foreignInstance(::TemplateInstance* tithis,
                                                       Scope* sc)
{
    auto& Context = calypso.pch.AST->getASTContext();
    auto& S = calypso.pch.AST->getSema();

    if (isForeignInstance(tithis))
        return nullptr;

    TypeMapper tymap;
    ExprMapper expmap(tymap);

    auto Temp = getPrimaryTemplate();

    cpp::TemplateInstance* ti;
    if (isCPP(tithis))
        ti = static_cast<cpp::TemplateInstance *>(
            tithis->syntaxCopy(nullptr));
    else
    {
        ti = new cpp::TemplateInstance(tithis->loc, tithis->name);
        tithis->syntaxCopy(ti);
    }

    if (!ti->Instances.empty()) // the instance is already there, so we only need to correct the tempdecl and start semantic over again (to fix tdtypes notably)
        goto LcorrectTempDecl;

    {
        if (!ti->semanticTiargs(sc))
            assert(false && "foreignInstance semanticTiargs failed");

        llvm::SmallVector<clang::TemplateArgument, 4> Args;
        for (auto o: *ti->tiargs)
        {
            Type *ta = isType(o);
            Expression *ea = isExpression(o);
            Dsymbol *sa = isDsymbol(o);

            if (ta)
                Args.push_back(clang::TemplateArgument(tymap.toType(ti->loc, ta, sc)));
            else if (ea)
                Args.push_back(clang::TemplateArgument(expmap.toExpression(ea)));
            else if (sa)
            {
                auto tempdecl = sa->isTemplateDeclaration();
                assert(tempdecl && isCPP(tempdecl));

                auto c_td = static_cast<cpp::TemplateDeclaration *>(tempdecl);
                clang::TemplateName Name(c_td->getPrimaryTemplate());
                Args.push_back(clang::TemplateArgument(Name));
            }
            else
                assert(false && "unhandled template arg C++ -> D conversion");
        }

        // from Sema::CheckTemplateIdType
        clang::QualType CanonType;
        if (clang::ClassTemplateDecl *ClassTemplate
                    = dyn_cast<clang::ClassTemplateDecl>(Temp)) {
            // Find the class template specialization declaration that
            // corresponds to these arguments.
            void *InsertPos = nullptr;
            clang::ClassTemplateSpecializationDecl *Decl
                = ClassTemplate->findSpecialization(Args, InsertPos);
            if (!Decl) {
                // This is the first time we have referenced this class template
                // specialization. Create the canonical declaration and add it to
                // the set of specializations.
                Decl = clang::ClassTemplateSpecializationDecl::Create(Context,
                                        ClassTemplate->getTemplatedDecl()->getTagKind(),
                                        ClassTemplate->getDeclContext(),
                                        ClassTemplate->getTemplatedDecl()->getLocStart(),
                                        ClassTemplate->getLocation(),
                                        ClassTemplate,
                                        Args.data(),
                                        Args.size(), nullptr);
                ClassTemplate->AddSpecialization(Decl, InsertPos);
                if (ClassTemplate->isOutOfLine())
                    Decl->setLexicalDeclContext(ClassTemplate->getLexicalDeclContext());
            }

            CanonType = Context.getTypeDeclType(Decl);
            assert(isa<clang::RecordType>(CanonType) &&
                "type of non-dependent specialization is not a RecordType");
        }

        clang::TemplateName Name(Temp);
        auto Ty = Context.getTemplateSpecializationType(Name, Args.data(),
                                                        Args.size(), CanonType);

        if (auto RT = Ty->getAs<clang::RecordType>())
        {
            auto CTSD = cast<clang::ClassTemplateSpecializationDecl>(RT->getDecl());
            ti->Instances[ti->name] = CTSD;
            ti->completeInst(sc->module);
        }
        else
            assert(false);
    }

LcorrectTempDecl:
    correctTempDecl(ti);
    ti->havetempdecl = true;

    ti->correctTiargs();

    ti->semanticRun = PASSinit; // WARNING: may disrupt something?
    ti->semantic(sc);
    return ti;
}

void TemplateDeclaration::correctTempDecl(TemplateInstance *ti)
{
    auto Inst = ti->Instances[ti->name];
    auto CTSD = dyn_cast<clang::ClassTemplateSpecializationDecl>(Inst);

    if (isa<clang::TypeAliasTemplateDecl>(TempOrSpec) || // FIXME, any way to retrieve the template decl for alias templates?
        !CTSD) // FIXME for func templates
    {
        ti->tempdecl = this;
        return;
    }

    auto RealTemp = getTemplateSpecializedDecl(CTSD)->getCanonicalDecl();

    ::TemplateDeclaration *td = this;
    if (td->overroot)
        td = td->overroot;

    // Find and set the correct tempdecl of the instance
    for (; td; td = td->overnext)
    {
        if (!isCPP(td))
        {
            ::warning(td->loc, "Unexpected non C++ template declaration");
            continue;
        }

        auto c_td = static_cast<cpp::TemplateDeclaration*>(td);
        if (c_td->TempOrSpec->getCanonicalDecl() == RealTemp)
        {
            ti->tempdecl = c_td;
            break;
        }
    }
    assert(static_cast<cpp::TemplateDeclaration*>(ti->tempdecl)->TempOrSpec->getCanonicalDecl() == RealTemp);
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

Identifier *TemplateInstance::getIdent()
{
    auto a = tiargs;
    if (origTiargs)
        tiargs = origTiargs;

    auto result = ::TemplateInstance::getIdent();

    tiargs = a;
    return result;
}

void TemplateInstance::completeInst(::Module *instMod)
{
    auto& Context = calypso.pch.AST->getASTContext();
    auto& S = calypso.pch.AST->getSema();

    for (auto D: Instances)
    {
        auto CTSD = dyn_cast<clang::ClassTemplateSpecializationDecl>(D.second);

        if (CTSD && !CTSD->hasDefinition() &&
            CTSD->getSpecializedTemplate()->getTemplatedDecl()->hasDefinition()) // unused forward template specialization decls will exist but as empty aggregates
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

void TemplateInstance::correctTiargs()
{
    auto Inst = Instances[name];
    auto CTSD = dyn_cast<clang::ClassTemplateSpecializationDecl>(Inst);

    if (!CTSD)
        return;

    auto TempOrSpec = static_cast<TemplateDeclaration*>(tempdecl)->TempOrSpec;

    // Correction is only needed for instances from partial specs
    if (auto Partial = dyn_cast<clang::ClassTemplatePartialSpecializationDecl>(TempOrSpec))
    {
        TypeMapper tymap; tymap.addImplicitDecls = false;
        auto Args = CTSD->getTemplateInstantiationArgs().asArray();
            // FIXME FIXME The issue here, is that the arguments are already substitued.
            // Template arguments deduction for partial specializations do not make use of any outside type, everything is contained in one form or another in the original arguments

        origTiargs = tiargs;
        tiargs = TypeMapper::FromType(tymap).fromTemplateArguments(Args.begin(), Args.end(),
                        Partial->getTemplateParameters());
    }

}

}