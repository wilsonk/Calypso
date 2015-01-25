// Contributed by Elie Morisse, same license DMD uses

#include "cpp/cpptemplate.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppexpression.h"
#include "aggregate.h"
#include "enum.h"
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

        clang::TemplateArgumentListInfo Args;

        // See translateTemplateArgument() in SemaTemplate.cpp
        for (auto o: *ti->tiargs)
        {
            Type *ta = isType(o);
            Expression *ea = isExpression(o);
            Dsymbol *sa = isDsymbol(o);

            if (ta)
            {
                auto T = tymap.toType(ti->loc, ta, sc);
                auto DI = Context.getTrivialTypeSourceInfo(T);

                clang::TemplateArgumentLoc Loc(clang::TemplateArgument(T), DI);
                Args.addArgument(Loc);
            }
            else if (ea)
            {
                auto E = expmap.toExpression(ea);

                clang::TemplateArgumentLoc Loc(clang::TemplateArgument(E), E);
                Args.addArgument(Loc);
            }
            else if (sa)
            {
                auto tempdecl = sa->isTemplateDeclaration();
                assert(tempdecl && isCPP(tempdecl));

                auto c_td = static_cast<cpp::TemplateDeclaration *>(tempdecl);
                auto Temp = c_td->getPrimaryTemplate();
                clang::TemplateName Name(Temp);

                clang::TemplateArgumentLoc Loc(clang::TemplateArgument(Name),
                                        clang::NestedNameSpecifierLoc(),
                                        Temp->getLocation(), clang::SourceLocation());
                Args.addArgument(Loc);
            }
            else
                assert(false && "unhandled template arg C++ -> D conversion");
        }

        clang::TemplateName Name(Temp);
        auto Ty = S.CheckTemplateIdType(Name, Temp->getLocation(), Args); // NOTE: this also substitutes the argument types
                                            // to TemplateTypeParmType, which is needed for partial specializations to work.

        if (auto RT = Ty->getAs<clang::RecordType>())
        {
            auto CTSD = cast<clang::ClassTemplateSpecializationDecl>(RT->getDecl());
            ti->Instances[ti->name] = CTSD;
            ti->completeInst();
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
    instantiatedByD = o.instantiatedByD;
}

Dsymbol *TemplateInstance::syntaxCopy(Dsymbol *s)
{
    if (!s)
        s = new cpp::TemplateInstance(*this);
    else
    {
        auto ti = static_cast<cpp::TemplateInstance*>(s);
        ti->Instances = Instances;
        ti->instantiatedByD = instantiatedByD;
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

void TemplateInstance::completeInst()
{
    auto& Context = calypso.pch.AST->getASTContext();
    auto& S = calypso.pch.AST->getSema();

    for (auto I: Instances)
    {
        auto CTSD = dyn_cast<clang::ClassTemplateSpecializationDecl>(I.second);

        if (CTSD && !CTSD->hasDefinition() &&
            CTSD->getSpecializedTemplate()->getTemplatedDecl()->hasDefinition()) // unused forward template specialization decls will exist but as empty aggregates
        {
            auto Ty = Context.getRecordType(CTSD);

            if (S.RequireCompleteType(CTSD->getLocation(), Ty, 0))
                assert(false && "Sema::RequireCompleteType() failed on template specialization");

            // if the definition of the class template specialization wasn't present in the PCH
            // there's a chance the code wasn't emitted in the C++ libraries, so we do it ourselves.
            instantiatedByD = true;

            // Force instantiation of method definitions
            for (auto *D : CTSD->decls())
            {
                if (auto Function = dyn_cast<clang::FunctionDecl>(D))
                    if (!Function->isDefined() && Function->getInstantiatedFromMemberFunction())
                        S.InstantiateFunctionDefinition(CTSD->getLocation(), Function, true, true);
            }
        }
    }
}

// HACK-ish unfortunately.. but partial spec arg deduction isn't trivial. Can't think of a simpler way.
struct CppSymCollector
{
    Dsymbols *substsyms;
    CppSymCollector(Dsymbols *substsyms)
        : substsyms(substsyms) {}

    inline void addIfCPP(Dsymbol *s)
    {
        if (isCPP(s))
            substsyms->push(s);
    }

    void collect(Dsymbol *s)
    {
        addIfCPP(s);
    }

    void collect(Type *t)
    {
        switch (t->ty)
        {
            case Tstruct:
                addIfCPP(static_cast<TypeStruct*>(t)->sym);
                break;
            case Tclass:
                addIfCPP(static_cast<TypeClass*>(t)->sym);
                break;
            case Tenum:
                addIfCPP(static_cast<TypeEnum*>(t)->sym);
                break;
            case Tarray:
            case Tsarray:
            case Tpointer:
            case Treference:
                collect(static_cast<TypeNext*>(t)->next);
                break;
            case Tident:
            case Tinstance:
            default:
//                 ::warning(Loc(), "Collecting C++ symbols unhandled for type %s:\"%s\"",
//                           t->kind(), t->toChars());
                break;
        }
    }

    void collect(Expression *e)
    {
        // TODO DotIdExp ...
    }
};

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
        auto Args = CTSD->getTemplateInstantiationArgs().asArray();
            // NOTE: The issue with getTemplateInstantiationArgs() is that the arguments are already substitued.
            // Since the deduced arguments are contained in one form or another in the original args,
            // the trick is to reference C++ symbols inside the original args and tell TypeMapper to use them.

        auto substsyms = new Dsymbols;
        CppSymCollector collector(substsyms);
        for (auto o: *tiargs)
        {
            Type *ta = isType(o);
            Expression *ea = isExpression(o);
            Dsymbol *sa = isDsymbol(o);

            if (ta) collector.collect(ta);
            else if (ea) collector.collect(ea);
            else { assert(sa); collector.collect(sa); }
        }

        origTiargs = tiargs;

        TypeMapper tymap;
        tymap.addImplicitDecls = false;
        tymap.substsyms = substsyms;

        tiargs = TypeMapper::FromType(tymap).fromTemplateArguments(Args.begin(), Args.end(),
                        Partial->getTemplateParameters());
    }

}

}