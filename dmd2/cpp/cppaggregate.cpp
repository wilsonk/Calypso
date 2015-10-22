// Contributed by Elie Morisse, same license DMD uses

#include "cpp/astunit.h"
#include "cpp/calypso.h"
#include "cpp/cppaggregate.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cpptemplate.h"
#include "scope.h"
#include "target.h"
#include "template.h"
#include "id.h"

#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/RecordLayout.h"
#include "clang/AST/VTableBuilder.h"
#include "clang/Sema/Sema.h"

namespace cpp
{

using llvm::isa;
using llvm::cast;
using llvm::dyn_cast;

StructDeclaration::StructDeclaration(Loc loc, Identifier* id,
                                     const clang::RecordDecl* RD)
    : ::StructDeclaration(loc, id)
{
    this->RD = RD;
}

StructDeclaration::StructDeclaration(const StructDeclaration& o)
    : StructDeclaration(o.loc, o.ident, o.RD)
{
}

ClassDeclaration::ClassDeclaration(Loc loc, Identifier *id, BaseClasses *baseclasses,
                                   const clang::CXXRecordDecl *RD)
    : ::ClassDeclaration(loc, id, baseclasses)
{
    this->RD = RD;
}

ClassDeclaration::ClassDeclaration(const ClassDeclaration& o)
    : ClassDeclaration(o.loc, o.ident, nullptr, o.RD)
{ // NOTE: baseclasses will be duplicated by syntaxCopy, but this makes the copy constructor not doing what it should be doing
}

UnionDeclaration::UnionDeclaration(Loc loc, Identifier* id,
                                     const clang::RecordDecl* RD)
    : ::UnionDeclaration(loc, id)
{
    this->RD = RD;
}

UnionDeclaration::UnionDeclaration(const UnionDeclaration& o)
    : UnionDeclaration(o.loc, o.ident, o.RD)
{
}

IMPLEMENT_syntaxCopy(StructDeclaration, RD)
IMPLEMENT_syntaxCopy(ClassDeclaration, RD)
IMPLEMENT_syntaxCopy(UnionDeclaration, RD)

void StructDeclaration::semantic(Scope *sc)
{
    if (semanticRun >= PASSsemanticdone)
        return;

    auto CRD = dyn_cast<clang::CXXRecordDecl>(RD);
    assert(CRD || !sc->parent->isTemplateInstance());

    if (CRD && (CRD->getDescribedClassTemplate() ||
                isa<clang::ClassTemplatePartialSpecializationDecl>(CRD)))
    {
        auto ti = sc->parent->isTemplateInstance();

        assert(ti && isCPP(ti->inst));
        auto c_ti = static_cast<cpp::TemplateInstance*>(ti->inst);
        auto InstRD = cast<clang::ClassTemplateSpecializationDecl>(c_ti->Inst);

        assert(isCPP(sc->module));
        DeclMapper m(static_cast<cpp::Module*>(sc->module), true);
        m.addImplicitDecls = false;

        auto instsd = static_cast<cpp::StructDeclaration*>(
                m.VisitInstancedClassTemplate(InstRD)->isStructDeclaration());
        assert(instsd);

        instsd->syntaxCopy(this);
    }

    ::StructDeclaration::semantic(sc);
}

unsigned int StructDeclaration::size(Loc loc)
{
    if (sizeok != SIZEOKdone)
        buildAggLayout(this);

    return structsize;
}

bool StructDeclaration::mayBeAnonymous()
{
    return true;
}

void ClassDeclaration::semantic(Scope *sc)
{
    if (semanticRun >= PASSsemanticdone)
        return;

    // Basically a hook at the beginning of semantic(), to change RD from the template decl
    // to the instantation decl if needed.
    if (RD->getDescribedClassTemplate() ||
            isa<clang::ClassTemplatePartialSpecializationDecl>(RD))
    {
        auto ti = sc->parent->isTemplateInstance();

        assert(ti && isCPP(ti->inst));
        auto c_ti = static_cast<cpp::TemplateInstance*>(ti->inst);
        auto InstRD = cast<clang::ClassTemplateSpecializationDecl>(c_ti->Inst);

        assert(isCPP(sc->module));
        DeclMapper m(static_cast<cpp::Module*>(sc->module), true);
        m.addImplicitDecls = false;

        auto instcd = static_cast<cpp::ClassDeclaration*>(
            m.VisitInstancedClassTemplate(InstRD, DeclMapper::ForceNonPOD)->isClassDeclaration());
        assert(instcd);

        instcd->syntaxCopy(this);
    }

    ::ClassDeclaration::semantic(sc);
}

unsigned int ClassDeclaration::size(Loc loc)
{
    if (sizeok != SIZEOKdone)
        buildLayout();

    return structsize;
}

bool ClassDeclaration::mayBeAnonymous()
{
    return true;
}

bool ClassDeclaration::isBaseOf(::ClassDeclaration *cd, int *poffset)
{
    if (!isBaseOf2(cd))
        return false;

    if (poffset)
    {
        *poffset = 0;

        // If the derived class is a D one, class instances need to hold two __vptr pointers
        // Their header will be:
        //      D __vptr
        //      D __monitor
        //      C++ __vptr
        if (!isCPP(cd))
            *poffset = Target::ptrsize * 2;

        while (!isCPP(cd))
            cd = static_cast<::ClassDeclaration*>(cd->baseClass);

        auto& Context = calypso.getASTContext();
        auto RD2 = static_cast<cpp::ClassDeclaration*>(cd)->RD;

        if (RD->getCanonicalDecl() == RD2->getCanonicalDecl())
            return true;

        auto Offset = clang::ComputeBaseOffset(Context, RD, RD2);
        *poffset += Offset.NonVirtualOffset.getQuantity();
    }

    return true;
}

void ClassDeclaration::interfaceSemantic(Scope *sc)
{
}

Expression *ClassDeclaration::defaultInit(Loc loc)
{
    if (!defaultCtor)
        return ::ClassDeclaration::defaultInit(loc);

//     auto arguments = new Expressions;
//     return new CallExp(loc, new TypeExp(loc, type), arguments);

    return nullptr; // handled in cpptoir.cpp because CallExp(TypeExp()) causes recursive evaluation
}

void ClassDeclaration::makeNested()
{
    // do not add vthis
}

// NOTE: the "D" vtbl isn't used unless a D class inherits from a C++ one
// Note that Func::semantic will re-set methods redundantly (although it's useful as a sanity check and it also sets vtblIndex),
// but vanilla doesn't know how to deal with multiple inheritance hence the need to query Clang.

// Why is this needed? Because D vtbls are only built after the first base class, so this is actually the cleanest and easiest way
// to take C++ multiple inheritance into account. No change to FuncDeclaration::semantic needed.
void ClassDeclaration::finalizeVtbl()
{
    clang::CXXFinalOverriderMap FinaOverriders;
    RD->getFinalOverriders(FinaOverriders);

    llvm::DenseSet<const clang::CXXMethodDecl*> inVtbl;

    for (auto I = FinaOverriders.begin(), E = FinaOverriders.end();
         I != E; ++I)
    {
        auto OverMD = I->second.begin()->second.front().Method;
        if (inVtbl.count(OverMD))
            continue;

        auto md = findMethod(this, OverMD);
        if (!md)
            continue;

        inVtbl.insert(OverMD);

        auto vi = md->findVtblIndex(&vtbl, vtbl.dim);
        if (vi < 0)
            vtbl.push(md);
    }
}

void ClassDeclaration::buildLayout()
{
    buildAggLayout(this);
}

unsigned int UnionDeclaration::size(Loc loc)
{
    if (sizeok != SIZEOKdone)
        buildAggLayout(this);

    return structsize;
}

bool UnionDeclaration::mayBeAnonymous()
{
    return true;
}

// NOTE: we need to adjust every "this" pointer when accessing fields from bases
// This is what Clang does in Sema::PerformObjectMemberConversion
Expression *LangPlugin::getRightThis(Loc loc, Scope *sc, ::AggregateDeclaration *ad,
        Expression *e1, Declaration *var, int)
{
    if (!ad->isClassDeclaration()) // FIXME handle 2nd, 3rd etc. struct base, or revert the inherit from struct commit
        return nullptr;

    auto cd = static_cast<cpp::ClassDeclaration*>(ad);

    Type *t = e1->type->toBasetype();
    if (ad->getType()->constConv(t) > MATCHnomatch)
        return e1;

    ::ClassDeclaration *tcd = t->isClassHandle();
    assert(tcd && cd->isBaseOf2(tcd));

    e1 = new CastExp(loc, e1, ad->getType());
    e1 = e1->semantic(sc);

    return e1;
}

::FuncDeclaration *LangPlugin::buildDtor(::AggregateDeclaration *ad, Scope *sc)
{
    assert(ad->dtors.dim < 2);

    if (ad->dtors.empty())
        return nullptr; // forward reference

    return ad->dtors[0];
}

::FuncDeclaration *LangPlugin::buildCpCtor(::StructDeclaration *sd, Scope *sc)
{
    return nullptr; // TODO
}

::FuncDeclaration *LangPlugin::buildOpAssign(::StructDeclaration *sd, Scope *sc)
{
    auto& S = calypso.pch.AST->getSema();
    auto RD = getRecordDecl(sd);

    if (sd->isUnionDeclaration() || !RD->getDefinition())
        return nullptr;

    auto CRD = const_cast<clang::CXXRecordDecl*>(
                cast<clang::CXXRecordDecl>(RD));
    auto MD = S.LookupCopyingAssignment(CRD, clang::Qualifiers::Const, false, 0);

    if (!MD || MD->isDeleted())
        return nullptr;

    return findMethod(sd, MD);
}

template <typename AggTy>
 void buildAggLayout(AggTy *ad)
{
    assert(isCPP(ad));

    if (ad->layoutQueried)
        return;

    if (ad->RD->isInvalidDecl())
        return; // if it's a forward reference, consider the record empty

    auto& Context = calypso.getASTContext();
    auto& RL = Context.getASTRecordLayout(ad->RD);

    ad->alignment = ad->alignsize = RL.getAlignment().getQuantity();
    ad->structsize = RL.getSize().getQuantity();

    for (size_t i = 0; i < ad->members->dim; i++)
    {
        auto s = (*ad->members)[i];

        auto vd = s->isVarDeclaration();
        if (!vd)
            continue;

        assert(isCPP(vd));

        auto c_vd = static_cast<VarDeclaration*>(vd);
        auto FD = dyn_cast<clang::FieldDecl>(c_vd->VD);

        if (!FD)
            continue;

        auto fldIdx = FD->getFieldIndex();
        c_vd->offset = RL.getFieldOffset(fldIdx) / 8;

        ad->fields.push(c_vd);
    }

    ad->layoutQueried = true;
}

const clang::RecordDecl *getRecordDecl(::AggregateDeclaration *ad)
{
    assert(isCPP(ad));

    if (auto sd = ad->isStructDeclaration())
        return static_cast<StructDeclaration*>(sd)->RD;
    else if (auto cd = ad->isClassDeclaration())
        return static_cast<ClassDeclaration*>(cd)->RD;

   llvm_unreachable("Unknown aggregate decl type?");
}

const clang::RecordDecl *getRecordDecl(::Type *t)
{
    ::AggregateDeclaration *ad;

    switch(t->ty)
    {
        case Tstruct:
            ad = static_cast<TypeStruct*>(t)->sym;
            break;
        case Tclass:
            ad = static_cast<TypeClass*>(t)->sym;
            break;
        default:
            llvm_unreachable("Non-aggregate type");
    }

    return getRecordDecl(ad);
}

::FuncDeclaration *findMethod(::AggregateDeclaration *ad, const clang::FunctionDecl* FD)
{
    TypeMapper tmap;
    tmap.addImplicitDecls = false;

    auto ident = getExtendedIdentifier(FD, tmap);

    auto s = ad->ScopeDsymbol::search(ad->loc, ident);
    if (s && s->isFuncDeclaration())
    {
        assert(isCPP(s));
        auto fd = static_cast<::FuncDeclaration*>(s);
        fd = FuncDeclaration::overloadCppMatch(fd, FD);
        if (fd)
            return fd;
    }

    // search in base classes
    if (auto cd = ad->isClassDeclaration())
        for (auto *b: *cd->baseclasses)
        {
            if (!isCPP(b->base)) // skip Object
                continue;

            auto result = findMethod(b->base, FD);
            if (result)
                return result;
        }

    return nullptr;
}

}
