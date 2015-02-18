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

IMPLEMENT_syntaxCopy(StructDeclaration, RD)
IMPLEMENT_syntaxCopy(ClassDeclaration, RD)

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

        DeclMapper m(nullptr);
        m.addImplicitDecls = false;

        auto instsd = static_cast<cpp::StructDeclaration*>(
                m.VisitInstancedClassTemplate(InstRD)->isStructDeclaration());
        assert(instsd);

        instsd->syntaxCopy(this);
    }

    ::StructDeclaration::semantic(sc);
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

        DeclMapper m(nullptr);
        m.addImplicitDecls = false;

        auto instcd = static_cast<cpp::ClassDeclaration*>(
            m.VisitInstancedClassTemplate(InstRD, DeclMapper::ForceNonPOD)->isClassDeclaration());
        assert(instcd);

        instcd->syntaxCopy(this);
    }

    ::ClassDeclaration::semantic(sc);
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

        auto offset = Context.getASTRecordLayout(RD2).getBaseClassOffset(RD);
        *poffset += offset.getQuantity();
    }

    return true;
}

void ClassDeclaration::interfaceSemantic(Scope *sc)
{
}

static FuncDeclaration *funcMatch(Dsymbol *s, const clang::CXXMethodDecl* MD)
{
    auto md = s->isFuncDeclaration();
    if (!md || !isCPP(md))
        return nullptr;

    auto FD = getFD(md);
    if (FD->getCanonicalDecl() == MD->getCanonicalDecl())
        return static_cast<FuncDeclaration*>(md);

    return nullptr;
}

FuncDeclaration *ClassDeclaration::findMethod(const clang::CXXMethodDecl* MD)
{
    TypeMapper tmap;
    tmap.addImplicitDecls = false;

    Identifier *ident;
    if (isa<clang::CXXConstructorDecl>(MD))
        ident = Id::ctor;
    else if (isa<clang::CXXDestructorDecl>(MD))
        ident = Id::dtor;
    else
        ident = fromIdentifier(MD->getIdentifier());

    auto s = ScopeDsymbol::search(loc, ident);
    if (s)
    {
//         if (auto os = s->isOverloadSet())
//         {
//             for (auto *s2: os->a)
//             {
//                 if (auto md = funcMatch(s2, MD))
//                     return md;
//             }
//         }

        if (auto fd = s->isFuncDeclaration())
        {
            fd = fd->overloadExactMatch(tmap.fromType(MD->getType()));
            if (fd)
                if (auto md = funcMatch(fd, MD))
                    return md;
        }
    }

    // search in base classes
    for (auto *b: *baseclasses)
    {
        if (!b->base->isClassDeclaration() || !isCPP(b->base)) // skip Object
            continue;

        auto base = static_cast<ClassDeclaration*>(b->base);

        auto result = base->findMethod(MD);
        if (result)
            return result;
    }

    return nullptr;
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
void ClassDeclaration::initVtbl()
{
    ::ClassDeclaration::initVtbl();

    clang::CXXFinalOverriderMap FinaOverriders;
    RD->getFinalOverriders(FinaOverriders);

    auto cb = isClassDeclarationOrNull(baseClass);
    llvm::DenseSet<const clang::CXXMethodDecl*> inVtbl;

    for (auto I = FinaOverriders.begin(), E = FinaOverriders.end();
         I != E; ++I)
    {
        auto OverMD = I->second.begin()->second.front().Method;
        if (inVtbl.count(OverMD))
            continue;

        auto md = findMethod(OverMD);
#if 0
        if (!md)
            md = findMethod(OverMD);
        assert(md && "CXXFinalOverrider not in cpp::ClassDeclaration");
#else
        if (!md)
            continue;
#endif

        inVtbl.insert(OverMD);

        if (cb)
        {
            auto vi = md->findVtblIndex(&cb->vtbl, cb->vtbl.dim);
            if (vi >= 0)
            {
                vtbl[vi] = md;
                continue;
            }
        }
        vtbl.push(md);
    }
}

void ClassDeclaration::buildLayout()
{
    auto& Context = calypso.getASTContext();
    auto& RL = Context.getASTRecordLayout(RD);
    
    alignsize = RL.getAlignment().getQuantity();
    structsize = RL.getSize().getQuantity();
    
    for (size_t i = 0; i < members->dim; i++)
    {
        auto s = (*members)[i];
        
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
        
        fields.push(c_vd);
    }
    
    sizeok = SIZEOKdone;
}

// NOTE: we need to adjust every "this" pointer when accessing fields from bases
// This is what Clang does in Sema::PerformObjectMemberConversion
Expression *LangPlugin::getRightThis(Loc loc, Scope *sc, ::AggregateDeclaration *ad,
        Expression *e1, Declaration *var, int)
{
    if (!ad->isClassDeclaration())
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

::FuncDeclaration *LangPlugin::buildCpCtor(::StructDeclaration *sd,
                                           Scope *sc)
{
    return nullptr;
}

::FuncDeclaration *LangPlugin::buildDtor(::AggregateDeclaration *ad,
                                         Scope *sc)
{
    auto& S = getASTUnit()->getSema();
    bool isClass = ad->isClassDeclaration();

    const clang::RecordDecl *RD = isClass
              ? static_cast<cpp::ClassDeclaration*>(ad)->RD
              : static_cast<cpp::StructDeclaration*>(ad)->RD;

    auto CRD = dyn_cast<clang::CXXRecordDecl>(RD);
    if (!CRD || !RD->isCompleteDefinition())
        return nullptr;

    if (CRD->hasTrivialDestructor())
        return nullptr; // WARNING not sure about all the implications of letting DMD build the dtor

    auto CDD = S.LookupDestructor(
            const_cast<clang::CXXRecordDecl *>(CRD));
    S.MarkFunctionReferenced(clang::SourceLocation(), CDD);

    StorageClass stc = STCsafe | STCnothrow | STCpure | STCnogc;
    DtorDeclaration *dd = new DtorDeclaration(Loc(), stc,
                                              Lexer::idPool("__fieldDtor"), CDD);
    ad->dtors.shift(dd);
    ad->members->push(dd);
    dd->semantic(sc);

    return dd;
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

}
