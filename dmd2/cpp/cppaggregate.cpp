// Contributed by Elie Morisse, same license DMD uses

#include "cppaggregate.h"
#include "cppdeclaration.h"
#include "calypso.h"
#include "../target.h"

#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/RecordLayout.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/Sema/Sema.h"

namespace cpp
{

StructDeclaration::StructDeclaration(Loc loc, Identifier* id,
                                     const clang::RecordDecl* RD)
    : ::StructDeclaration(loc, id)
{
    this->RD = RD;
}

ClassDeclaration::ClassDeclaration(Loc loc, Identifier *id, BaseClasses *baseclasses,
                                   const clang::CXXRecordDecl *RD)
    : ::ClassDeclaration(loc, id, baseclasses)
{
    this->RD = RD;
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
            cd = cd->baseClass;

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
    auto md = static_cast<cpp::FuncDeclaration *>(
        s->isFuncDeclaration());
    assert(md);

    if (md->FD->getCanonicalDecl() == MD->getCanonicalDecl())
        return md;

    return nullptr;
}

FuncDeclaration *ClassDeclaration::findMethod(const clang::CXXMethodDecl* MD)
{
    TypeMapper tmap;

    auto s = ScopeDsymbol::search(loc,
                                  toIdentifier(MD->getIdentifier()));

    if (s)
    {
        if (auto os = s->isOverloadSet())
        {
            for (auto *s2: os->a)
            {
                if (auto md = funcMatch(s2, MD))
                    return md;
            }
        }
        else if (auto fd = s->isFuncDeclaration())
        {
            if (auto md = funcMatch(fd, MD))
                return md;
        }
    }

    // search in base classes
    for (auto *b: *baseclasses)
    {
        auto base = static_cast<cpp::ClassDeclaration*>(b->base);

        auto result = base->findMethod(MD);
        if (result)
            return result;
    }

    return nullptr;
}

// NOTE since this might be confusing a bit:
// the "D" vtbl isn't used unless a D class inherits from a C++ one
// So we're free to ignore the base classes' vtbl and get a "final" C++ vtbl
// Note that Func::semantic will re-set methods redundantly (although it's useful as a sanity check and it also sets vtblIndex),
// but vanilla doesn't know how to deal with multiple inheritance hence the need to query Clang.

// Why is this needed? Because D vtbls are only built after the one base class, so this is actually the cleanest and easiest way
// to take C++ multiple inheritance into account. No change to FuncDeclaration::semantic needed.
void ClassDeclaration::initVtbl()
{
    vtbl.setDim(0);
    if (vtblOffset())
        vtbl.push(this);

    clang::CXXFinalOverriderMap FinaOverriders;
    RD->getFinalOverriders(FinaOverriders);

    llvm::DenseSet<const clang::CXXMethodDecl*> inVtbl;

    for (auto I = FinaOverriders.begin(), E = FinaOverriders.end();
         I != E; ++I)
    {
        auto OverMD = I->second.begin()->second.front().Method;
        if (inVtbl.count(OverMD))
            continue;

        auto md = findMethod(OverMD);
        assert(md && "CXXFinalOverrider not in cpp::ClassDeclaration");

        vtbl.push(md);
        inVtbl.insert(OverMD);
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
        
        auto c_vd = static_cast<VarDeclaration*>(vd);
        auto FD = llvm::dyn_cast<clang::FieldDecl>(c_vd->VD);
        
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
    if (t->equals(ad->getType()))
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

    auto CRD = llvm::dyn_cast<clang::CXXRecordDecl>(RD);
    if (!CRD)
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

}
