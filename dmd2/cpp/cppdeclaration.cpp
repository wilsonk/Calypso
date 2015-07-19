// Contributed by Elie Morisse, same license DMD uses

#include "cpp/calypso.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppexpression.h"
#include "cpp/cpptemplate.h"
#include "aggregate.h"
#include "scope.h"

#include "clang/AST/Decl.h"

namespace cpp
{

using llvm::isa;
using llvm::cast;
using llvm::dyn_cast;

DeclReferencer declReferencer;

VarDeclaration::VarDeclaration(Loc loc, Identifier *id,
                               const clang::ValueDecl *VD, Type *t, Initializer *init)
    : ::VarDeclaration(loc, t, id, init)
{
    this->VD = VD;
}

VarDeclaration::VarDeclaration(const VarDeclaration& o)
    : VarDeclaration(o.loc, o.ident, o.VD, o.type, o.init)
{
    storage_class = o.storage_class; // workaround for syntaxCopy because base method only assigns storage_class if the arg is null (BUG?)
}

FuncDeclaration::FuncDeclaration(Loc loc, Identifier *id, StorageClass storage_class,
                    Type* type, const clang::FunctionDecl *FD)
    : ::FuncDeclaration(loc, loc, id, storage_class, type)
{
    this->FD = FD;
}

FuncDeclaration::FuncDeclaration(const FuncDeclaration& o)
    : FuncDeclaration(o.loc, o.ident, o.storage_class, o.type, o.FD)
{
}

FuncDeclaration *FuncDeclaration::overloadCppMatch(const clang::FunctionDecl* FD)
{
    struct FDEquals
    {
        const clang::FunctionDecl* FD;            // type to match
        FuncDeclaration *f; // return value

        static int fp(void *param, Dsymbol *s)
        {
            if (!s->isFuncDeclaration() || !isCPP(s))
                return 0;
            FuncDeclaration *f = static_cast<FuncDeclaration*>(s);
            FDEquals *p = (FDEquals *)param;

            if (p->FD == f->FD)
            {
                p->f = f;
                return 1;
            }

            return 0;
        }
    };
    FDEquals p;
    p.FD = FD;
    p.f = nullptr;
    overloadApply(this, &p, &FDEquals::fp);
    return p.f;
}

CtorDeclaration::CtorDeclaration(Loc loc, StorageClass storage_class,
                                 Type* type, const clang::CXXConstructorDecl* CCD)
    : ::CtorDeclaration(loc, loc, storage_class, type)
{
    this->CCD = CCD;
}

CtorDeclaration::CtorDeclaration(const CtorDeclaration& o)
    : CtorDeclaration(o.loc, o.storage_class, o.type, o.CCD)
{
}

DtorDeclaration::DtorDeclaration(Loc loc, StorageClass storage_class,
                                 Identifier* id, const clang::CXXDestructorDecl* CDD)
    : ::DtorDeclaration(loc, loc, storage_class, id)
{
    this->CDD = CDD;
}

DtorDeclaration::DtorDeclaration(const DtorDeclaration& o)
    : DtorDeclaration(o.loc, o.storage_class, o.ident, o.CDD)
{
}

EnumDeclaration::EnumDeclaration(Loc loc, Identifier* id, Type* memtype,
                                 const clang::EnumDecl* ED)
    : ::EnumDeclaration(loc, id, memtype)
{
    this->ED = ED;
}

EnumDeclaration::EnumDeclaration(const EnumDeclaration &o)
    : EnumDeclaration(o.loc, o.ident, o.memtype, o.ED)
{
}

AliasDeclaration::AliasDeclaration(Loc loc, Identifier* ident,
                                Type* type, const clang::TypedefNameDecl* TND)
    : ::AliasDeclaration(loc, ident, type)
{
    this->TND = TND;
}

AliasDeclaration::AliasDeclaration(const AliasDeclaration &o)
    : AliasDeclaration(o.loc, o.ident, o.type->syntaxCopy(), o.TND)
{
    this->storage_class = o.storage_class;
}

Dsymbol* AliasDeclaration::syntaxCopy(Dsymbol* s)
{
    assert(!s);
    return new cpp::AliasDeclaration(*this); // hmm hmm
}

OverloadAliasDeclaration::OverloadAliasDeclaration(Loc loc, Identifier *ident,
                        Type *type, TypeFunction *overtf)
    : ::AliasDeclaration(loc, ident, type)
{
    this->overtf = overtf;
}

OverloadAliasDeclaration::OverloadAliasDeclaration(const OverloadAliasDeclaration &o)
    : OverloadAliasDeclaration(o.loc, o.ident, o.type->syntaxCopy(), o.overtf)
{
    this->storage_class = o.storage_class;
}

Dsymbol* OverloadAliasDeclaration::syntaxCopy(Dsymbol* s)
{
    assert(!s);
    return new OverloadAliasDeclaration(*this);
}

IMPLEMENT_syntaxCopy(VarDeclaration, VD)
IMPLEMENT_syntaxCopy(FuncDeclaration, FD)
IMPLEMENT_syntaxCopy(CtorDeclaration, CCD)
IMPLEMENT_syntaxCopy(DtorDeclaration, CDD)
IMPLEMENT_syntaxCopy(EnumDeclaration, ED)

void OverloadAliasDeclaration::semantic(Scope *sc)
{
    ::AliasDeclaration::semantic(sc);

    if (aliassym && overtf)
    {
        assert(aliassym->isFuncDeclaration());
        if (aliassym->hasOverloads())
        {
            overtf = static_cast<TypeFunction*>(overtf->semantic(loc, aliassym->scope));

            struct TypeFunctionEquals
            {
                TypeFunction *tf;            // type to match
                FuncDeclaration *f; // return value

                static int fp(void *param, Dsymbol *s)
                {
                    if (!s->isFuncDeclaration())
                        return 0;
                    FuncDeclaration *f = static_cast<FuncDeclaration*>(s);
                    TypeFunctionEquals *p = (TypeFunctionEquals *)param;

                    if (p->tf->deco == f->type->deco)
                    {
                        p->f = f;
                        return 1;
                    }

                    return 0;
                }
            };
            TypeFunctionEquals p;
            p.tf = overtf;
            p.f = nullptr;
            overloadApply(this, &p, &TypeFunctionEquals::fp);
            assert(p.f);
            aliassym = p.f;
        }
        overtf = nullptr;
    }
}

void FuncDeclaration::semantic(Scope *sc)
{
    if (semanticRun >= PASSsemanticdone)
        return;

    if (FD->getDescribedFunctionTemplate())
    {
        auto ti = sc->parent->isTemplateInstance();
        assert(ti && isCPP(ti->inst));
        auto c_ti = static_cast<cpp::TemplateInstance*>(ti->inst);

        auto Inst = cast<clang::FunctionDecl>(c_ti->Inst);

        assert(isCPP(sc->module));
        DeclMapper m(static_cast<cpp::Module*>(sc->module), true);
        m.addImplicitDecls = false;

        auto inst = static_cast<decltype(this)>(
                (*m.VisitFunctionDecl(Inst))[0]->isFuncDeclaration());
        assert(inst);

        inst->syntaxCopy(this);
    }

    ::FuncDeclaration::semantic(sc);
}

// Cheat and use the C++ "global scope", we can do it safely in specific cases
Scope *globalScope(::Module *m)
{
    auto sc = Scope::createGlobal(m);
    sc = sc->pop();
    sc = sc->push(cpp::Module::rootPackage);
    return sc;
}

void DeclReferencer::Traverse(Loc loc, Scope *sc, clang::Stmt *Body)
{
    this->loc = loc;
    this->sc = sc;
    TraverseStmt(Body);
}

bool DeclReferencer::Reference(const clang::NamedDecl *D)
{
    if (D->isInvalidDecl())
        return true;

    if (auto FD = dyn_cast<clang::FunctionDecl>(D))
    {
        // all FIXME except implicit and builtin decls
        if (FD->getBuiltinID() ||
                FD->isOverloadedOperator() ||
                D->getIdentifierNamespace() & clang::Decl::IDNS_NonMemberOperator ||
                isa<clang::CXXConversionDecl>(D))
            return true;
        if (FD->isOverloadedOperator() && FD->isImplicit())
            return true;
        if (isa<clang::CXXDestructorDecl>(D) && FD->isImplicit())
            return true;
        if (FD->isExternC())
            return true; // FIXME: Clang 3.6 doesn't always map decls to the right source file,
                // so the path generated by typeQualifiedFor although correct will result in a failed lookup.
                // This may get fixed by 3.7.
    }

    if (Referenced.count(D->getCanonicalDecl()))
        return true;
    Referenced.insert(D->getCanonicalDecl());

    auto im = mapper.AddImplicitImportForDecl(D, true);
    im->isstatic = true;
    im->semantic(sc);
    im->semantic2(sc);
    Module::addDeferredSemantic3(im->mod);

    auto tqual = TypeMapper::FromType(mapper).typeQualifiedFor(
                const_cast<clang::NamedDecl*>(D));
    assert(tqual); // temporaire

    auto te = new TypeExp(loc, tqual);
    te->semantic(sc);

    // Memory usage can skyrocket when using a large library
    if (im->packages) delete im->packages;
    delete im;
    delete te;
    delete tqual;

    return true;
}

bool DeclReferencer::Reference(const clang::Type *T)
{
    if (auto RT = T->getAs<clang::RecordType>())
        if (!RT->isUnionType())
            Reference(RT->getDecl());

    return true;
}

bool DeclReferencer::VisitCallExpr(const clang::CallExpr *E)
{
    if (auto Callee = E->getDirectCallee())
        return Reference(Callee);
    return true;
}

bool DeclReferencer::VisitCXXConstructExpr(const clang::CXXConstructExpr *E)
{
    auto ConstructedType = E->getType();
    if (!ConstructedType.isNull())
        Reference(ConstructedType.getTypePtr());
    return true;
}

bool DeclReferencer::VisitCXXNewExpr(const clang::CXXNewExpr *E)
{
    return Reference(E->getOperatorNew());
}

bool DeclReferencer::VisitCXXDeleteExpr(const clang::CXXDeleteExpr *E)
{
    auto DestroyedType = E->getDestroyedType();
    if (!DestroyedType.isNull())
        Reference(DestroyedType.getTypePtr());

    return Reference(E->getOperatorDelete());
}

void FuncDeclaration::semantic3reference(::FuncDeclaration *fd, Scope *sc)
{
    if (fd->semanticRun >= PASSsemantic3)
        return;
    fd->semanticRun = PASSsemantic3;
    fd->semantic3Errors = false;

    auto FD = getFD(fd);

    const clang::FunctionDecl *Def;
    if (!FD->isInvalidDecl() && FD->hasBody(Def))
        declReferencer.Traverse(fd->loc, globalScope(sc->instantiatingModule()), Def->getBody());

    fd->semanticRun = PASSsemantic3done;
}

void FuncDeclaration::semantic3(Scope *sc)
{
    semantic3reference(this, sc);
}

void CtorDeclaration::semantic3(Scope *sc)
{
    cpp::FuncDeclaration::semantic3reference(this, sc);
}

void DtorDeclaration::semantic3(Scope *sc)
{
    cpp::FuncDeclaration::semantic3reference(this, sc);
}

const clang::FunctionDecl *getFD(::FuncDeclaration *f)
{
    assert(isCPP(f));

    if (f->isCtorDeclaration())
        return static_cast<CtorDeclaration*>(f)->CCD;
    else if (f->isDtorDeclaration())
        return static_cast<DtorDeclaration*>(f)->CDD;
    else
        return static_cast<FuncDeclaration*>(f)->FD;
}

}
