// Contributed by Elie Morisse, same license DMD uses

#include "../aggregate.h"
#include "../attrib.h"
#include "../declaration.h"
#include "../enum.h"
#include "../identifier.h"
#include "../import.h"
#include "../init.h"
#include "../lexer.h"
#include "../template.h"
#include "../scope.h"
#include "id.h"

#include "calypso.h"
#include "cppmodule.h"
#include "cppdeclaration.h"
#include "cppimport.h"
#include "cppaggregate.h"
#include "cppexpression.h"
#include "cpptemplate.h"

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

#include "llvm/ADT/DenseMap.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/Sema/Sema.h"

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

/********************************/

Package *Module::rootPackage;
Modules Module::amodules;

void Module::init()
{
    rootPackage = new Package(Lexer::idPool("cpp"));
    rootPackage->symtab = new DsymbolTable;

    modules->insert(rootPackage);
}

static void combine(char *&objfn, Identifier *id)
{
    auto prevobjfn = objfn;

    auto objlen = strlen(objfn);
    auto idlen = id->len;
    objfn = (char *)mem.malloc(objlen + 1 + idlen + 1);
    memcpy(objfn, prevobjfn, objlen);
    objfn[objlen] = '_';
    objlen++;
    memcpy(objfn + objlen, id->string, idlen + 1);

    mem.free(prevobjfn);
}

Module::Module(const char* filename, Identifier* ident, Identifiers *packages)
    : ::Module(nullptr, ident, 0, 0)
{
    srcfile = new File(filename);

    // Let's not create directories for the time being
    const char *objPrefix = "__cpp";
    char *objfn = strdup(objPrefix);

    // e.g __cpp_package_package_module.o
    for (size_t i = 1; i < packages->dim; i++)
    {
        Identifier *pid = (*packages)[i];
        combine(objfn, pid);
    }
    combine(objfn, ident);

    arg = objfn;
}

void Module::addPreambule()
{
    // Statically import object.d for object and size_t (used by buildXtoHash)

    // TODO This still makes "object" susceptible to collide with C++ names.
    // We could eventually choose a random unused alias name if necessary.
    if (members->dim == 0 || ((*members)[0])->ident != Id::object)
    {
        ::Import *im = new ::Import(Loc(), NULL, Id::object, NULL, true);
        members->shift(im);
    }
}

/************************************/

// DeclMapper::DeclMapper(Module* mod)
//     : TypeMapper(mod)
// {
// }

inline PROT DeclMapper::toPROT(clang::AccessSpecifier AS)
{
    switch(AS) {
        case clang::AS_public:
            return PROTpublic;
        case clang::AS_protected:
            return PROTprotected;
        case clang::AS_private:
            return PROTprivate;
        case clang::AS_none:
            return PROTnone;
        default:
            return PROTundefined;
    }
}

/*****/

Dsymbols *DeclMapper::VisitDeclContext(const clang::DeclContext *DC)
{
    auto decldefs = new Dsymbols;

    for (auto D = DC->decls_begin(), DEnd = DC->decls_end();
        D != DEnd; ++D)
    {
        if (auto d = VisitDecl(*D))
            decldefs->push(d);
    }

    return decldefs;
}

Dsymbol *DeclMapper::VisitDecl(const clang::Decl *D)
{
    if (!D->isCanonicalDecl())
        return nullptr;

    Dsymbol *s = nullptr;

    // Unfortunately a long ugly list of if (... dyn_cast...) is more solid and
    // future-proof than a pretty switch à la decl_visitor

#define DECL(BASE) \
    else if (const clang::BASE##Decl *BASE##D = \
                            dyn_cast<clang::BASE##Decl>(D)) \
        s = Visit##BASE##Decl(BASE##D);

    if (0) ;
    DECL(TypedefName)
    DECL(ClassTemplateSpecialization)
    DECL(Record)
    DECL(Function)
    DECL(RedeclarableTemplate)
    DECL(Enum)
    DECL(Value)

#undef DECL

    return s;
}

Dsymbol *DeclMapper::VisitValueDecl(const clang::ValueDecl *D)
{
    auto& Context = calypso.getASTContext();
    ExprMapper expmap(*this);

    auto loc = fromLoc(D->getLocation());
    auto Ty = D->getType().getDesugaredType(Context);

    if (Ty->isUnionType() || Ty->isStructureType())
    {
        if (auto RT = Ty->getAs<clang::RecordType>()) // dyn_cast because it can also be a typedeftype, which always have names
        {
            auto RD = RT->getDecl();

            if (RD->isAnonymousStructOrUnion()) // NOTE: in union {...} myUnion this will be false, see Decl.h
                return VisitRecordDecl(RD);

            // FIXME how to handle union {...} myUnion? Ignore them for now
            if (!RD->getIdentifier())
                return nullptr;
        }
    }

    auto id = fromIdentifier(D->getIdentifier());
    auto t = fromType(D->getType());

    if (!t)
        return nullptr;

    auto a = new VarDeclaration(loc, id, D, t);

    if (auto Var = dyn_cast<clang::VarDecl>(D))
    {
        if (Var->hasExternalStorage())
            a->storage_class |= STCextern;

        if (Var->getTLSKind() == clang::VarDecl::TLS_Dynamic)
            a->storage_class |= STCtls;

        if (Var->isStaticDataMember())
            a->storage_class |= STCstatic;

        if (Var->isConstexpr() && Var->getAnyInitializer())
        {
            // we avoid initializer expressions except for constexpr variables
            auto e = expmap.fromExpression(Var->getAnyInitializer(),
                                           nullptr, true);
            if (e->op != TOKnull)
            {
                a->init = new ExpInitializer(loc, e);
                a->storage_class |= STCimmutable;
            }
        }
    }
    return a;
}

Dsymbol *DeclMapper::VisitRecordDecl(const clang::RecordDecl *D, unsigned flags)
{
    auto& Context = calypso.pch.AST->getASTContext();
    auto& S = calypso.pch.AST->getSema();

    if (D->isImplicit())
        return nullptr;

    if (!D->isCompleteDefinition() && D->getDefinition())
        D = D->getDefinition();  // WARNING: if the definition isn't in the PCH this is going to submit an empty aggregate decl, there could be conflicts if several PCH or modules are used
    bool isDefined = D->isCompleteDefinition();

    auto TND = D->getTypedefNameForAnonDecl();
    bool isPOD = true;

    int anon = 0;
    if (D->isAnonymousStructOrUnion())
    {
        assert(!TND);

        anon = 1;
        if (D->isUnion())
            anon = 2;
    }

    if (!anon && !getIdentifierOrNull(D))
    {
        fprintf(stderr, "Value with anon record type, no D equivalent (needs workaround), discarding\n");
        return nullptr;
    }

    auto loc = fromLoc(D->getLocation());

    auto CRD = dyn_cast<clang::CXXRecordDecl>(D);
        // NOTE: CXXRecordDecl will disappear in a future version of Clang and only
        // RecordDecl will remain to be used for both C and C++.

    if ((flags & ForceNonPOD) ||
            (CRD && isDefined && !CRD->isPOD()))
        isPOD = false;

    AggregateDeclaration *a;
    if (!anon)
    {
        auto id = getIdentifier(D);

        if (D->isUnion())
        {
            a = new UnionDeclaration(loc, id);
        }
        else if (isPOD)
        {
            a = new StructDeclaration(loc, id, D);
        }
        else
        {
            auto baseclasses = new BaseClasses;

            if (CRD)
            {
                for (auto B = CRD->bases_begin(),
                        BEnd = CRD->bases_end(); B != BEnd; ++B)
                {
                    auto brt = fromType(B->getType());
                    baseclasses->push(new BaseClass(brt,
                                                    toPROT(B->getAccessSpecifier())));
                }
            }

            auto cd = new ClassDeclaration(loc, id, baseclasses, CRD);
            a = cd;
        }

        declMap[D] = a;
    }

    // atm we're sortof mirroring parseAggregate()
    auto members = new Dsymbols;

    if (!isDefined)
        goto Ldeclaration;

    CXXScope.push(D);

    for (auto I = D->field_begin(), E = D->field_end();
            I != E; ++I)
    {
        if (I->getCanonicalDecl() != *I)
            continue;

        auto field = VisitValueDecl(*I);
        if (field)
            members->push(field);
    }

    if (CRD && /* TEMPORARY HACK */ !D->isUnion())
    {
        if (!isPOD && !CRD->isDependentType())
        {
            S.CurContext = Context.getTranslationUnitDecl(); // HACK?

            // Clang declares and defines the implicit default constructor lazily, so do it here
            // before adding methods.
            if (auto CD = S.LookupDefaultConstructor(
                        const_cast<clang::CXXRecordDecl *>(CRD)))
            {
                CD->setTrivial(false); // Force its definition and Sema to act on it
                S.MarkFunctionReferenced(clang::SourceLocation(), CD); // TODO put into NewExp semantic
            }
        }

        for (auto I = CRD->method_begin(), E = CRD->method_end();
            I != E; ++I)
        {
            if (I->getCanonicalDecl() != *I)
                continue;

            auto CCD = dyn_cast<clang::CXXConstructorDecl>(*I);
            if (CCD && CCD->isDefaultConstructor() && isPOD)
                continue;

            // CALYPSO FIXME remove the null check once everything is implemented
            auto fd = VisitFunctionDecl(*I);
            if (fd)
                members->push(fd);
        }
    }

    // Add specific decls: vars, tags, templates, typedefs
#define SPECIFIC_ADD(DECL) \
    typedef clang::DeclContext::specific_decl_iterator<clang::DECL##Decl> DECL##_iterator; \
    for (DECL##_iterator I(D->decls_begin()), E(D->decls_end()); \
                I != E; I++) \
        if (auto s = VisitDecl(*I)) \
            members->push(s);

    SPECIFIC_ADD(Tag)
    SPECIFIC_ADD(Var)
    SPECIFIC_ADD(RedeclarableTemplate)
    SPECIFIC_ADD(TypedefName)

#undef SPECIFIC_ADD

    CXXScope.pop();

Ldeclaration:
    if (anon)
        return new AnonDeclaration(loc, anon == 2, members);

    a->members = members;

    return a;
}

Dsymbol* DeclMapper::VisitTypedefNameDecl(const clang::TypedefNameDecl* D)
{
    if (isAnonTagTypedef(D))
        return nullptr;  // the anon tag will be mapped by VisitRecordDecl to an aggregate named after the typedef identifier

    auto loc = fromLoc(D->getLocation());
    auto id = fromIdentifier(D->getIdentifier());
    auto t = fromType(D->getUnderlyingType());

    auto a = new AliasDeclaration(loc, id, t, D);
    return a;
}

Dsymbol *DeclMapper::VisitFunctionDecl(const clang::FunctionDecl *D)
{
    if (D->isOverloadedOperator() || isa<clang::CXXConversionDecl>(D))
        return nullptr; // TODO

    auto loc = fromLoc(D->getLocation());

    auto FPT = D->getType();
    auto MD = dyn_cast<clang::CXXMethodDecl>(D);
    
    auto tf = fromType(FPT);
    if (!tf)
    {
        fprintf(stderr, "Discarding %s, has a class value argument\n", D->getDeclName().getAsString().c_str());
        return nullptr; // FIXME function with unhandled argument types
    }
    assert(tf->ty == Tfunction);

    StorageClass stc = STCundefined;
    if (MD)
    {
        if (MD->isStatic())
            stc |= STCstatic;

        if (!MD->isVirtual())
            stc |= STCfinal;

        if (MD->isPure())
            stc |= STCabstract;

        if (MD->begin_overridden_methods()
                != MD->end_overridden_methods())
            stc |= STCoverride;

        if (isa<clang::CXXDestructorDecl>(D))
            stc &= ~STCoverride; // dtors aren't added to D's VTBLs
    }
    tf->addSTC(stc);
    
    ::FuncDeclaration *fd = nullptr;
    if (auto CD = dyn_cast<clang::CXXConstructorDecl>(D))
    {
        fd = new CtorDeclaration(loc, stc, tf, CD);
    }
    else if (auto DD = dyn_cast<clang::CXXDestructorDecl>(D))
    {
         // Destructors are a special case, Clang can only emit a destructor if it's not trivial.
        // The dtor is checked and added by buildDtor during the semantic pass.
        if (DD->isImplicit())
            return nullptr;

        fd = new DtorDeclaration(loc, stc, Id::dtor, DD);
    }
    else
    {
        auto id = fromIdentifier(D->getIdentifier());
        fd = new FuncDeclaration(loc, id, stc, tf, D);
    }

    // TODO: operators
    return fd;
}

// NOTE: doesn't return null if the template isn't defined. What we really want is some sort of canonical declaration to refer to for template parameters.
static const clang::ClassTemplateDecl *getDefinition(const clang::ClassTemplateDecl *D)
{
    for (auto RI: D->redecls()) // find the definition if any
    {
        auto I = cast<clang::ClassTemplateDecl>(RI);
        if (I->isThisDeclarationADefinition())
            return I;
    }

    // This is more heuristical than anything else.. I'm not sure yet why templates inside
    // specializations (e.g std::allocator::rebind) do not get defined.
    if (auto MemberTemp = const_cast<clang::ClassTemplateDecl*>(D)->getInstantiatedFromMemberTemplate())
        if (auto MemberDef = getDefinition(MemberTemp))
            return MemberDef;

    return D->getCanonicalDecl();
}

bool isTemplateParameterPack(const clang::NamedDecl *Param)
{
    if (auto NTTPD = dyn_cast<clang::NonTypeTemplateParmDecl>(Param))
        return NTTPD->isParameterPack();
    else if (auto TTPD = dyn_cast<clang::TemplateTypeParmDecl>(Param))
        return TTPD->isParameterPack();
    else if (auto TempTemp = dyn_cast<clang::TemplateTemplateParmDecl>(Param))
        return TempTemp->isParameterPack();

    llvm::llvm_unreachable_internal();
}

Dsymbol *DeclMapper::VisitRedeclarableTemplateDecl(const clang::RedeclarableTemplateDecl *D)
{   TemplateDeclaration *a;

    if (!isa<clang::ClassTemplateDecl>(D) && !isa<clang::TypeAliasTemplateDecl>(D)
         && !isa<clang::FunctionTemplateDecl>(D))
        return nullptr; // temporary

    if (auto FTD = dyn_cast<clang::FunctionTemplateDecl>(D))
    {
        if (FTD->getTemplatedDecl()->isOverloadedOperator())
            return nullptr;

        if (isa<clang::CXXConversionDecl>(FTD->getTemplatedDecl()))
            return nullptr;
    }

    auto loc = fromLoc(D->getLocation());
    auto id = getIdentifier(D);

    if (auto CTD = dyn_cast<clang::ClassTemplateDecl>(D))
        D = CTD = getDefinition(CTD);

    auto tpl = new TemplateParameters;
    auto TPL = D->getTemplateParameters();

    templateParameters.push_back(TPL);

    // FIXME: C++ templates may have multiple parameter packs, which isn't supported by D
    // Since this is a rare occurence ignore them for now
    bool packFound = false;

    for (auto P: *TPL)
    {
        if (packFound)
        {
            ::warning(loc, "Template %s has more than one parameter pack, ignore for now", id->toChars());
            return nullptr;
        }
        packFound = isTemplateParameterPack(P);

        auto tp = VisitTemplateParameter(P);
        tpl->push(tp);
    }

    auto s = VisitDecl(D->getTemplatedDecl()->getCanonicalDecl());

    if (!s)
        return nullptr;

    auto decldefs = new Dsymbols;
    decldefs->push(s);

    templateParameters.pop_back();

    a = new TemplateDeclaration(loc, id, tpl, decldefs, D);
    return a;
}

Identifier *DeclMapper::getIdentifierForTemplateNonTypeParm(const clang::NonTypeTemplateParmDecl *T)
{
    if (auto Id = T->getIdentifier())
        return fromIdentifier(Id);
    else
    {
        ::warning(Loc(), "Generating identifier for anonymous C++ non-type template parameter");

        // This should only ever happen in template param decl mapping
        std::string str;
        llvm::raw_string_ostream OS(str);
        OS << "value_parameter_" << T->getDepth() << '_' << T->getIndex();

        return Lexer::idPool(OS.str().c_str());
    }
}

TemplateParameter *DeclMapper::VisitTemplateParameter(const clang::NamedDecl *Param,
                                                                      const clang::TemplateArgument *SpecArg)
{
    ExprMapper expmap(*this);
    TemplateParameter *tp;

    auto loc = fromLoc(Param->getLocation());
    Identifier *id;

    if (auto NTTPD =
            dyn_cast<clang::NonTypeTemplateParmDecl>(Param))
    {
        id = getIdentifierForTemplateNonTypeParm(NTTPD);
        auto valTy = fromType(NTTPD->getType());

        if (NTTPD->isParameterPack())
        {
            ::warning(Loc(), "%s", "C++ template non-type parameter packs do not strictly map to D tuple parameters");
            return new TemplateTupleParameter(loc, id);
        }
        else
        {
            Expression *tp_specvalue = nullptr;
            Expression *tp_defaultvalue = nullptr;

            if (SpecArg)
            {
                switch (SpecArg->getKind())
                {
                    case clang::TemplateArgument::Expression:
                        tp_specvalue = expmap.fromExpression(SpecArg->getAsExpr());
                        break;
                    case clang::TemplateArgument::Integral:
                        tp_specvalue = expmap.fromAPInt(SpecArg->getAsIntegral());
                        break;
                    case clang::TemplateArgument::NullPtr:
                        tp_specvalue = new NullExp(Loc()/*, fromType(SpecArg->getNullPtrType())*/);
                        break;
                    default:
                        assert(false && "Unsupported template specialization value");
                }

                tp_specvalue = isExpression(FromType(*this).fromTemplateArgument(SpecArg));
                assert(tp_specvalue);
            }

            if (NTTPD->hasDefaultArgument())
            {
//                 tp_defaultvalue = expmap.fromExpression(NTTPD->getDefaultArgument());

                // TEMPORARY HACK: we choose a simple default value to make defaultArg()'s life easier
                //  (there were obscure identifier errors, e.g in __iterator_traits<_normal_iterator>).
                //  This doesn't affect anything other than reflection since the default argument evaluation is done by Clang
                //  We could btw use Clang's evaluation while keeping the mapped expression for reflection
                tp_defaultvalue = new DotIdExp(loc, new TypeExp(loc, valTy), Id::init);
            }

            tp = new TemplateValueParameter(loc, id, valTy,
                                        tp_specvalue, tp_defaultvalue);
        }

    }
    else if (auto TTPD =
            dyn_cast<clang::TemplateTypeParmDecl>(Param))
    {
        id = getIdentifierForTemplateTypeParm(
                cast<clang::TemplateTypeParmType>(TTPD->getTypeForDecl()));

        if (TTPD->isParameterPack())
        {
            ::warning(Loc(), "%s", "C++ template type parameter packs do not strictly map to D tuple parameters");
            return new TemplateTupleParameter(loc, id);
        }
        else
        {
            Type *tp_spectype = nullptr;
            Type *tp_defaulttype = nullptr;

            if (SpecArg)
            {
                tp_spectype = isType(FromType(*this).fromTemplateArgument(SpecArg));
                assert(tp_spectype);
            }

            if (TTPD->hasDefaultArgument())
                tp_defaulttype = fromType(TTPD->getDefaultArgument());

            tp = new TemplateTypeParameter(loc, id, tp_spectype, tp_defaulttype);
        }
    }
    else if (auto TempTemp =
            dyn_cast<clang::TemplateTemplateParmDecl>(Param))
    {
        id = getIdentifierForTemplateTemplateParm(TempTemp);

        if (TempTemp->isParameterPack())
        {
            ::warning(Loc(), "%s", "C++ template template parameter packs do not strictly map to D tuple parameters");
            return new TemplateTupleParameter(loc, id);
        }
        else
        {
            Type *tp_spectype = nullptr;
            Type *tp_defaulttype = nullptr;

            if (SpecArg)
            {
                tp_spectype = isType(FromType(*this).fromTemplateArgument(SpecArg));
                assert(tp_spectype);
            }

            if (TempTemp->hasDefaultArgument())
                tp_defaulttype = FromType(*this).fromTemplateName(
                        TempTemp->getDefaultArgument().getArgument().getAsTemplate());

            tp = new TemplateAliasParameter(loc, id, nullptr, tp_spectype, tp_defaulttype);
        }
    }
    else assert(false && "unrecognized template parameter");

    return tp;
}

Dsymbol *DeclMapper::VisitInstancedClassTemplate(const clang::ClassTemplateSpecializationDecl *D,
                                                 unsigned flags)
{
    assert(!isa<clang::ClassTemplatePartialSpecializationDecl>(D));
    auto CT = getDefinition(D->getSpecializedTemplate());

    templateParameters.push_back(CT->getTemplateParameters());
    auto s = VisitRecordDecl(D, flags);
    templateParameters.pop_back();

    return s;
}

static const clang::ClassTemplateSpecializationDecl *getDefinition(const clang::ClassTemplateSpecializationDecl *D)
{
    if (auto Definition = D->getDefinition())
        return cast<clang::ClassTemplateSpecializationDecl>(Definition);

    if (auto Partial = dyn_cast<clang::ClassTemplatePartialSpecializationDecl>(D))
        if (auto MemberInst = const_cast<clang::ClassTemplatePartialSpecializationDecl*>(Partial)->getInstantiatedFromMember()) // not the same method name..
            if (auto MemberDef = getDefinition(MemberInst))
                return MemberDef;

    return D;
}

// Explicit specializations only
Dsymbol *DeclMapper::VisitClassTemplateSpecializationDecl(const clang::ClassTemplateSpecializationDecl *D)
{   TemplateDeclaration *a;

    D = getDefinition(D);

    if (!D->isExplicitSpecialization())
        return nullptr;

    auto Partial = dyn_cast<clang::ClassTemplatePartialSpecializationDecl>(D);
        // NOTE: D's partial specializations != C++'s partial specializations
        // The mapping provides a "close" but not exact approximation of equivalent template specs in D (for reflection),
        // but TemplateDeclaration::findBestMatch is skipped since the choice is done by Clang anyway.
        //
        // C++ template decl lookup doesn't exactly work like D's. For example:
        //
        //    template<typename _Iterator>
        //    struct __iterator_traits<_Iterator, true> {};
        //
        // In C++ you can refer to that partial spec either by __iterator_traits<iterator> or __iterator_traits<iterator, true>
        // whereas in D only __iterator_traits<iterator> works.
        // Furthermore in C++ you may have multiple partial specs taking the same number of seemingly identical parameters.
        // it's not always possible to work around that here.

    auto loc = fromLoc(D->getLocation());
    auto id = fromIdentifier(D->getIdentifier());

    auto tpl = new TemplateParameters;

    auto CT = getDefinition(D->getSpecializedTemplate());
    auto TPL = CT->getTemplateParameters();
    auto AI = D->getTemplateArgs().asArray().begin();

    if (Partial)
    {
        TPL = Partial->getTemplateParameters(); // SEMI-HACK #1 because we alter the tiargs to match the partial spec params
        AI = nullptr; // SEMI-HACK #2 because partial spec args won't matter during semantic
    }
    templateParameters.push_back(TPL);

    for (auto PI = TPL->begin(), PE = TPL->end();
        PI != PE; PI++)
    {
        auto tp = VisitTemplateParameter(*PI, AI);
        tpl->push(tp);

        if (AI) AI++;
    }

    auto decldefs = new Dsymbols;
    auto ad = VisitRecordDecl(D);
    decldefs->push(ad);

    templateParameters.pop_back();

    a = new TemplateDeclaration(loc, id, tpl, decldefs, D);
    return a;
}

Dsymbol* DeclMapper::VisitEnumDecl(const clang::EnumDecl* D)
{
    if (!D->isCompleteDefinition() && D->getDefinition())
        D = D->getDefinition();

    auto loc = fromLoc(D->getLocation());
    auto ident = getIdentifierOrNull(D);

    Type *memtype = nullptr;
    if (!D->isDependentType())
    {
        auto IntType = D->getIntegerType();
        if (IntType.isNull())
            IntType = D->getPromotionType();

        memtype = fromType(IntType);
    }

    auto e = new EnumDeclaration(loc, ident, memtype, D);

    for (auto ECD: D->enumerators())
    {
        if (!e->members)
            e->members = new Dsymbols;

        auto ident = fromIdentifier(ECD->getIdentifier());
        Expression *value = nullptr;

        if (auto InitE = ECD->getInitExpr())
            value = ExprMapper(*this).fromExpression(InitE);

        auto em = new EnumMember(loc, ident, value, nullptr);
        e->members->push(em);
    }

    return e;
}

/*****/

std::string moduleName(Identifiers *packages, Identifier *ident)
{
    std::string result = "__cpp/";
    for (size_t i = 1; i < packages->dim; i++)
    {
        Identifier *pid = (*packages)[i];
        result.append(pid->string, pid->len);
        result.append("/");
    }
    result.append(ident->string, ident->len);
    return result;
}

// Look into namespace redecls if there are any
clang::DeclContext::lookup_const_result wideLookup(Loc loc,
                                                   const clang::DeclContext *DC,
                                                   Identifier *id,
                                                   bool lookupInRedecls = true)
{
    auto& AST = calypso.pch.AST;
    auto& Table = AST->getPreprocessor().getIdentifierTable();
    auto& II = Table.get(id->string);

    if (lookupInRedecls)
        if (auto NS = dyn_cast<clang::NamespaceDecl>(DC))
        {
            for (auto Redecl: NS->redecls())
            {
                auto R = wideLookup(loc, Redecl, id, false);
                if (!R.empty())
                    return R;
            }
        }

    typedef clang::DeclContext::specific_decl_iterator<clang::LinkageSpecDecl>
        linkagespec_iterator;
    linkagespec_iterator LSI(DC->decls_begin()), LSE(DC->decls_end());

    do
    {
        auto R = DC->lookup(clang::DeclarationName(&II));

        if (!R.empty())
            return R;

        if (LSI != LSE)
        {
            DC = *LSI;
            LSI++;
        }
        else
            DC = nullptr;

    } while(DC);

    return clang::DeclContext::lookup_const_result();
}

// HACK: The C++ "module loading" works more like a hack at the moment.
// Clang's C++ modules being currently « very experimental and broken », using Clang's module system
// would add many further obstacles to get Calypso working, so I've decided to stick to one big PCH for the
// time being. Once Calypso is working fixing C++ modules should be a TODO.

Module *Module::load(Loc loc, Identifiers *packages, Identifier *id)
{
    auto& Context = calypso.getASTContext();

    const clang::DeclContext *DC = Context.getTranslationUnitDecl();
    auto dst = rootPackage->symtab;

    assert(packages && packages->dim);

    for (size_t i = 1; i < packages->dim; i++)
    {
        Identifier *pid = (*packages)[i];

        auto R = wideLookup(loc, DC, pid);
        if (R.empty())
        {
            ::error(loc, "no C++ package named %s", pid->toChars());
            fatal();
        }

        auto NSN = dyn_cast<clang::NamespaceDecl>(R[0]);
        if (!NSN)
        {
            ::error(loc, "only namespaces can be C++ packages");
            fatal();
        }
        auto ident = fromIdentifier(NSN->getIdentifier());

        auto pkg = static_cast<Package*>(dst->lookup(ident));
        assert(pkg);

        DC = NSN;
        dst = pkg->symtab;
    }

    auto m = new Module(moduleName(packages, id).c_str(),
                        id, packages);
    m->members = new Dsymbols;
    m->loc = loc;

    DeclMapper mapper(m);

    // HACK « hardcoded modules »
    if (strcmp(id->string, "_") == 0)
    {
        // All non-tag declarations inside the namespace go in _ (this is horrible for C functions of course, this will be fixed by the switch to Clang module system)
        auto NS = dyn_cast<clang::NamespaceDecl>(DC);

        if (!NS)
        {
            warning(Loc(), "package isn't a C++ namespace yet module is '_', ignoring this module for now (testing, will add TU later)");
//             warning(Loc(), "_ identifiers will lose their special meaning after the switch to Clang's module system");
        }
        else
        {
            m->rootDecl = NS->getCanonicalDecl();

            auto I = NS->redecls_begin(),
                    E = NS->redecls_end();

            for (; I != E; ++I)
            {
                DC = *I;

                auto D = DC->decls_begin(),
                        DE = DC->decls_end();

                for (; D != DE; ++D)
                {
                    if (llvm::isa<clang::FunctionDecl>(*D) ||
                            llvm::isa<clang::VarDecl>(*D) ||
                            llvm::isa<clang::TypedefNameDecl>(*D) ||
                            llvm::isa<clang::TypeAliasTemplateDecl>(*D))
                    {
                        if (auto s = mapper.VisitDecl(*D))
                            m->members->push(s);
                    }
                }
            }
        }
    }
    else
    {
        auto R = wideLookup(loc, DC, id);
        if (R.empty())
        {
            ::error(loc, "no C++ module named %s", id->toChars());
            fatal();
        }

        auto D = R[0];
        if (auto TD = dyn_cast<clang::TypedefNameDecl>(D))
        {
            auto UT = TD->getUnderlyingType().getDesugaredType(Context);
            if (auto RT = dyn_cast<clang::TagType>(UT))
                D = RT->getDecl();
        }

        // Module must be a record or enum
        if (!isa<clang::TagDecl>(D) &&
            !isa<clang::ClassTemplateDecl>(D))
        {
            ::error(loc, "C++ modules have to be records (class/struct, template or not) or enums");
            fatal();
        }

        D = cast<clang::NamedDecl>(D->getCanonicalDecl());
        m->rootDecl = D;

        if (auto s = mapper.VisitDecl(D))
            m->members->push(s);

        // Special case for class template, we need to add explicit specializations to the module as well
        if (auto CTD = dyn_cast<clang::ClassTemplateDecl>(D))
        {
            llvm::SmallVector<clang::ClassTemplatePartialSpecializationDecl *, 2> PS;
            CTD->getPartialSpecializations(PS);

            for (auto PartialSpec: PS)
                if (auto s = mapper.VisitDecl(PartialSpec->getCanonicalDecl()))
                    m->members->push(s);

            for (auto Spec: CTD->specializations())
                if (auto s = mapper.VisitDecl(Spec->getCanonicalDecl()))
                    m->members->push(s);
        }

//         srcFilename = AST->getSourceManager().getFilename(TD->getLocation());
    }
    
    amodules.push_back(m);
    dst->insert(m);
    return m;
}

}
