// Contributed by Elie Morisse, same license DMD uses

#include "../aggregate.h"
#include "../attrib.h"
#include "../declaration.h"
#include "../enum.h"
#include "../identifier.h"
#include "../import.h"
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

DsymbolTable *Module::modules;
Modules Module::amodules;

void Module::init()
{
    modules = new DsymbolTable();
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
    for (size_t i = 0; i < packages->dim; i++)
    {
        Identifier *pid = (*packages)[i];
        combine(objfn, pid);
    }
    combine(objfn, ident);

    arg = objfn;
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
{   Dsymbol *s = nullptr;

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
    DECL(Template)
    DECL(Enum)
    DECL(Value)

#undef DECL

    return s;
}

Dsymbol *DeclMapper::VisitValueDecl(const clang::ValueDecl *D)
{
    auto& Context = calypso.getASTContext();

    auto loc = toLoc(D->getLocation());
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

    auto id = toIdentifier(D->getIdentifier());
    auto t = toType(D->getType());

    if (!t)
        return nullptr;

    auto a = new VarDeclaration(loc, id, D, t);

    if (auto Var = dyn_cast<clang::VarDecl>(D))
    {
        if (Var->hasExternalStorage())
            a->storage_class |= STCextern;

        if (Var->getTLSKind() == clang::VarDecl::TLS_Dynamic)
            a->storage_class |= STCtls;
    }
    return a;
}

Dsymbol *DeclMapper::VisitRecordDecl(const clang::RecordDecl *D, unsigned flags)
{   AggregateDeclaration *a;
    auto& Context = calypso.pch.AST->getASTContext();
    auto& S = calypso.pch.AST->getSema();
    auto TND = D->getTypedefNameForAnonDecl();
    bool isPOD = true;

    if (!D->isThisDeclarationADefinition())
        return nullptr;

    int anon = 0;
    if (D->isAnonymousStructOrUnion())
    {
        assert(!TND);

        anon = 1;
        if (D->isUnion())
            anon = 2;
    }

    // If the definition isn't in the PCH we will still submit an empty aggregate decl

    auto loc = toLoc(D->getLocation());

    auto CRD = dyn_cast<clang::CXXRecordDecl>(D);
        // NOTE: CXXRecordDecl will disappear in a future version of Clang and only
        // RecordDecl will remain to be used for both C and C++.

    if ((flags & ForceNonPOD) ||
            (CRD && !CRD->isPOD()))
        isPOD = false;

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
                    auto brt = toType(B->getType());
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
        if (!isPOD && !CRD->getDescribedClassTemplate())
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

    // Add specific decls: enums, typedefs
    typedef clang::DeclContext::specific_decl_iterator<clang::EnumDecl> enum_iterator;
    for (enum_iterator I(D->decls_begin()), E(D->decls_end());
                I != E; I++)
        if (!I->getTypedefNameForAnonDecl())
            members->push(VisitDecl(*I));

    typedef clang::DeclContext::specific_decl_iterator<clang::TypedefDecl> typedef_iterator;
    for (typedef_iterator I(D->decls_begin()), E(D->decls_end());
                I != E; I++)
        members->push(VisitDecl(*I));

    if (anon)
        return new AnonDeclaration(loc, anon == 2, members);

    a->members = members;

    return a;
}

// TODO translate C++-0x's "using Alias = type;" to alias declarations.
Dsymbol* DeclMapper::VisitTypedefNameDecl(const clang::TypedefNameDecl* D)
{
    auto& Context = calypso.getASTContext();

    if (auto TagTy = dyn_cast<clang::TagType>
            (D->getUnderlyingType().getDesugaredType(Context)))
    {
        // special case for typedef class/struct/enum { ...anon record... } SymbolName
        auto Tag = TagTy->getDecl();

        if (Tag->getTypedefNameForAnonDecl())
            return VisitDecl(Tag);
    }

    auto loc = toLoc(D->getLocation());
    auto id = toIdentifier(D->getIdentifier());
    auto t = toType(D->getUnderlyingType());

    auto a = new AliasDeclaration(loc, id, t);
    return a;
}

Dsymbol *DeclMapper::VisitFunctionDecl(const clang::FunctionDecl *D)
{
    auto loc = toLoc(D->getLocation());

    auto FPT = cast<clang::FunctionProtoType>(D->getType().getTypePtr());
    auto MD = dyn_cast<clang::CXXMethodDecl>(D);
    
    auto tf = toTypeFunction(FPT);
    if (!tf)
    {
        fprintf(stderr, "Discarding %s, has a class value argument\n", D->getDeclName().getAsString().c_str());
        return nullptr; // FIXME function with unhandled argument types
    }

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
        if (!D->getIdentifier())
        {
//             fprintf(stderr, "No id, operator? support soon\n");
            return nullptr;
        }
        
        auto id = toIdentifier(D->getIdentifier());
        fd = new FuncDeclaration(loc, id, stc, tf, D);
    }

    // TODO: operators
    return fd;
}

Dsymbol *DeclMapper::VisitTemplateDecl(const clang::TemplateDecl *D)
{   TemplateDeclaration *a;

    auto loc = toLoc(D->getLocation());
    auto id = toIdentifier(D->getIdentifier());

    auto tpl = new TemplateParameters;
    auto TPL = D->getTemplateParameters();

    for (auto P: *TPL)
    {
        auto tp = VisitTemplateParameter(P);

        if (!tp)    // FIXME temporary
            return nullptr;

        tpl->push(tp);
    }

    Dsymbol *s;
    if (auto CTD =  dyn_cast<clang::ClassTemplateDecl>(D))
    {
        if (!CTD->isThisDeclarationADefinition())
            return nullptr;

        s = VisitRecordDecl(CTD->getTemplatedDecl());
    }
    else if (auto FTD = dyn_cast<clang::FunctionTemplateDecl>(D))
    {
//         if (!FTD->isThisDeclarationADefinition())
            return nullptr;

//         s = VisitFunctionDecl(FTD->getTemplatedDecl());
    }

    auto decldefs = new Dsymbols;
    decldefs->push(s);

    a = new TemplateDeclaration(loc, id, tpl, nullptr, decldefs, false);
    return a;
}

// Since D doesn't accept anonymous template parameters, unless we complexify the language spec just for C++ it's necessary to generate identifiers.
// We look for one that is unused in the current scope.
static Identifier *genAnonIdentifier(const char *base, Scope *sc)
{
    static unsigned suffix = 0;  // FIXME
    suffix++;
    if (suffix > 1000)
        suffix = 1;

//     for (unsigned suffix = 1; ; suffix++)
    {
        std::string str(base);
        str.append(std::to_string(suffix));

        auto ident = Lexer::idPool(str.c_str());
//         if (!sc->search(Loc(), ident, nullptr))
            return ident;
    }
}

TemplateParameter *DeclMapper::VisitTemplateParameter(const clang::NamedDecl *Param,
                                                                      const clang::TemplateArgument *SpecArg)
{
    ExprMapper expmap(*this);

    auto loc = toLoc(Param->getLocation());
    Identifier *id;
    if (auto II = Param->getIdentifier())
        id = toIdentifier(II);
    else
        id = genAnonIdentifier("__tp_", sc); // FIXME there can't be a scope before semantic

    TemplateParameter *tp;

    if (auto NTTPD =
            dyn_cast<clang::NonTypeTemplateParmDecl>(Param))
    {
        auto valTy = toType(NTTPD->getType());

        if (NTTPD->isParameterPack())
        {
            // tuple
            ::error(Loc(), "%s", "C++ template parameter pack not supported yet");
            return nullptr;
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
                        tp_specvalue = expmap.toExpression(SpecArg->getAsExpr());
                        break;
                    case clang::TemplateArgument::Integral:
                        tp_specvalue = APIntToExpression(SpecArg->getAsIntegral());
                        break;
                    case clang::TemplateArgument::NullPtr:
                        tp_specvalue = new NullExp(Loc()/*, toType(SpecArg->getNullPtrType())*/);
                        break;
                    default:
                        assert(false && "Unsupported template specialization value");
                }
                assert(tp_specvalue);
            }

            if (NTTPD->hasDefaultArgument())
                tp_defaultvalue = expmap.toExpression(NTTPD->getDefaultArgument());

            tp = new TemplateValueParameter(loc, id, valTy,
                                        tp_specvalue, tp_defaultvalue);
        }

    }
    else if (auto TTPD =
            dyn_cast<clang::TemplateTypeParmDecl>(Param))
    {
        if (TTPD->isParameterPack())
        {
            ::error(Loc(), "%s", "C++ template parameter pack not supported yet");
            return nullptr;
        }
        else
        {
            Type *tp_spectype = nullptr;
            Type *tp_defaulttype = nullptr;

            if (SpecArg)
            {
                assert(SpecArg->getKind() == clang::TemplateArgument::Type);
                tp_spectype = toType(SpecArg->getAsType());
            }

            if (TTPD->hasDefaultArgument())
                tp_defaulttype = toType(TTPD->getDefaultArgument());

            tp = new TemplateTypeParameter(loc, id, tp_spectype, tp_defaulttype);
        }
    }
    else assert(false && "unrecognized template parameter");

    return tp;
}

// Explicit specializations only
Dsymbol *DeclMapper::VisitClassTemplateSpecializationDecl(const clang::ClassTemplateSpecializationDecl *D)
{   TemplateDeclaration *a;

    if (!D->isThisDeclarationADefinition())
        return nullptr;

    if (!D->isExplicitSpecialization())
        return nullptr; // not sure about that

    if (isa<clang::ClassTemplatePartialSpecializationDecl>(D))
        return nullptr; // TODO

    auto loc = toLoc(D->getLocation());
    auto id = toIdentifier(D->getIdentifier());

    auto tpl = new TemplateParameters;

    auto CT = D->getDescribedClassTemplate();
    auto TPL = CT->getTemplateParameters();
    auto& TAL = D->getTemplateArgs();

    assert(TPL->size() == TAL.size());

    auto AI = TAL.asArray().begin();
    for (auto PI = TPL->begin(), PE = TPL->end();
        PI != PE; PI++, AI++)
    {
        auto tp = VisitTemplateParameter(*PI, AI);

        if (!tp)
            return nullptr;

        tpl->push(tp);
    }

    auto decldefs = new Dsymbols;
    auto ad = VisitRecordDecl(D);
    decldefs->push(ad);

    a = new TemplateDeclaration(loc, id, tpl, nullptr, decldefs);
    return a;
}

Dsymbol* DeclMapper::VisitEnumDecl(const clang::EnumDecl* D)
{
    if (!D->isCompleteDefinition())
        return nullptr;

    auto loc = toLoc(D->getLocation());
    auto ident = getIdentifierOrNull(D);

    auto IntType = D->getIntegerType();
    if (IntType.isNull())
        IntType = D->getPromotionType();

    auto e = new EnumDeclaration(loc, ident, toType(IntType));

    for (auto ECD: D->enumerators())
    {
        if (!e->members)
            e->members = new Dsymbols;

        auto ident = toIdentifier(ECD->getIdentifier());
        Expression *value = nullptr;

        if (auto InitE = ECD->getInitExpr())
            value = ExprMapper(*this).toExpression(InitE);

        auto em = new EnumMember(loc, ident, value, nullptr);
        e->members->push(em);
    }

    return e;
}

/*****/

std::string moduleName(Identifiers *packages, Identifier *ident)
{
    std::string result = "__cpp/";
    for (size_t i = 0; i < packages->dim; i++)
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

//     typedef clang::DeclContext::specific_decl_iterator<clang::LinkageSpecDecl>
//         linkagespec_iterator;
//     linkagespec_iterator LSI(DC->decls_begin()), LSE(DC->decls_end());

    do
    {
        auto R = DC->lookup(clang::DeclarationName(&II));

        if (!R.empty())
            return R;

//         if (LSI != LSE)
//         {
//             DC = *LSI;
//             LSI++;
//         }
//         else
            DC = nullptr;

    } while(DC);

    return clang::DeclContext::lookup_const_result();
}

// HACK: The C++ "module loading" works more like a hack at the moment.
// Clang's C++ modules being currently « very experimental and broken », using Clang's module system
// would add many further obstacles to get Calypso working, so I've decided to stick to one big PCH for the
// time being. Once Calypso is working fixing C++ modules should be a TODO.

Module *Module::load(Loc loc, Identifiers *packages, Identifier *ident)
{
    auto& Context = calypso.getASTContext();

    const clang::DeclContext *DC = Context.getTranslationUnitDecl();
    auto dst = modules;

    if (packages && packages->dim)
    {
        for (size_t i = 0; i < packages->dim; i++)
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

            DC = NSN;

            if (auto NDC = dyn_cast<clang::NamedDecl>(DC))
            {
                auto id = toIdentifier(NDC->getIdentifier());

                auto pkg = static_cast<Package*>(dst->lookup(id));
                if (!pkg)
                {
                    pkg = new Package(id);
                    pkg->symtab = new DsymbolTable();

                    dst->insert(pkg);
                }

                dst = pkg->symtab;
            }
        }
    }

    auto m = new Module(moduleName(packages, ident).c_str(),
                        ident, packages);
    m->members = new Dsymbols;
    m->loc = loc;

    DeclMapper mapper(m);

    // HACK « hardcoded modules »
    if (strcmp(ident->string, "_") == 0)
    {
        // All non-tag declarations inside the namespace go in _ (this is horrible for C functions of course, this will be fixed by the switch to Clang module system)
        auto NS = dyn_cast<clang::NamespaceDecl>(DC);

        if (!NS)
        {
            warning(Loc(), "package isn't a C++ namespace yet module is '_', ignoring this module for now (testing, will add TU later)");
            warning(Loc(), "_ identifiers will lose their special meaning after the switch to Clang's module system");
        }
        else
        {
            m->rootDecl = NS->getCanonicalDecl();

            auto I = NS->redecls_begin(),
                    E = NS->redecls_end();

            for (; I != E; ++I)
            {
                auto D = DC->decls_begin(),
                        DE = DC->decls_end();

                for (; D != DE; ++D)
                {
                    if (llvm::isa<clang::FunctionDecl>(*D) ||
                            llvm::isa<clang::VarDecl>(*D))
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
        auto R = wideLookup(loc, DC, ident);
        if (R.empty())
        {
            ::error(loc, "no C++ module named %s", ident->toChars());
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

        m->rootDecl = D->getCanonicalDecl();

        if (auto s = mapper.VisitDecl(R[0]))
            m->members->push(s);
//         srcFilename = AST->getSourceManager().getFilename(TD->getLocation());
    }
    
    amodules.push_back(m);
    modules->insert(m);
    return m;
}

}
