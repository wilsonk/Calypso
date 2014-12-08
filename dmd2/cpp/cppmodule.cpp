// Contributed by Elie Morisse, same license DMD uses

#include "../aggregate.h"
#include "../declaration.h"
#include "../enum.h"
#include "../identifier.h"
#include "../import.h"
#include "../lexer.h"
#include "../template.h"
#include "id.h"

#include "calypso.h"
#include "cppmodule.h"
#include "cppdeclaration.h"
#include "cppimport.h"
#include "cppaggregate.h"

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

namespace
{

class Mapper : public TypeMapper
{
public:
    Mapper(Module *mod);

    inline PROT toPROT(clang::AccessSpecifier AS);

    // Declarations
    Dsymbols *VisitDeclContext(const clang::DeclContext *DC);

    Dsymbol *VisitDecl(const clang::Decl *D);

    Dsymbol *VisitValueDecl(const clang::ValueDecl *D);
    Dsymbol *VisitRecordDecl(const clang::RecordDecl *D);
    Dsymbol *VisitTypedefDecl(const clang::TypedefDecl *D);
    Dsymbol *VisitFunctionDecl(const clang::FunctionDecl *D);
    Dsymbol *VisitTemplateDecl(const clang::TemplateDecl *D);
    Dsymbol *VisitEnumDecl(const clang::EnumDecl *D);

    TemplateParameter *VisitTemplateParameter(const clang::NamedDecl *D);

//     Dsymbol *VisitEnumConstantDecl(const clang::EnumConstantDecl *D);
//     Dsymbol *VisitEmptyDecl(const clang::EmptyDecl *D);
//     Dsymbol *VisitFriendDecl(const clang::FriendDecl *D);
//     Dsymbol *VisitLabelDecl(const clang::LabelDecl *D);
//     Dsymbol *VisitParmVarDecl(const clang::ParmVarDecl *D);
//     Dsymbol *VisitFileScopeAsmDecl(const clang::FileScopeAsmDecl *D);
//     Dsymbol *VisitImportDecl(const clang::ImportDecl *D);
//     Dsymbol *VisitStaticAssertDecl(const clang::StaticAssertDecl *D);
//     Dsymbol *VisitNamespaceDecl(const clang::NamespaceDecl *D);
//     Dsymbol *VisitUsingDirectiveDecl(const clang::UsingDirectiveDecl *D);
//     Dsymbol *VisitNamespaceAliasDecl(const clang::NamespaceAliasDecl *D);
//     Dsymbol *VisitLinkageSpecDecl(const clang::LinkageSpecDecl *D);
//     Dsymbol *VisitFunctionTemplateDecl(const clang::FunctionTemplateDecl *D);
//     Dsymbol *VisitClassTemplateDecl(const clang::ClassTemplateDecl *D);
//     Dsymbol *VisitUnresolvedUsingTypenameDecl(const clang::UnresolvedUsingTypenameDecl *D);
//     Dsymbol *VisitUnresolvedUsingValueDecl(const clang::UnresolvedUsingValueDecl *D);
};


/*****/

Mapper::Mapper(Module* mod)
    : TypeMapper(mod)
{
}

inline PROT Mapper::toPROT(clang::AccessSpecifier AS)
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

Dsymbols *Mapper::VisitDeclContext(const clang::DeclContext *DC)
{    Dsymbols *decldefs = new Dsymbols();

    for (clang::DeclContext::decl_iterator D = DC->decls_begin(), DEnd = DC->decls_end();
        D != DEnd; ++D)
        decldefs->push(VisitDecl(*D));

    return decldefs;
}

Dsymbol *Mapper::VisitDecl(const clang::Decl *D)
{   Dsymbol *s = nullptr;

    // Unfortunately a long ugly list of if (... dyn_cast...) is more solid and
    // future-proof than a pretty switch à la decl_visitor

#define DECL(BASE) \
    else if (const clang::BASE##Decl *BASE##D = \
                            llvm::dyn_cast<clang::BASE##Decl>(D)) \
        s = Visit##BASE##Decl(BASE##D);

    if (0) ;
//     DECL(Value)
    DECL(Typedef)
    DECL(Record)
    DECL(Function)
//     DECL(Template)
    DECL(Enum)

#undef DECL

    return s;
}

Dsymbol *Mapper::VisitValueDecl(const clang::ValueDecl *D)
{
    auto loc = toLoc(D->getLocation());
    auto t = toType(D->getType());
    auto id = toIdentifier(D->getIdentifier());

    return new VarDeclaration(loc, id, D, t);
}

// // Translate typedefs and C++-0x's "using Alias = type;" to alias declarations.
// Dsymbol *Mapper::VisitTypedefDecl(const clang::TypedefDecl *D)
// {   AliasDeclaration *a;
//     Loc *loc = toLoc(D->getLocation());
//     Type *t = toType(D->getUnderlyingType());
//     Identifier *id = toIdentifier(D->getIdentifier());
//
//     a = new AliasDeclaration(loc, id, t);
//
//     return a;
// }
//
Dsymbol *Mapper::VisitRecordDecl(const clang::RecordDecl *D)
{   AggregateDeclaration *a;
    auto& Context = calypso.pch.AST->getASTContext();
    auto& S = calypso.pch.AST->getSema();
    bool isPOD = true;
    bool isDefined = D->getDefinition();

    if (isDefined && !D->isCompleteDefinition())
        return nullptr;

    // If the definition isn't in the PCH we will still submit an empty aggregate decl

    auto loc = toLoc(D->getLocation());
    auto id = toIdentifier(D->getIdentifier());

    auto CRD = llvm::dyn_cast<clang::CXXRecordDecl>(D);
        // NOTE: CXXRecordDecl will disappear in a future version of Clang and only
        // RecordDecl will remain to be used for both C and C++.

    if (CRD && !CRD->isPOD())
        isPOD = false;

    if (isPOD)
    {
        a = new StructDeclaration(loc, id, D);
    }
    else
    {
        auto baseclasses = new BaseClasses;

        for (auto B = CRD->bases_begin(),
                BEnd = CRD->bases_end(); B != BEnd; ++B)
        {
            auto BRT = llvm::cast<clang::RecordType>(B->getType().getTypePtr());
            auto BRD = BRT->getDecl();
            auto brt = new TypeIdentifier(loc,
                                          toIdentifier(BRD->getIdentifier()));

            AddImplicitImportForDecl(BRD);
            baseclasses->push(new BaseClass(brt,
                                            toPROT(B->getAccessSpecifier())));
        }

        auto cd = new ClassDeclaration(loc, id, baseclasses, CRD);
        a = cd;
    }
    
    declMap[D] = a;

    if (isDefined)
    {
        // atm we're sortof mirroring parseAggregate()
        auto members = new Dsymbols;

        for (auto I = D->field_begin(), E = D->field_end();
             I != E; ++I)
        {
            if (I->getCanonicalDecl() != *I)
                continue;

            members->push(VisitValueDecl(*I));
        }

        if (CRD)
        {
            if (!isPOD)
            {
                // Clang declares and defines the implicit default constructor lazily, so do it here
                // before adding methods.
                auto CD = S.LookupDefaultConstructor(
                            const_cast<clang::CXXRecordDecl *>(CRD));
                S.MarkFunctionReferenced(clang::SourceLocation(), CD); // TODO put into NewExp semantic
            }

            for (auto I = CRD->method_begin(), E = CRD->method_end();
                I != E; ++I)
            {
                if (I->getCanonicalDecl() != *I)
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
            members->push(VisitDecl(*I));

        typedef clang::DeclContext::specific_decl_iterator<clang::TypedefDecl> typedef_iterator;
        for (typedef_iterator I(D->decls_begin()), E(D->decls_end());
                    I != E; I++)
            members->push(VisitDecl(*I));

        a->members = members;
    }

    return a;
}

Dsymbol* Mapper::VisitTypedefDecl(const clang::TypedefDecl* D)
{
    auto loc = toLoc(D->getLocation());
    auto id = toIdentifier(D->getIdentifier());
    auto t = toType(D->getUnderlyingType());

    auto a = new AliasDeclaration(loc, id, t);
    return a;
}

Dsymbol *Mapper::VisitFunctionDecl(const clang::FunctionDecl *D)
{
    auto loc = toLoc(D->getLocation());

    auto FPT = llvm::cast<clang::FunctionProtoType>(D->getType().getTypePtr());
    auto MD = llvm::dyn_cast<clang::CXXMethodDecl>(D);
    
    auto tf = toTypeFunction(FPT);
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
    if (auto CD = llvm::dyn_cast<clang::CXXConstructorDecl>(D))
    {
        fd = new CtorDeclaration(loc, stc, tf, CD);
    }
    else if (auto DD = llvm::dyn_cast<clang::CXXDestructorDecl>(D))
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
            fprintf(stderr, "No id, operator? support soon\n");
            return nullptr;
        }
        
        auto id = toIdentifier(D->getIdentifier());
        fd = new FuncDeclaration(loc, id, stc, tf, D);
    }

    // TODO: operators
    return fd;
}

// Dsymbol *Mapper::VisitTemplateDecl(const clang::TemplateDecl *D)
// {   TemplateDeclaration *a;
//
//     Loc *loc = toLoc(D->getLocation());
//     Identifier *id = toIdentifier(D->getIdentifier());
//
//     TemplateParameters *tpl = new TemplateParameters();
//     auto TPL = D->getTemplateParameters();
//
//     for (auto P = TPL->begin(),
//                 PEnd = TPL->end(); P != PEnd; ++P)
//     {
//         TemplateParameter *tp = VisitTemplateParameter(*P);
//         tpl->push(tp);
//     }
//
//     Dsymbol *s;
//     if (auto CTD =
//                     llvm::dyn_cast<clang::ClassTemplateDecl>(D))
//     {
//         if (!CTD->isThisDeclarationADefinition())
//             return nullptr;
//
//         s = VisitRecordDecl(CTD->getTemplatedDecl());
//     }
//     else if (auto FTD =
//                     llvm::dyn_cast<clang::FunctionTemplateDecl>(D))
//     {
//         if (!FTD->isThisDeclarationADefinition())
//             return nullptr;
//
//         s = VisitFunctionDecl(FTD->getTemplatedDecl());
//     }
//
//     Dsymbols *decldefs = new Dsymbols();
//     decldefs->push(s);
//
//     a = new TemplateDeclaration(loc, id, tpl, nullptr, decldefs, 0);
//
//     return a;
// }
//
// TemplateParameter *Mapper::VisitTemplateParameter(const clang::NamedDecl *D)
// {   TemplateParameter *tp;
//
//     Loc *loc = toLoc(D->getLocation());
//     Identifier *tp_ident = toIdentifier(D->getIdentifier());
//
//     if (const clang::NonTypeTemplateParmDecl *NTTPD =
//             llvm::dyn_cast<clang::NonTypeTemplateParmDecl>(D))
//     {
//         Type *tp_valtype = toType(NTTPD->getType());
//
//         if (NTTPD->isParameterPack())
//         {
//             // tuple
//         }
//         else
//         {
//             Expression *tp_defaultvalue = nullptr;
//
//             if (NTTPD->hasDefaultArgument())
//                 tp_defaultvalue = toExpression(NTTPD->getDefaultArgument());
//
//                 // NOTE: the default argument isn't really needed though
//
//             tp = new TemplateValueParameter(loc, tp_ident, tp_valtype,
//                                         nullptr, tp_defaultvalue);
//         }
//
//     }
//     else if (const clang::TemplateTypeParmDecl *TTPD =
//             llvm::dyn_cast<clang::TemplateTypeParmDecl>(D))
//     {
//         if (TTPD->isParameterPack())
//         {
//
//         }
//         else
//         {
//             Type *tp_defaulttype = nullptr;
//
//             if (TTPD->hasDefaultArgument())
//                 tp_defaulttype = toType(TTPD->getDefaultArgument());
//
//             tp = new TemplateTypeParameter(loc, tp_ident, nullptr, tp_defaulttype);
//         }
//     }
//     else assert(false && "unrecognized template parameter");
//
//     return tp;
// }

Type *getAPIntDType(const llvm::APInt &i)
{
    bool needs64bits = i.getBitWidth() > 32;

    if (i.isNegative())
        return needs64bits ? Type::tint64 : Type::tint32;
    else
        return needs64bits ? Type::tuns64 : Type::tuns32;
}

Dsymbol* Mapper::VisitEnumDecl(const clang::EnumDecl* D)
{
    auto loc = toLoc(D->getLocation());
    auto id = toIdentifier(D->getIdentifier());

    auto IntType = D->getIntegerType();
    if (IntType.isNull())
        IntType = D->getPromotionType();

    auto e = new EnumDeclaration(loc, id, toType(IntType));

    for (auto ECD: D->enumerators())
    {
        if (!e->members)
            e->members = new Dsymbols;

        auto ident = toIdentifier(ECD->getIdentifier());
        Expression *value = nullptr;

        if (ECD->getInitExpr())
        {
            auto InitVal = ECD->getInitVal();
            auto t = getAPIntDType(InitVal);

            value = new IntegerExp(loc, InitVal.isNegative() ? InitVal.getSExtValue() : InitVal.getZExtValue(), t);
        }

        auto em = new EnumMember(loc, ident, value, nullptr);
        e->members->push(em);
    }

    return e;
}

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
                                                   Identifier *id)
{
    auto& AST = calypso.pch.AST;
    auto& Table = AST->getPreprocessor().getIdentifierTable();

    auto NS = llvm::dyn_cast<clang::NamespaceDecl>(DC);
    clang::NamespaceDecl::redecl_iterator I, E;

    if (NS)
    {
        I = NS->redecls_begin();
        E = NS->redecls_end();
    }

    do
    {
        auto& II = Table.get(id->string);
        auto R = DC->lookup(clang::DeclarationName(&II));

        if (!R.empty())
            return R;

        if (NS && I != E)
        {
            DC = *I;
            I++;
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

Module *Module::load(Loc loc, Identifiers *packages, Identifier *ident)
{
    auto& AST = calypso.pch.AST;

    const clang::DeclContext *DC = AST->getASTContext().getTranslationUnitDecl();
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

            auto NSN = llvm::dyn_cast<clang::NamespaceDecl>(R[0]);
            if (!NSN)
            {
                ::error(loc, "only namespaces can be C++ packages");
                fatal();
            }

            DC = NSN;

            if (auto NDC = llvm::dyn_cast<clang::NamedDecl>(DC))
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

    Mapper mapper(m);

    // HACK « hardcoded modules »
    if (strcmp(ident->string, "_") == 0)
    {
        // All non-tag declarations inside the namespace go in _ (this is horrible for C functions of course, this will be fixed by the switch to Clang module system)
        auto NS = llvm::dyn_cast<clang::NamespaceDecl>(DC);

        if (!NS)
        {
            warning(Loc(), "package isn't a C++ namespace yet module is '_', ignoring this module for now (testing, will add TU later)");
            warning(Loc(), "_ identifiers will lose their special meaning after the switch to Clang's module system");
        }
        else
        {
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
                        m->members->push(mapper.VisitDecl(*D));
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

        // Module must be a record or enum
        auto TD = llvm::dyn_cast<clang::TagDecl>(R[0]);

        if (!TD)
        {
            ::error(loc, "C++ modules have to be records (class/struct) (template or not) or enums");
            fatal();
        }

        m->members->push(mapper.VisitDecl(TD));
//         srcFilename = AST->getSourceManager().getFilename(TD->getLocation());
    }
    
    amodules.push_back(m);
    return m;
}

}
