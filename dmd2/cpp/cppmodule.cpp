// Contributed by Elie Morisse, same license DMD uses

#include "../aggregate.h"
#include "../declaration.h"
#include "../identifier.h"
#include "../import.h"
#include "../lexer.h"
#include "../template.h"

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
    : ::Module(NULL, ident, 0, 0)
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

struct Mapper
{
    clang::ASTUnit *ast() { return calypso.pch.AST; }

    llvm::DenseMap<const clang::NamedDecl*,
            Dsymbol*> declMap;  // fast lookup of mirror decls

    inline PROT toPROT(clang::AccessSpecifier AS);

    // Type conversions
    Type *toType(const clang::QualType T);  // main type conversion method

    Type *toTypeUnqual(const clang::Type *T);
    Type *toTypeBuiltin(const clang::BuiltinType *T);
    Type *toTypeBuiltinInt(clang::TargetInfo::IntType intTy);
    Type *toTypeComplex(const clang::ComplexType *T);
    Type *toTypeRecord(const clang::RecordType *T);
    TypeFunction *toTypeFunction(const clang::FunctionProtoType *T);

    Dsymbol * LoadDeclaration(const clang::NamedDecl *ND);
    Dsymbol * FindDeclaration(const clang::NamedDecl *ND);

    // Declarations
    Dsymbols *VisitDeclContext(const clang::DeclContext *DC);

    Dsymbol *VisitDecl(const clang::Decl *D);

    Dsymbol *VisitValueDecl(const clang::ValueDecl *D);
    Dsymbol *VisitRecordDecl(const clang::RecordDecl *D);
    Dsymbol *VisitTypedefDecl(const clang::TypedefDecl *D);
    Dsymbol *VisitFunctionDecl(const clang::FunctionDecl *D);
    Dsymbol *VisitTemplateDecl(const clang::TemplateDecl *D);

    TemplateParameter *VisitTemplateParameter(const clang::NamedDecl *D);

//     Dsymbol *VisitEnumDecl(const clang::EnumDecl *D);
//     Dsymbol *VisitRecordDecl(const clang::RecordDecl *D);
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

private:
    ScopeDsymbol *LoadDeclarationInternal(const clang::DeclContext *DC, Loc loc,
            Identifiers *sPackages, Identifier *&sModule);
    ScopeDsymbol *FindDeclarationInternal(const clang::DeclContext *DC, Loc loc);
};


/*****/

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


/***** Type mapping *****/

Type *Mapper::toType(const clang::QualType T)
{
    Type *t = toTypeUnqual(T.getTypePtr());

    if (T.isConstQualified())
        t = t->makeConst();

    if (T.isVolatileQualified())
    {
        ::warning(Loc(), "volatile qualifier found, declaration won't be exposed (fixme?)");
        return NULL;
    }

    // restrict qualifiers are inconsequential

    return t;
}

Type *Mapper::toTypeUnqual(const clang::Type *T)
{
    if (auto BT = llvm::dyn_cast<clang::BuiltinType>(T))
        return toTypeBuiltin(BT);
    else if (auto CT = T->getAsComplexIntegerType())
        return toTypeComplex(CT);

    // Aggregates
    if (auto RT = llvm::dyn_cast<clang::RecordType>(T))
        return toTypeRecord(RT);

        // NOTE: the C++ classes don't exactly map to D classes, but we can work
        // around that:
        //  - if a C++ function has an argument taking a class, the value will be dereferenced
        //  - if a variable of a class type is exposed, it will have struct-like semantics
        //  in D
        //  - if a C++ function returns an object of a class, make the GC acquire it
        //  somehow.

    // Array types
    if (const clang::ConstantArrayType *CAT = llvm::dyn_cast<clang::ConstantArrayType>(T))
    {
        Expression *dim = new IntegerExp(CAT->getSize().getLimitedValue());
        Type *t = toType(CAT->getElementType());
        return new TypeSArray(t, dim);
    }

    // Pointer and reference types
    bool isPointer = llvm::isa<clang::PointerType>(T),
            isReference = llvm::isa<clang::ReferenceType>(T);

    if (isPointer || isReference)
    {
        auto pointeeT = T->getPointeeType();
        auto pt = toType(pointeeT);

        if (isPointer)
            return pt->pointerTo();
        else
            return pt->referenceTo();
    }

    llvm::llvm_unreachable_internal("unrecognized");
}

Type *Mapper::toTypeBuiltin(const clang::BuiltinType *T)
{
    auto& targetInfo = ast()->getASTContext().getTargetInfo();

    switch(T->getKind())
    {
    //===- Void -----------------------------------------------------===//
        case clang::BuiltinType::Void:
            return Type::tvoid;
    //===- Unsigned Types -----------------------------------------------------===//
        case clang::BuiltinType::Bool:
            return Type::tbool;     // Clang assumes that bool means unsigned 8 bits
        case clang::BuiltinType::Char_U:
            return Type::tchar;
        case clang::BuiltinType::UChar:
            return Type::tuns8;    // getCharWidth() always returns 8
        case clang::BuiltinType::WChar_U:
        {
            clang::TargetInfo::IntType wcharTy = targetInfo.getWCharType();
            if (targetInfo.getTypeWidth(wcharTy) == 16)
                return Type::twchar;
            else
                return Type::tdchar;
        }
        case clang::BuiltinType::Char16:
            return toTypeBuiltinInt(targetInfo.getChar16Type());
        case clang::BuiltinType::Char32:
            return toTypeBuiltinInt(targetInfo.getChar32Type());
        case clang::BuiltinType::UShort:
            return toTypeBuiltinInt(clang::TargetInfo::UnsignedShort);
        case clang::BuiltinType::UInt:
            return toTypeBuiltinInt(clang::TargetInfo::UnsignedInt);
        case clang::BuiltinType::ULong:
            return toTypeBuiltinInt(clang::TargetInfo::UnsignedLong);
        case clang::BuiltinType::ULongLong:
            return toTypeBuiltinInt(clang::TargetInfo::UnsignedLongLong);
        case clang::BuiltinType::UInt128:
            return Type::tuns128;

    //===- Signed Types -------------------------------------------------------===//
        case clang::BuiltinType::Char_S:
        case clang::BuiltinType::SChar:
            return Type::tint8;
        case clang::BuiltinType::WChar_S:
            return toTypeBuiltinInt(targetInfo.getWIntType());
        case clang::BuiltinType::Short:
            return toTypeBuiltinInt(clang::TargetInfo::SignedShort);
        case clang::BuiltinType::Int:
            return toTypeBuiltinInt(clang::TargetInfo::SignedInt);
        case clang::BuiltinType::Long:
            return toTypeBuiltinInt(clang::TargetInfo::SignedLong);
        case clang::BuiltinType::LongLong:
            return toTypeBuiltinInt(clang::TargetInfo::SignedLongLong);
        case clang::BuiltinType::Int128:
            return Type::tint128;

    //===- Floating point types -----------------------------------------------===//
        case clang::BuiltinType::Float:
            return Type::tfloat32;
        case clang::BuiltinType::Double:
            return Type::tfloat64;
        case clang::BuiltinType::LongDouble:
            return Type::tfloat80;

    //===- Language-specific types --------------------------------------------===//
        case clang::BuiltinType::NullPtr:
            return Type::tnull;     // or is tvoidptr?

    //===-------------------------------------------------------------------------------------===//
        default:
            assert(false && "missing built-in type correspondance");
            return NULL;
    }
}

// Most reliable way to determine target-dependent int type correspondances (except for char)
Type *Mapper::toTypeBuiltinInt(clang::TargetInfo::IntType intTy)
{
    auto& targetInfo = ast()->getPreprocessor().getTargetInfo();

    auto width = targetInfo.getTypeWidth(intTy);
    if (clang::TargetInfo::isTypeSigned(intTy))
    {
        switch(width)
        {
            case 8:
                return Type::tint8;
            case 16:
                return Type::tint16;
            case 32:
                return Type::tint32;
            case 64:
                return Type::tint64;
            case 128:
                return Type::tint128;
        }
    }
    else
    {
        switch(width)
        {
            case 8:
                return Type::tuns8;
            case 16:
                return Type::tuns16;
            case 32:
                return Type::tuns32;
            case 64:
                return Type::tuns64;
            case 128:
                return Type::tuns128;
        }
    }

    assert(false && "unexpected int type size");
    return NULL;
}

Type *Mapper::toTypeComplex(const clang::ComplexType *T)
{
    auto dT = T->desugar();

    if (dT == ast()->getASTContext().FloatComplexTy)
        return Type::tcomplex32;
    else if (dT == ast()->getASTContext().DoubleComplexTy)
        return Type::tcomplex64;
    else if (dT == ast()->getASTContext().LongDoubleComplexTy)
        return Type::tcomplex80;

    assert(false && "unexpected complex number type");
    return NULL;
}

Type *Mapper::toTypeRecord(const clang::RecordType *T)
{
    const clang::RecordDecl *RD = T->getDecl();
    return declMap[RD]->getType();
}

TypeFunction *Mapper::toTypeFunction(const clang::FunctionProtoType* T)
{
    auto params = new Parameters;
    params->reserve(T->getNumParams());

    for (auto I = T->param_type_begin(), E = T->param_type_end();
                I != E; I++)
    {
        params->push(new Parameter(STCundefined, toType(*I), NULL, NULL));
    }

    return new TypeFunction(params, toType(T->getReturnType()), 0, LINKcpp);  // does LINK matter?
}

// assume that the decl was visited
// Dsymbol* Mapper::FindDeclaration(const clang::NamedDecl* ND)
// {
//     auto loc = toLoc(ND->getLocation());
//     auto sPackages = new Identifiers();
//     Identifier *sModule = nullptr;
//
//     auto sc = FindDeclarationInternal(ND->getDeclContext(), loc);
//
//
//     auto id = toIdentifier(ND->getIdentifier());
//     return sd->search(loc, id, 0);
// }
//
// ScopeDsymbol* Mapper::FindDeclarationInternal(const clang::DeclContext* DC, Loc loc)
// {
//     if (DC->isTranslationUnit()) return calypso.pch.;
// //     if (DC->isFunctionOrMethod()) return;
//     auto scope = FindDeclarationInternal(DC->getParent(), loc);
//
//      if (!scope)
//     {
//         if (auto NS = llvm::dyn_cast<clang::NamespaceDecl>(DC))
//         {
//             if (!NS->isAnonymousNamespace() && !NS->isInline())
//             {
//                 Identifier *id = toIdentifier(NS->getIdentifier());
//                 sPackages->push(id);
//             }
//             return NULL;
//         }
//     }
//
//     if (auto Tag = llvm::dyn_cast<clang::TagDecl>(DC))
//     {
//         clang::IdentifierInfo *II;
//
//         if (auto Typedef = Tag->getTypedefNameForAnonDecl())
//             II = Typedef->getIdentifier();
//         else if (auto Spec = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(DC))
//             II = Spec->getSpecializedTemplate()->getIdentifier();
//         else
//             II = Tag->getIdentifier();
//
//         Identifier *id = toIdentifier(II);
//
//         if (!scope)
//         {
//             // non-nested tag decl, which means it's in its own module named after it
//             sModule = id;
//
//             // Create a static C++ import (which are prefixed with "cpp." by default)
//             Import *im = new Import(loc, sPackages, sModule, NULL, 1);
//             // NOTE: Assume that we are in the first semantic pass and call semantic()
//             // for that new Import (which isn't added to the AST)
//             im->semantic(NULL);
//
//             scope = im->mod;
//             // now look for the top-level tag declaration the module was named after.
//         }
//
//         Dsymbol *s = scope->search(loc, id, 0); // mmm DMD not naming its flags,
//                     // maybe I should  remove all my comments for style harmonization?
//
//                     // FIXME TEMPLATES HOW?
//
//         ScopeDsymbol *sd = s->isScopeDsymbol();
//         assert(sd);
//
//         return sd;
//     }
//
//     return NULL;
// }

// Load a module for a declaration, most often for a type definition, and return
// the decl.
Dsymbol *Mapper::LoadDeclaration(const clang::NamedDecl *ND)
{
    auto sPackages = new Identifiers();
    Identifier *sModule = nullptr;

    auto loc = toLoc(ND->getLocation());

    auto sd = LoadDeclarationInternal(ND->getDeclContext(), loc,
                                               sPackages, sModule);

    if (!sd)
    {
        // It means that ND's parent context isn't a tag but either a namespace
        // or the translation unit itself.

        sModule = Lexer::idPool("_"); // FIXME: find a better module name?

        auto im = new Import(loc, sPackages, sModule, NULL, 1);
        im->semantic(NULL);

        sd = im->mod;
    }

    auto id = toIdentifier(ND->getIdentifier());
    return sd->search(loc, id, 0);
}

ScopeDsymbol *Mapper::LoadDeclarationInternal(const clang::DeclContext *DC, Loc loc,
            Identifiers *sPackages, Identifier *&sModule)
{
    if (DC->isTranslationUnit()) return NULL;
//     if (DC->isFunctionOrMethod()) return;
    auto scope = LoadDeclarationInternal(DC->getParent(), loc,
                                                  sPackages, sModule);

    if (!scope)
    {
        if (auto NS = llvm::dyn_cast<clang::NamespaceDecl>(DC))
        {
            if (!NS->isAnonymousNamespace() && !NS->isInline())
            {
                Identifier *id = toIdentifier(NS->getIdentifier());
                sPackages->push(id);
            }
            return NULL;
        }
    }

    if (auto Tag = llvm::dyn_cast<clang::TagDecl>(DC))
    {
        clang::IdentifierInfo *II;

        if (auto Typedef = Tag->getTypedefNameForAnonDecl())
            II = Typedef->getIdentifier();
        else if (auto Spec = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(DC))
            II = Spec->getSpecializedTemplate()->getIdentifier();
        else
            II = Tag->getIdentifier();

        Identifier *id = toIdentifier(II);

        if (!scope)
        {
            // non-nested tag decl, which means it's in its own module named after it
            sModule = id;

            // Create a static C++ import (which are prefixed with "cpp." by default)
            Import *im = new Import(loc, sPackages, sModule, NULL, 1);
            // NOTE: Assume that we are in the first semantic pass and call semantic()
            // for that new Import (which isn't added to the AST)
            im->semantic(NULL);

            scope = im->mod;
            // now look for the top-level tag declaration the module was named after.
        }

        Dsymbol *s = scope->search(loc, id, 0); // mmm DMD not naming its flags,
                    // maybe I should  remove all my comments for style harmonization?

                    // FIXME TEMPLATES HOW?

        ScopeDsymbol *sd = s->isScopeDsymbol();
        assert(sd);

        return sd;
    }

    return NULL;
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
{   Dsymbol *s = NULL;

    // Unfortunately a long ugly list of if (... dyn_cast...) is more solid and
    // future-proof than a pretty switch à la decl_visitor

#define DECL(BASE) \
    else if (const clang::BASE##Decl *BASE##D = \
                            llvm::dyn_cast<clang::BASE##Decl>(D)) \
        s = Visit##BASE##Decl(BASE##D);

    if (0) ;
//     DECL(Value)
//     DECL(Typedef)
    DECL(Record)
    DECL(Function)
//     DECL(Template)

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
    bool isPOD = true;
    bool isDefined = D->getDefinition() != nullptr;

    if (isDefined && !D->isCompleteDefinition())
        return nullptr;

    // If the definition isn't in the PCH we will still submit an empty aggregate decl

    auto loc = toLoc(D->getLocation());
    auto id = toIdentifier(D->getIdentifier());

    auto CRD = llvm::dyn_cast<clang::CXXRecordDecl>(D);
        // NOTE: CXXRecordDecl will disappear in a future version of Clang and only
        // RecordDecl will remain to be used for both C and C++.

    if (CRD && !CRD->isPOD())
    {
        ::warning(loc, "non-POD decls not implemented yet FIXME"); //FIXME
        isPOD = false;
    }

    if (isPOD)
    {
        a = new StructDeclaration(loc, id, D);

        if (isDefined)
        {
            // atm we're sortof mirroring parseAggregate()
            auto members = new Dsymbols;

            for (auto I = D->field_begin(), E = D->field_end(); I != E; ++I)
            {
                if (I->getCanonicalDecl() != *I)
                    continue;

                members->push_back(VisitValueDecl(*I));
            }

            a->members = members;
        }
    }
    else
    {
        auto baseclasses = new BaseClasses;

        for (auto B = CRD->bases_begin(),
                BEnd = CRD->bases_end(); B != BEnd; ++B)
        {
            auto BRT = llvm::cast<clang::RecordType>(B->getType().getTypePtr());
            auto brt = toTypeUnqual(BRT);

            baseclasses->push(new BaseClass(brt, toPROT(B->getAccessSpecifier())));
        }

        auto cd = new ClassDeclaration(loc, id, baseclasses, CRD);
        a = cd;
    }

//     a->members = VisitDeclContext(D);

    declMap[D] = a;
    return a;
}

Dsymbol *Mapper::VisitFunctionDecl(const clang::FunctionDecl *D)
{
    auto loc = toLoc(D->getLocation());
    auto id = toIdentifier(D->getIdentifier());

    auto FPT = llvm::cast<clang::FunctionProtoType>(D->getType().getTypePtr());
    return new FuncDeclaration(loc, id, D, toTypeFunction(FPT));
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
//             return NULL;
//
//         s = VisitRecordDecl(CTD->getTemplatedDecl());
//     }
//     else if (auto FTD =
//                     llvm::dyn_cast<clang::FunctionTemplateDecl>(D))
//     {
//         if (!FTD->isThisDeclarationADefinition())
//             return NULL;
//
//         s = VisitFunctionDecl(FTD->getTemplatedDecl());
//     }
//
//     Dsymbols *decldefs = new Dsymbols();
//     decldefs->push(s);
//
//     a = new TemplateDeclaration(loc, id, tpl, NULL, decldefs, 0);
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
//             Expression *tp_defaultvalue = NULL;
//
//             if (NTTPD->hasDefaultArgument())
//                 tp_defaultvalue = toExpression(NTTPD->getDefaultArgument());
//
//                 // NOTE: the default argument isn't really needed though
//
//             tp = new TemplateValueParameter(loc, tp_ident, tp_valtype,
//                                         NULL, tp_defaultvalue);
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
//             Type *tp_defaulttype = NULL;
//
//             if (TTPD->hasDefaultArgument())
//                 tp_defaulttype = toType(TTPD->getDefaultArgument());
//
//             tp = new TemplateTypeParameter(loc, tp_ident, NULL, tp_defaulttype);
//         }
//     }
//     else assert(false && "unrecognized template parameter");
//
//     return tp;
// }

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

    Mapper mapper;
    auto members = new Dsymbols();

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
                        members->push(mapper.VisitDecl(*D));
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

        members->push(mapper.VisitDecl(TD));
//         srcFilename = AST->getSourceManager().getFilename(TD->getLocation());
    }

    auto m = new Module(moduleName(packages, ident).c_str(), ident, packages);
    m->loc = loc;
    m->members = members;
    amodules.push_back(m);

    return m;
}

}
