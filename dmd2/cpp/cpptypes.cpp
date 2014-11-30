#include "cpp/calypso.h"
#include "module.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Type.h"
#include "clang/Frontend/ASTUnit.h"

namespace cpp
{

void BuiltinTypes::map(clang::CanQualType &CQT, Type* t)
{
    auto T = CQT.getTypePtr()->castAs<clang::BuiltinType>();

    toD[T] = t;
    toClang[t] = T;
}

void BuiltinTypes::build(clang::ASTContext &Context)
{
    auto& targetInfo = Context.getTargetInfo();

        //===- Void -----------------------------------------------------===//
    map(Context.VoidTy, Type::tvoid);

        //===- Unsigned Types -----------------------------------------------------===//
    map(Context.BoolTy, Type::tbool);     // Clang assumes that bool means unsigned 8 bits
    map(Context.CharTy, Type::tchar);
    map(Context.UnsignedCharTy, Type::tuns8);    // getCharWidth() always returns 8

    clang::TargetInfo::IntType wcharTy = targetInfo.getWCharType();
    if (targetInfo.getTypeWidth(wcharTy) == 16)
        map(Context.WCharTy, Type::twchar);
    else
        map(Context.WCharTy, Type::tdchar);

    map(Context.Char16Ty, toInt(targetInfo.getChar16Type()));
    map(Context.Char32Ty, toInt(targetInfo.getChar32Type()));
    map(Context.UnsignedShortTy, toInt(clang::TargetInfo::UnsignedShort));
    map(Context.UnsignedIntTy, toInt(clang::TargetInfo::UnsignedInt));
    map(Context.UnsignedLongTy, toInt(clang::TargetInfo::UnsignedLong));
    map(Context.UnsignedLongLongTy, toInt(clang::TargetInfo::UnsignedLongLong));
    map(Context.UnsignedInt128Ty, Type::tuns128);

        //===- Signed Types -------------------------------------------------------===//
//     map(Context.CharTy, Type::tint8);
    map(Context.SignedCharTy, Type::tint8);
//     map(Context.WCharTy, toInt(targetInfo.getWIntType()));
    map(Context.ShortTy, toInt(clang::TargetInfo::SignedShort));
    map(Context.IntTy, toInt(clang::TargetInfo::SignedInt));
    map(Context.LongTy, toInt(clang::TargetInfo::SignedLong));
    map(Context.LongLongTy, toInt(clang::TargetInfo::SignedLongLong));
    map(Context.Int128Ty, Type::tint128);

        //===- Floating point types -----------------------------------------------===//
    map(Context.FloatTy, Type::tfloat32);
    map(Context.DoubleTy, Type::tfloat64);
    map(Context.LongDoubleTy, Type::tfloat80);

        //===- Language-specific types --------------------------------------------===//
    map(Context.NullPtrTy, Type::tnull); // or is tvoidptr?
}

// Most reliable way to determine target-dependent int type correspondances (except for char)
Type *BuiltinTypes::toInt(clang::TargetInfo::IntType intTy)
{
    auto& targetInfo = calypso.getASTContext().getTargetInfo();

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
    return nullptr;
}

/***** Type mapping *****/

Type *TypeMapper::toType(const clang::QualType T)
{
    Type *t = toTypeUnqual(T.getTypePtr());

    if (T.isConstQualified())
        t = t->makeConst();

    if (T.isVolatileQualified())
    {
        ::warning(Loc(), "volatile qualifier found, declaration won't be exposed (fixme?)");
        return nullptr;
    }

    // restrict qualifiers are inconsequential

    return t;
}

Type *TypeMapper::toTypeUnqual(const clang::Type *T)
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


Type *TypeMapper::toTypeBuiltin(const clang::BuiltinType *T)
{
    auto t = calypso.builtinTypes.toD[T];

    assert(t && "missing built-in type correspondance");
    return t;
}

Type *TypeMapper::toTypeComplex(const clang::ComplexType *T)
{
    auto& Context = calypso.pch.AST->getASTContext();
    auto dT = T->desugar();

    if (dT == Context.FloatComplexTy)
        return Type::tcomplex32;
    else if (dT == Context.DoubleComplexTy)
        return Type::tcomplex64;
    else if (dT == Context.LongDoubleComplexTy)
        return Type::tcomplex80;

    assert(false && "unexpected complex number type");
    return nullptr;
}

Type *TypeMapper::toTypeRecord(const clang::RecordType *T)
{
    const clang::RecordDecl *RD = T->getDecl();
    AddImplicitImportForDecl(RD);
    return new TypeIdentifier(Loc(), toIdentifier(RD->getIdentifier()));

//     if (auto ad = declMap[RD])
//         return ad->getType();
//     else
//     {
//         ad = static_cast<AggregateDeclaration*>(
//                 BuildImplicitImport(RD));
//         declMap[RD] = ad;
//         return ad->getType();
//     }
}

TypeFunction *TypeMapper::toTypeFunction(const clang::FunctionProtoType* T)
{
    auto params = new Parameters;
    params->reserve(T->getNumParams());

    for (auto I = T->param_type_begin(), E = T->param_type_end();
                I != E; I++)
    {
        params->push(new Parameter(STCundefined, toType(*I), nullptr, nullptr));
    }

    return new TypeFunction(params, toType(T->getReturnType()), 0, LINKd);  // does LINK matter?
}

// In D if a class is inheriting from another module's class, then its own module has to import the base class' module.
// So we need to populate the beginning of our virtual module with imports for derived classes.
void TypeMapper::AddImplicitImportForDecl(const clang::NamedDecl* ND)
{
    auto D = GetImplicitImportKeyForDecl(ND);

    if (implicitImports[D])
        return;

    auto im = BuildImplicitImport(ND);
    implicitImports[D] = im;
    mod->members->insert(0, im);
}

// Record -> furthest parent tagdecl
// Other decl in namespace -> the canonical namespace decl
// Other decl in TU -> the TU
const clang::Decl* TypeMapper::GetImplicitImportKeyForDecl(const clang::NamedDecl* ND)
{
    if (auto Tag = llvm::dyn_cast<clang::TagDecl>(ND))
    {
        if (auto ParentTag = llvm::dyn_cast<clang::TagDecl>(ND->getDeclContext()))
            return GetImplicitImportKeyForDecl(ParentTag);
        else
            return Tag->getCanonicalDecl();
    }
    else if (auto NS = llvm::dyn_cast<clang::NamespaceDecl>(ND->getDeclContext()))
        return NS->getCanonicalDecl();
    else
        return llvm::cast<clang::TranslationUnitDecl>(ND->getDeclContext());
}

Import *TypeMapper::BuildImplicitImport(const clang::Decl *ND)
{
    auto loc = toLoc(ND->getLocation());

    auto sPackages = new Identifiers;
    Identifier *sModule = nullptr;

    auto DC = llvm::cast<clang::DeclContext>(ND);
    if (!BuildImplicitImportInternal(DC, loc, sPackages, sModule))
        // ND isn't a tag, we need to import the namespace's functions and vars
        sModule = Lexer::idPool("_");

    return new Import(loc, sPackages, sModule, nullptr, 0);
}

bool TypeMapper::BuildImplicitImportInternal(const clang::DeclContext *DC, Loc loc,
            Identifiers *sPackages, Identifier *&sModule)
{
    if (DC->isTranslationUnit()) return false;
    assert(!DC->isFunctionOrMethod() && "Building import for a decl nested inside a func?");

    if (BuildImplicitImportInternal(DC->getParent(), loc, sPackages, sModule))
        return true;

    if (auto NS = llvm::dyn_cast<clang::NamespaceDecl>(DC))
    {
        if (NS->isAnonymousNamespace())
            error(loc, "Cannot import symbols from anonymous namespaces");

        if (!NS->isInline())
            sPackages->push(toIdentifier(NS->getIdentifier()));

        return false;
    }
    else if (auto Tag = llvm::dyn_cast<clang::TagDecl>(DC))
    {
        clang::IdentifierInfo *II;

        if (auto Typedef = Tag->getTypedefNameForAnonDecl())
            II = Typedef->getIdentifier();
        else if (auto Spec = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(DC))
            II = Spec->getSpecializedTemplate()->getIdentifier();
        else
            II = Tag->getIdentifier();

        sModule = toIdentifier(II);
        return true;
    }

    assert(false && "Unhandled case");
    return false;
}

TypeMapper::TypeMapper(Module* mod)
    : mod(mod)
{
}

}
