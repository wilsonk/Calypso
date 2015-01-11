#include "cpp/calypso.h"
#include "cpp/cppaggregate.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppexpression.h"
#include "cpp/cppimport.h"
#include "cpp/cppmodule.h"
#include "cpp/cpptemplate.h"
#include "module.h"
#include "template.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Type.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/Sema/Sema.h"

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

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
    map(Context.NullPtrTy, Type::tvoidptr);
    map(Context.DependentTy, Type::tnull);  // should work?
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

/***** Clang -> DMD types *****/

Type *TypeMapper::fromType(const clang::QualType T)
{
    return FromType(*this)(T);
}

TypeMapper::FromType::FromType(TypeMapper &tm, TypeQualified *prefix)
    : tm(tm), prefix(prefix)
{
}

Type *TypeMapper::FromType::operator()(const clang::QualType T)
{
    Type *t = fromTypeUnqual(T.getTypePtr());

    if (T.isConstQualified())
        t = t->makeConst();

    if (T.isVolatileQualified())
        ::warning(Loc(), "volatile qualifier found, declaration will be exposed anyway");

    // restrict qualifiers are inconsequential

    return t;
}

Type *TypeMapper::FromType::fromType(const clang::QualType T)
{
    return (*this)(T);
}

Type *TypeMapper::FromType::fromTypeUnqual(const clang::Type *T)
{
    if (auto BT = dyn_cast<clang::BuiltinType>(T))
        return fromTypeBuiltin(BT);
    else if (auto CT = T->getAs<clang::ComplexType>())
        return fromTypeComplex(CT);

    if (auto FT = dyn_cast<clang::FunctionProtoType>(T))
        return fromTypeFunction(FT);

    // Purely cosmetic sugar types
    if (auto PT = dyn_cast<clang::ParenType>(T))
        return fromType(PT->desugar());
    else if (auto AT = dyn_cast<clang::AdjustedType>(T))
        return fromType(AT->desugar());

    if (auto MPT = dyn_cast<clang::MemberPointerType>(T)) // what is this seriously? it appears in boost/none_t.hpp and I have no idea what this is supposed to achieve, but CodeGen converts it to ptrdiff_t
        return Type::tptrdiff_t;

#define TYPEMAP(Ty) \
    if (auto Ty##T = dyn_cast<clang::Ty##Type>(T)) \
        return fromType##Ty(Ty##T);

    TYPEMAP(Typedef)
    TYPEMAP(Enum)
    TYPEMAP(Record)
    TYPEMAP(Elaborated)
    TYPEMAP(TemplateSpecialization)
    TYPEMAP(TemplateTypeParm)
    TYPEMAP(SubstTemplateTypeParm)
    TYPEMAP(InjectedClassName)
    TYPEMAP(DependentName)
    TYPEMAP(DependentTemplateSpecialization)
    TYPEMAP(Decltype)
    TYPEMAP(PackExpansion)
#undef TYPEMAP

        // NOTE: the C++ classes don't exactly map to D classes, but we can work
        // around that:
        //  - if a C++ function has an argument taking a class, the value will be dereferenced
        //  - if a variable of a class type is exposed, it's ok to use DotVarExp, but DtoLoad will be skipped.
        //  - if a C++ function returns an object of a class, make the GC acquire it somehow.

    // Array types
    if (auto AT = dyn_cast<clang::ArrayType>(T))
        return fromTypeArray(AT);

    // Pointer and reference types
    bool isPointer = isa<clang::PointerType>(T),
            isReference = isa<clang::ReferenceType>(T);

    if (isPointer || isReference)
    {
        auto pointeeT = T->getPointeeType();
        auto pt = fromType(pointeeT);

        if (isPointer)
            return pt->pointerTo();
        else
            return tm.isNonPODRecord(pointeeT) ? pt : pt->referenceTo();  // special case for classes
    }

    llvm::llvm_unreachable_internal("Unrecognized C++ type");
}


Type *TypeMapper::FromType::fromTypeBuiltin(const clang::BuiltinType *T)
{
    auto t = calypso.builtinTypes.toD[T];

    assert(t && "missing built-in type correspondance");
    return t;
}

Type *TypeMapper::FromType::fromTypeComplex(const clang::ComplexType *T)
{
    auto& Context = calypso.pch.AST->getASTContext();
    auto dT = T->desugar();

    if (dT == Context.FloatComplexTy)
        return Type::tcomplex32;
    else if (dT == Context.DoubleComplexTy)
        return Type::tcomplex64;
    else if (dT == Context.LongDoubleComplexTy)
        return Type::tcomplex80;

    assert(false && "unknown complex number type");
    return nullptr;
}

Type* TypeMapper::FromType::fromTypeArray(const clang::ArrayType* T)
{
    auto t = fromType(T->getElementType());

    if (auto CAT = dyn_cast<clang::ConstantArrayType>(T))
    {
        auto dim = new IntegerExp(CAT->getSize().getLimitedValue());
        return new TypeSArray(t, dim);
    }
    else if (auto DSAT = dyn_cast<clang::DependentSizedArrayType>(T))
    {
        auto dim = ExprMapper(tm).fromExpression(DSAT->getSizeExpr());
        return new TypeSArray(t, dim);
    }
    else if (auto IAT = dyn_cast<clang::IncompleteArrayType>(T))
    {
        return t->pointerTo();
    }

    llvm::llvm_unreachable_internal("Unrecognized C++ array type");
}

RootObject* TypeMapper::FromType::fromTemplateArgument(const clang::TemplateArgument* Arg,
                const clang::NamedDecl *Param)
{
    ExprMapper expmap(tm);

    RootObject *tiarg = nullptr;
    switch (Arg->getKind())
    {
        case clang::TemplateArgument::Expression:
            tiarg = expmap.fromExpression(Arg->getAsExpr());
            break;
        case clang::TemplateArgument::Integral:
        {
            auto e = expmap.fromAPInt(Arg->getAsIntegral());

            // In Clang AST enum values in template arguments are resolved to integer literals
            // If the parameter has an enum type, we need to revert integer literals to DeclRefs pointing to enum constants
            // or else DMD won't find the template decl since from its point of view uint != Enum
            if (auto NTTP = llvm::dyn_cast_or_null<clang::NonTypeTemplateParmDecl>(Param))
            {
                if (auto ET = dyn_cast<clang::EnumType>(NTTP->getType()))
                {
                    bool found = false;
                    for (auto ECD: ET->getDecl()->enumerators())
                    {
                        auto Val = ECD->getInitVal().getZExtValue();

                        if (Val == ((IntegerExp *)e)->getInteger())
                        {
                            found = true;
                            e = expmap.fromExpressionDeclRef(Loc(), ECD);
                        }
                    }

                    assert(found && "Couldn't find the corresponding enum constant for template argument");
                }
            }
            tiarg = e;
            break;
        }
        case clang::TemplateArgument::NullPtr:
            tiarg = new NullExp(Loc()/*, fromType(Arg->getNullPtrType())*/);
            break;
        case clang::TemplateArgument::Type:
            tiarg = FromType(tm)(Arg->getAsType());
            break;
        case clang::TemplateArgument::Template:
            tiarg = fromTemplateName(Arg->getAsTemplate());
            break;
        case clang::TemplateArgument::Pack:
            tiarg = fromTemplateArgument(Arg->pack_begin(), Param); // WARNING: this only takes the first arg of the pack
            break;
        default:
            assert(false && "Unsupported template arg kind");
    }

    assert(tiarg && "Template argument not supported");
    return tiarg;
}

Objects* TypeMapper::FromType::fromTemplateArguments(const clang::TemplateArgument *First,
                                        const clang::TemplateArgument *End,
                                        const clang::TemplateParameterList *ParamList)
{
    auto tiargs = new Objects;
    auto Param = ParamList ? ParamList->begin() : nullptr;

    const clang::TemplateArgument *Pack = nullptr;

    for (auto Arg = First; Arg != End; Arg++)
    {
        auto P = Param ? *Param : nullptr;
        tiargs->push(fromTemplateArgument(Arg, P));

        if (ParamList)
            Param++;

        if (Arg->getKind() == clang::TemplateArgument::Pack)
            Pack = Arg;
    }

    if (Pack && Pack->pack_size() > 1)
    {
        auto Arg = Pack->pack_begin();
        Arg++;

        for (; Arg != Pack->pack_end(); Arg++)
            tiargs->push(fromTemplateArgument(Arg));
    }

    return tiargs;
}

class ScopeChecker // determines if a C++ decl is "scopingly" equivalent to another's
{
public:
    const clang::Decl *ScopeDecl, *TemplateDecl;

    ScopeChecker(const clang::Decl *ScopeDecl)
        : ScopeDecl(ScopeDecl->getCanonicalDecl())
    {
        TemplateDecl = dyn_cast<clang::ClassTemplateDecl>(ScopeDecl); // non dependent type decls might have the template as the decl context rather than the instance

        if (auto Spec = dyn_cast<clang::ClassTemplateSpecializationDecl>(ScopeDecl))
            if (!Spec->isExplicitSpecialization())
                TemplateDecl = getTemplateSpecializedDecl(Spec);

        if (auto Temp = llvm::dyn_cast_or_null<clang::ClassTemplateDecl>(TemplateDecl))
            TemplateDecl = Temp->getTemplatedDecl();

        if (TemplateDecl)
            TemplateDecl = TemplateDecl->getCanonicalDecl();
    }

    bool operator()(const clang::Decl *D)
    {
        if (auto Temp = dyn_cast<clang::ClassTemplateDecl>(D))
            D = Temp->getTemplatedDecl();

        auto Canon = D->getCanonicalDecl();
        return Canon == ScopeDecl || Canon == TemplateDecl;
    }
};

class TypeQualifiedBuilder
{
public:
    TypeMapper::FromType &from;
    TypeMapper &tm;

    ScopeChecker RootEquals;
    const clang::TemplateArgument *TopTempArgBegin,
        *TopTempArgEnd;

    void addIdent(TypeQualified *&tqual,
                  Identifier *ident);
    void addInst(TypeQualified *&tqual,
                 clang::NamedDecl* D,
                 const clang::TemplateArgument *TempArgBegin,
                 const clang::TemplateArgument *TempArgEnd);

    TypeQualifiedBuilder(TypeMapper::FromType &from, const clang::Decl* Root,
        const clang::TemplateArgument *TempArgBegin = nullptr,
        const clang::TemplateArgument *TempArgEnd = nullptr)
        : from(from), tm(from.tm), RootEquals(Root),
          TopTempArgBegin(TempArgBegin),
          TopTempArgEnd(TempArgEnd) {}

    TypeQualified *get(clang::NamedDecl* ND);
};

void TypeQualifiedBuilder::addIdent(TypeQualified *&tqual,
                                    Identifier *ident)
{
    if (!tqual)
        tqual = new TypeIdentifier(Loc(), ident);
    else
        tqual->addIdent(ident);
}

void TypeQualifiedBuilder::addInst(TypeQualified *&tqual,
                clang::NamedDecl* D,
                const clang::TemplateArgument *ArgBegin,
                const clang::TemplateArgument *ArgEnd)
{
    auto ident = getIdentifier(D);
    auto loc = fromLoc(D->getLocation());
    auto Spec = dyn_cast<clang::ClassTemplateSpecializationDecl>(D);

    const clang::TemplateDecl *Temp;
    if (Spec)
        Temp = Spec->getSpecializedTemplate();
    else
        Temp = dyn_cast<clang::TemplateDecl>(D);

    auto tiargs = from.fromTemplateArguments(ArgBegin, ArgEnd,
            Temp->getTemplateParameters());

    auto tempinst = new cpp::TemplateInstance(loc, ident);
    tempinst->tiargs = tiargs;

    // NOTE: To reduce DMD -> Clang translations to a minimum we don't instantiate ourselves whenever possible, i.e when the template instance is already declared or defined in the PCH. If it's only declared, there's a chance the specialization wasn't emitted in the C++ libraries, so we tell Sema to complete its instantiation.
    if (Spec && !isa<clang::ClassTemplatePartialSpecializationDecl>(Spec))
    {
        tempinst->Instances[ident] = D;
        tempinst->completeInst(tm.getInstantiatingModule());
    }

    if (!tqual)
        tqual = new TypeInstance(Loc(), tempinst);
    else
        tqual->addInst(tempinst);
}

TypeQualified *TypeQualifiedBuilder::get(clang::NamedDecl *ND)
{
    TypeQualified *tqual;

    if (from.prefix)
    {
        tqual = from.prefix; // special case where the prefix has already been determined from a NNS
        goto LtqualDone;
    }

    if (RootEquals(ND))
        tqual = nullptr;
    else if (ScopeChecker(tm.GetTopMostDeclContext(ND))
            (ND))  // Root isn't even the implicit import key, there was a name collision => build a fully qualified type
    {
        // build a fake import
        auto im = tm.BuildImplicitImport(ND);

        tqual = nullptr;
        for (size_t i = 0; i < im->packages->dim; i++)
            addIdent(tqual, (*im->packages)[i]);
        addIdent(tqual, im->id);
    }
    else
        tqual = get(cast<clang::NamedDecl>(ND->getDeclContext()));

LtqualDone:
    auto ident = getIdentifierOrNull(ND);
    if (!ident)
        return tqual;

    auto CTSD = dyn_cast<clang::ClassTemplateSpecializationDecl>(ND);

    if (isa<clang::ClassTemplateDecl>(ND) || isa<clang::TypeAliasTemplateDecl>(ND))
    {
        addInst(tqual, ND, TopTempArgBegin, TopTempArgEnd);
        TopTempArgBegin = TopTempArgEnd = nullptr;  // just to be sure, because what happens if there are multiple TypeAliasTemplateDecl in the same qualified type?
    }
    else if (CTSD && !tm.isInjectedClassName(CTSD))
    {
        auto TempArgs = CTSD->getTemplateArgs().asArray();
        addInst(tqual, CTSD, TempArgs.begin(), TempArgs.end());
    }
    else
        addIdent(tqual, ident);

    return tqual;
}

Type *TypeMapper::FromType::typeQualifiedFor(clang::NamedDecl* ND,
    const clang::TemplateArgument *TempArgBegin,
    const clang::TemplateArgument *TempArgEnd)
{
    if (!TempArgBegin)
        if (auto subst = tm.trySubstitute(ND)) // HACK for correctTiargs
            return subst;

    if (!ND->getIdentifier())
        return new TypeNull; // FIXME anonymous record or enum

    const clang::Decl *Root;
    decltype(CXXScope) ScopeStack(tm.CXXScope);

    auto Name = ND->getDeclName();

    // This is currently the only place where a "C++ scope" is used, this is
    // especially needed for identifier lookups during template instantiations
    while (!ScopeStack.empty())
    {
        auto ScopeDecl = ScopeStack.top();
        ScopeStack.pop();
        ScopeChecker ScopeDeclEquals(ScopeDecl);

        const clang::Decl *DCDecl = ND;
        while(!isa<clang::TranslationUnitDecl>(DCDecl))
        {
            if (ScopeDeclEquals(DCDecl))
            {
                Root = ScopeDecl;
                goto LrootDone;
            }

            auto DC = DCDecl->getDeclContext();
            while (!isa<clang::Decl>(DC))
                DC = DC->getParent();
            DCDecl = cast<clang::Decl>(DC);
        }

        auto ScopeDC = cast<clang::DeclContext>(ScopeDecl);
        auto LookupResult = ScopeDC->lookup(Name);
        for (auto Decl: LookupResult)
        {
            if (Decl->isImplicit())
                continue;

            if (ND->getCanonicalDecl() != Decl->getCanonicalDecl())
            {
                Root = ND->getTranslationUnitDecl(); // to avoid name collisions, we do a static import and fully qualify the type
                goto LrootDone;
            }
        }
    }

    if (!isa<clang::TagDecl>(getDeclContextNamedOrTU(ND)))
        Root = ND;
    else
        Root = tm.GetTopMostDeclContext(ND);

LrootDone:
    tm.AddImplicitImportForDecl(ND);

    return TypeQualifiedBuilder(*this, Root, TempArgBegin, TempArgEnd).get(ND);
}

Type *TypeMapper::trySubstitute(const clang::Decl *D)
{
    if (!substsyms)
        return nullptr;

    for (auto s: *substsyms)
    {
#define SUBST(Kind, Sym) \
        else if (s->is##Kind##Declaration())  \
        { \
            auto Known = static_cast<cpp::Kind##Declaration*>(s)->Sym; \
            if (Known->getCanonicalDecl() != D->getCanonicalDecl()) \
                continue; \
            return new Type##Kind(static_cast<Kind##Declaration*>(s)); \
        }

        if (0) ;
        SUBST(Struct, RD)
        SUBST(Class, RD)
        SUBST(Enum, ED)
        else if (s->isTemplateDeclaration())
            continue;
        else
            assert(false && "Unexpected symbol kind");

#undef SUBST
    }

    return nullptr;
}

Type* TypeMapper::FromType::fromTypeTypedef(const clang::TypedefType* T)
{
    auto TD = T->getDecl();
    // Temporary HACK to avoid importing "_" just because of typedefs (eg size_t)
    // which doesn't even work atm
    if (getDeclContextNamedOrTU(TD)->isTranslationUnit())
        return fromType(T->desugar());

    return typeQualifiedFor(T->getDecl());
}

Type* TypeMapper::FromType::fromTypeEnum(const clang::EnumType* T)
{
    return typeQualifiedFor(T->getDecl());
}

Type *TypeMapper::FromType::fromTypeRecord(const clang::RecordType *T)
{
    return typeQualifiedFor(T->getDecl());
}

Type *TypeMapper::FromType::fromTypeElaborated(const clang::ElaboratedType *T)
{
    // NOTE: Why must we respect NestedNameSpecifiers? Because of this situation:
    //     template<typename _Iterator>
    //       class reverse_iterator
    //       : public iterator<typename iterator_traits<_Iterator>::iterator_category>
    //
    // When mapping the template DMD will add an import to iterator_traits, but when
    // the instance of iterator will mapped, iterator_category will have the *base* class
    // of the specialization of iterator_traits, __iterator_traits as its parent decl context,
    // I.e __iterator_traits::iterator_category
    // But iterator isn't aware of __iterator_traits so lookup error.
    // The NNS will always be known, so use it.

    TypeQualified *tqual = nullptr;
    if (auto NNS = T->getQualifier())
        tqual = fromNestedNameSpecifier(NNS);

    return FromType(tm, tqual)(T->desugar());
}

TypeQualified *TypeMapper::FromType::fromTemplateName(const clang::TemplateName Name,
                const clang::TemplateArgument *ArgBegin,
                const clang::TemplateArgument *ArgEnd)
{
    Identifier *tempIdent;

    switch (Name.getKind())
    {
        case clang::TemplateName::Template:
        case clang::TemplateName::QualifiedTemplate:
            return (TypeQualified *) typeQualifiedFor(Name.getAsTemplateDecl(),
                ArgBegin, ArgEnd);

        case clang::TemplateName::SubstTemplateTemplateParm:
            tempIdent = tm.getIdentifierForTemplateTemplateParm(
                    Name.getAsSubstTemplateTemplateParm()->getParameter());
            break;

        case clang::TemplateName::DependentTemplate:
            tempIdent = fromIdentifier(
                    Name.getAsDependentTemplateName()->getIdentifier());
            break;

        default:
            assert(false && "Unsupported template name kind");
            return nullptr;
    };

    if (ArgBegin)
    {
        auto ti = new cpp::TemplateInstance(Loc(), tempIdent);
        ti->tiargs = fromTemplateArguments(ArgBegin, ArgEnd,
                                        Name.getAsTemplateDecl()->getTemplateParameters());

        return new TypeInstance(Loc(), ti);
    }
    else
        return new TypeIdentifier(Loc(), tempIdent);
}

Type* TypeMapper::FromType::fromTypeTemplateSpecialization(const clang::TemplateSpecializationType* T)
{
    auto tqual = fromTemplateName(T->getTemplateName(),
                            T->begin(), T->end());

    if (T->isSugared())
    {
        // NOTE: To reduce DMD -> Clang translations to a minimum we don't instantiate ourselves whenever possible, i.e when the template instance is already declared or defined in the PCH. If it's only declared, there's a chance the specialization wasn't emitted in the C++ libraries, so we tell Sema to complete its instantiation.

        auto RT = T->getAs<clang::RecordType>();

        if (RT)
            if (auto subst = tm.trySubstitute(RT->getDecl())) // HACK for correctTiargs
                return subst;

        if (RT && !RT->isDependentType())
        {
            RootObject *o;
            if (tqual->idents.empty())
                o = static_cast<TypeInstance*>(tqual)->tempinst;
            else
                o = tqual->idents.back();
            auto ti = (cpp::TemplateInstance*)o;

            ti->Instances[ti->name] = RT->getDecl();
            ti->completeInst(tm.getInstantiatingModule());
        }
    }

    return tqual;
}

Identifier *TypeMapper::getIdentifierForTemplateTypeParm(const clang::TemplateTypeParmType *T)
{
    if (auto Id = T->getIdentifier())
        return fromIdentifier(Id);
    else
    {
        auto ParamList = templateParameters[T->getDepth()];

        if (T->getIndex() >= ParamList->size()) // this happens when the latter parameters are unnamed and have a default argument
            goto LgenId;

        auto Param = ParamList->getParam(T->getIndex());

        // Most of the time the identifier does exist in the TemplateTypeParmDecl
        if (auto Id = Param->getIdentifier())
            return fromIdentifier(Id);
    }

LgenId:
    // This should only ever happen in template param decl mapping
    ::warning(Loc(), "Generating identifier for anonymous C++ type template parameter");

    std::string str;
    llvm::raw_string_ostream OS(str);
    OS << "type_parameter_" << T->getDepth() << '_' << T->getIndex();

    return Lexer::idPool(OS.str().c_str());
}

Identifier *TypeMapper::getIdentifierForTemplateTemplateParm(const clang::TemplateTemplateParmDecl *D)
{
    // TODO: merge with others?

    if (auto Id = D->getIdentifier())
        return fromIdentifier(Id);

    // This should only ever happen in template param decl mapping
    ::warning(Loc(), "Generating identifier for anonymous C++ template template parameter");

    std::string str;
    llvm::raw_string_ostream OS(str);
    OS << "template_parameter_" << D->getDepth() << '_' << D->getIndex();

    return Lexer::idPool(OS.str().c_str());
}

Type* TypeMapper::FromType::fromTypeTemplateTypeParm(const clang::TemplateTypeParmType* T)
{
    auto ident = tm.getIdentifierForTemplateTypeParm(T);
    return new TypeIdentifier(Loc(), ident);
}

Type* TypeMapper::FromType::fromTypeSubstTemplateTypeParm(const clang::SubstTemplateTypeParmType* T)
{
    // NOTE: it's necessary to "revert" resolved symbol names of C++ template instantiations by Sema to the parameter name because the template instance cut access to its scope to its members, and the only links that remain are the AliasDeclarations created by TemplateInstance::declareParameters

    return fromTypeTemplateTypeParm(T->getReplacedParameter());
}

Type* TypeMapper::FromType::fromTypeInjectedClassName(const clang::InjectedClassNameType* T) // e.g in template <...> class A { A &next; } next has an injected class name type
{
    auto className = fromIdentifier(T->getDecl()->getIdentifier());
    return new TypeIdentifier(Loc(), className);
}

TypeQualified* TypeMapper::FromType::fromNestedNameSpecifier(const clang::NestedNameSpecifier* NNS)
{
    TypeQualified *result = nullptr;

    switch (NNS->getKind())
    {
        case clang::NestedNameSpecifier::Identifier:
        {
            auto ident = fromIdentifier(NNS->getAsIdentifier());
            if (auto Prefix = NNS->getPrefix())
            {
                result = fromNestedNameSpecifier(Prefix);
                result->addIdent(ident);
            }
            else
                result = new TypeIdentifier(Loc(), ident);

            break;
        }

        case clang::NestedNameSpecifier::TypeSpec:
        case clang::NestedNameSpecifier::TypeSpecWithTemplate:
        {
            auto t = fromTypeUnqual(NNS->getAsType());
            assert(t->ty == Tinstance || t->ty == Tident);
            result = (TypeQualified*) t;
            break;
        }

        case clang::NestedNameSpecifier::Namespace:
        case clang::NestedNameSpecifier::NamespaceAlias:
        case clang::NestedNameSpecifier::Global:
            return nullptr;  // not dependent, no derived <> base decl context issue so building a TypeQualified after the NNS is unnecessary

        default:
            assert(false && "Unsupported nested name specifier kind");
    }

    return result;
}

// NOTE: Dependent***Type are not mandatory to get templates working because the instantiation is done by Sema
// and then DMD simply maps the resulting class or function specialization, so we could return TypeNull and it would still work.
// Still good for reflection.
Type* TypeMapper::FromType::fromTypeDependentName(const clang::DependentNameType* T)
{
    TypeQualified *tqual = nullptr;

    if (auto NNS = T->getQualifier())
        tqual = fromNestedNameSpecifier(NNS);

    auto ident = fromIdentifier(T->getIdentifier());
    if (!tqual)
        tqual = new TypeIdentifier(Loc(), ident);
    else
        tqual->addIdent(ident);

    return tqual;
}

Type* TypeMapper::FromType::fromTypeDependentTemplateSpecialization(const clang::DependentTemplateSpecializationType* T)
{
    TypeQualified *tqual = nullptr;

    if (auto NNS = T->getQualifier())
        tqual = fromNestedNameSpecifier(NNS);

    auto ident = fromIdentifier(T->getIdentifier());
    auto tiargs = fromTemplateArguments(T->begin(), T->end());

    auto tempinst = new ::TemplateInstance(Loc(), ident);
    tempinst->tiargs = tiargs;

    if (!tqual)
        tqual = new TypeInstance(Loc(), tempinst);
    else
        tqual->addInst(tempinst);

    return tqual;
}

Type* TypeMapper::FromType::fromTypeDecltype(const clang::DecltypeType* T)
{
    if (T->isSugared())  // TODO: remove this for reflection
        return fromType(T->desugar());

    auto exp = ExprMapper(tm).fromExpression(T->getUnderlyingExpr());
    if (exp) // temporary? some decltype use CallExpr, which I feel would make getting things working much harder for little gain since templates are instantiated by Sema
        return new TypeTypeof(Loc(), exp);
    else
        return new TypeNull;
}

Type* TypeMapper::FromType::fromTypePackExpansion(const clang::PackExpansionType* T)
{
    return fromType(T->getPattern());
}

// This is to check whether template arguments have to be omitted
// There may be a more elegant way but for now that'll do
bool TypeMapper::isInjectedClassName(clang::Decl *D)
{
    D = D->getCanonicalDecl();

    decltype(TypeMapper::CXXScope) ScopeStack(CXXScope);
    while(!ScopeStack.empty())
    {
        auto ScopeDecl = ScopeStack.top();
        ScopeStack.pop();

        if (D == ScopeDecl->getCanonicalDecl())
            return true;
    }

    return false;
}

bool TypeMapper::isNonPODRecord(const clang::QualType T)
{
    auto RT = T->getAs<clang::RecordType>();
    if (!RT)
        return false;

    auto CRD = dyn_cast<clang::CXXRecordDecl>(RT->getDecl());
    if (!CRD)
        return false;

    if (isa<clang::ClassTemplateSpecializationDecl>(CRD))
        return true;

    // FIXME!!! If this is a forward decl, what do?? We can only know if an agg is POD if the definition is known :(
    if (!CRD->hasDefinition())
//     {
//         ::warning(Loc(), "FIXME: Assuming that CXXRecordDecl %s without a definition is non-POD",
//                   CRD->getQualifiedNameAsString().c_str());
//         return true;
//     }
        return false; // a reference of a reference is ok right?

    return !CRD->isPOD();
}

TypeFunction *TypeMapper::FromType::fromTypeFunction(const clang::FunctionProtoType* T)
{
    auto params = new Parameters;
    params->reserve(T->getNumParams());

    for (auto I = T->param_type_begin(), E = T->param_type_end();
                I != E; I++)
    {
        // FIXME we're ignoring functions with unhandled types i.e class values
        if (tm.isNonPODRecord(*I))
            return nullptr;

        params->push(new Parameter(STCundefined, FromType(tm)(*I), nullptr, nullptr));
    }

    return new TypeFunction(params, FromType(tm)(T->getReturnType()), 0, LINKd);  // does LINK matter?
}

// In D if a class is inheriting from another module's class, then its own module has to import the base class' module.
// So we need to populate the beginning of our virtual module with imports for derived classes.
void TypeMapper::AddImplicitImportForDecl(const clang::NamedDecl* ND)
{
    if (!addImplicitDecls)
        return;

    assert(mod);

    auto D = GetImplicitImportKeyForDecl(ND);

    if (D == mod->rootDecl)
        return; // do not import self

    if (implicitImports[D])
        return;

    auto im = BuildImplicitImport(ND);
    implicitImports[D] = im;
    mod->members->shift(im);
}

const clang::Decl* TypeMapper::GetImplicitImportKeyForDecl(const clang::NamedDecl* D)
{
    auto TopMost = GetTopMostDeclContext(D);

    if (auto Spec = dyn_cast<clang::ClassTemplateSpecializationDecl>(TopMost))
        return GetImplicitImportKeyForDecl(Spec->getSpecializedTemplate());

    return TopMost->getCanonicalDecl();
}

// Record -> furthest parent tagdecl
// Other decl in namespace -> the canonical namespace decl
// Other decl in TU -> the TU
const clang::Decl *TypeMapper::GetTopMostDeclContext(const clang::Decl *D)
{
    if (isa<clang::TranslationUnitDecl>(D))
        return D;

    auto ParentDC = cast<clang::Decl>(
                        getDeclContextNamedOrTU(D));

    if (auto ParentTag = dyn_cast<clang::TagDecl>(ParentDC))
        return GetTopMostDeclContext(ParentTag);

    if (isa<clang::ClassTemplateDecl>(D) || isa<clang::TagDecl>(D) ||
        isa<clang::NamespaceDecl>(D))
        return D;

    return GetTopMostDeclContext(ParentDC);
}

::Import *TypeMapper::BuildImplicitImport(const clang::Decl *ND)
{
    auto loc = fromLoc(ND->getLocation());

    auto sPackages = new Identifiers;
    Identifier *sModule = nullptr;

    auto DC = dyn_cast<clang::DeclContext>(ND);
    if (!DC)
        DC = ND->getDeclContext();

    if (!BuildImplicitImportInternal(DC, loc, sPackages, sModule))
    {
        if (isa<clang::ClassTemplateDecl>(ND))
            sModule = getIdentifier(cast<clang::NamedDecl>(ND));
        else
            // ND is neither a tag nor a class template, we need to import the namespace's functions and vars
            sModule = Lexer::idPool("_");
    }

    return new cpp::Import(loc, sPackages, sModule, nullptr, 0);
}

bool TypeMapper::BuildImplicitImportInternal(const clang::DeclContext *DC, Loc loc,
            Identifiers *sPackages, Identifier *&sModule)
{
    if (DC->isTranslationUnit()) return false;
    assert(!DC->isFunctionOrMethod() && "Building import for a decl nested inside a func?");

    if (BuildImplicitImportInternal(DC->getParent(), loc, sPackages, sModule))
        return true;

    if (auto NS = dyn_cast<clang::NamespaceDecl>(DC))
    {
        if (NS->isAnonymousNamespace())
            error(loc, "Cannot import symbols from anonymous namespaces");

        if (!NS->isInline())
            sPackages->push(fromIdentifier(NS->getIdentifier()));

        return false;
    }
    else if (isa<clang::TagDecl>(DC))
    {
        sModule = getIdentifier(cast<clang::NamedDecl>(DC));
        return true;
    }
    else if (isa<clang::LinkageSpecDecl>(DC))
        return false;

    assert(false && "Unhandled case");
    return false;
}

/***** DMD -> Clang types *****/

clang::QualType TypeMapper::toType(Loc loc, Type* t, Scope *sc)
{
    auto& Context = calypso.pch.AST->getASTContext();

    if (auto builtin = calypso.builtinTypes.toClang[t])
        return clang::QualType(builtin, 0);

    switch (t->ty)
    {
        case Tstruct:
        case Tclass:
        {
            const clang::RecordDecl *RD;

            if (t->ty == Tstruct)
            {
                auto sd = static_cast<TypeStruct*>(t)->sym;
                assert(isCPP(sd)); // FIXME: C++ template instanciation are supposed to happen only with types known to Clang for the time being
                RD = static_cast<cpp::StructDeclaration*>(sd)->RD;
            }
            else
            {
                auto cd = static_cast<TypeClass*>(t)->sym;
                assert(isCPP(cd));
                RD = static_cast<cpp::ClassDeclaration*>(cd)->RD;
            }

            return Context.getRecordType(RD);
        }
        case Tenum:
        {
            auto ed = static_cast<TypeEnum*>(t)->sym;
            assert(isCPP(ed));
            auto ED = static_cast<cpp::EnumDeclaration*>(ed)->ED;

            return Context.getEnumType(ED);
        }
        case Ttypedef:  // NOTE: these aren't the AliasDecl created by DeclMapper
        {
            auto td = static_cast<TypeTypedef*>(t)->sym;
            return toType(loc, td->basetype, sc);
        }
        case Tident:
        case Tinstance:
        {
            t = t->semantic(loc, sc);
            return toType(loc, t, sc);
        }
        case Tpointer:
        case Treference:
        {
            auto Pointee = toType(loc, t->nextOf(), sc);

            if (t->ty == Tpointer)
                return Context.getPointerType(Pointee);
            else
                return Context.getLValueReferenceType(Pointee);
        }
        case Tfunction:
        {
            auto tf = static_cast<TypeFunction*>(t);

            auto ResultTy = toType(loc, tf->next, sc);

            llvm::SmallVector<clang::QualType, 4> Args;
            for (auto p: *tf->parameters)
                Args.push_back(toType(loc, p->type, sc));

            clang::FunctionProtoType::ExtProtoInfo EPI;
            return Context.getFunctionType(ResultTy, Args, EPI);
        }
        // TODO arrays
    }

    llvm::llvm_unreachable_internal("Unhandled D -> Clang type conversion");
}

/***** *****/

TypeMapper::TypeMapper(cpp::Module* mod)
    : mod(mod)
{
}

const clang::DeclContext *getDeclContextNamedOrTU(const clang::Decl *D)
{
    auto DC = D->getDeclContext();

    while (isa<clang::LinkageSpecDecl>(DC))
        DC = DC->getParent();

    return DC;
}

const clang::NamedDecl *getTemplateSpecializedDecl(const clang::ClassTemplateSpecializationDecl *Spec)
{
    if (Spec->isExplicitSpecialization())
        return Spec;

    auto U = Spec->getSpecializedTemplateOrPartial();

    if (U.is<clang::ClassTemplateDecl*>())
        return U.get<clang::ClassTemplateDecl*>();
    else
        return U.get<clang::ClassTemplatePartialSpecializationDecl*>();
}

}
