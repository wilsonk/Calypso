// Contributed by Elie Morisse, same license DMD uses

#include "cpp/astunit.h"
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
#include "clang/Sema/Sema.h"

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

TypeBasic *TypeBasic::twchar_t;

TypeBasic::TypeBasic(TY ty, const clang::BuiltinType *T)
    : ::TypeBasic(ty), T(T)
{
}

void TypeBasic::toDecoBuffer(OutBuffer *buf, int flag)
{
    Type::toDecoBuffer(buf, flag);

    switch(T->getKind())
    {
        case clang::BuiltinType::WChar_S:
        case clang::BuiltinType::WChar_U: // do the same for long/tint128 too?
            buf->writeByte('#');
            break;
        default:
            break;
    }
}

unsigned short TypeBasic::sizeType()
{
    return sizeof(cpp::TypeBasic);
}

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

    {
        clang::TargetInfo::IntType wcharTy = targetInfo.getWCharType();
        bool isSigned = targetInfo.isTypeSigned(wcharTy);
        TY ty_wchar_t;

        if (targetInfo.getTypeWidth(wcharTy) == 16)
            ty_wchar_t = isSigned ? Tint16 : Twchar;
        else
            ty_wchar_t = isSigned ? Tint32 : Tdchar;

        auto BT = cast<clang::BuiltinType>(Context.WCharTy.getTypePtr());
        if (!TypeBasic::twchar_t)
            TypeBasic::twchar_t = new TypeBasic(ty_wchar_t, BT);
        else
        {
            assert(ty_wchar_t == TypeBasic::twchar_t->ty);
            TypeBasic::twchar_t->T = BT;
        }
    }
    map(Context.WCharTy, TypeBasic::twchar_t);

    map(Context.Char16Ty, Type::twchar);
    map(Context.Char32Ty, Type::tdchar);
    map(Context.UnsignedShortTy, toInt(clang::TargetInfo::UnsignedShort));
    map(Context.UnsignedIntTy, toInt(clang::TargetInfo::UnsignedInt));
    map(Context.UnsignedLongTy, toInt(clang::TargetInfo::UnsignedLong));
    map(Context.UnsignedLongLongTy, toInt(clang::TargetInfo::UnsignedLongLong));
    map(Context.UnsignedInt128Ty, Type::tuns128); // WARNING: a one-to-one correspondance would be safer for template partial specializations
            // NOTE: cent and ucent aren't supported by D and will trigger an error during semantic()

        //===- Signed Types -------------------------------------------------------===//
    map(Context.SignedCharTy, Type::tint8);
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

static bool isNonPODRecord(const clang::RecordDecl *RD)
{
    auto CRD = dyn_cast<clang::CXXRecordDecl>(RD);
    if (!CRD)
        return false;

    if (!CRD->hasDefinition()) // WARNING forward decls will always be considered POD
        return false;

    return !CRD->isPOD();
}

bool isNonPODRecord(const clang::QualType T)
{
    auto RT = T->getAs<clang::RecordType>();
    if (!RT)
        return false;

    return isNonPODRecord(RT->getDecl());
}

bool isNonSupportedType(clang::QualType T)
{
    auto& Context = calypso.pch.AST->getASTContext();

    // non-POD class values
    if (isNonPODRecord(T))
        return true;

    // (u)int128_t or any pointer/reference to (TODO: function types as well?)
    auto Pointee = T->getPointeeType();
    while (!Pointee.isNull())
    {
        T = Pointee;
        Pointee = T->getPointeeType();
    }

    if (auto BT = T->getAs<clang::BuiltinType>())
    {
        clang::QualType Builtin(BT, 0);
        if (Builtin == Context.Int128Ty || Builtin == Context.UnsignedInt128Ty)
            return true;
    }

    return false;
}

// As soon as the type is or might be a non-POD record, wrap it in TypeValueof
inline static Type *adjustAggregateType(Type *t, const clang::RecordDecl *RD = nullptr)
{
    if (!RD || RD->isDependentType() || isNonPODRecord(RD))
        return new TypeValueof(t);
    else
        return t;
}

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
    TYPEMAP(TypeOfExpr)
    TYPEMAP(PackExpansion)
#undef TYPEMAP

    // Array types
    if (auto AT = dyn_cast<clang::ArrayType>(T))
        return fromTypeArray(AT);

    // Pointer and reference types
    auto Pointer = dyn_cast<clang::PointerType>(T);
    auto Reference = dyn_cast<clang::ReferenceType>(T);

    if (Pointer || Reference)
    {
        auto pointeeT = Reference ?
                Reference->getPointeeTypeAsWritten() : Pointer->getPointeeType();
        auto pt = fromType(pointeeT);

        auto t2 = pt;
        while (t2->ty == Tvalueof)
            t2 = t2->nextOf();

        if (Pointer)
            return t2->pointerTo();
        else
            return (pt->ty != Tvalueof) ? pt->referenceTo() : t2;
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
    const clang::Decl *Scope, *Pattern;

    ScopeChecker(const clang::Decl *ScopeDecl)
        : Scope(ScopeDecl->getCanonicalDecl())
    {
        Pattern = dyn_cast<clang::ClassTemplateDecl>(ScopeDecl); // non dependent type decls might have the template as the decl context rather than the instance

        if (auto Spec = dyn_cast<clang::ClassTemplateSpecializationDecl>(ScopeDecl))
            if (!Spec->isExplicitSpecialization())
                Pattern = getTemplateSpecializedDecl(Spec);

        if (auto Temp = llvm::dyn_cast_or_null<clang::ClassTemplateDecl>(Pattern))
            Pattern = Temp->getTemplatedDecl();

        if (Pattern)
            Pattern = Pattern->getCanonicalDecl();
    }

    bool operator()(const clang::Decl *D)
    {
        if (auto Temp = dyn_cast<clang::ClassTemplateDecl>(D))
            D = Temp->getTemplatedDecl();

        auto Canon = D->getCanonicalDecl();
        if (Canon == Scope || Canon == Pattern)
            return true;

        if (auto Record = dyn_cast<clang::CXXRecordDecl>(Scope))
        {
            if (Record = Record->getDefinition())
            {
                for (auto& B: Record->bases())
                {
                    auto BRT = B.getType()->getAs<clang::RecordType>();

                    if (!BRT)
                        continue;

                    auto BRD = BRT->getDecl();
                    if (ScopeChecker(BRD)(D))
                        return true;
                }
            }
        }

        return false;
    }

    bool extended(const clang::Decl *D)
    {
        if (operator()(D))
            return true;

        if (auto ClassPattern = llvm::dyn_cast_or_null<clang::CXXRecordDecl>(Pattern))
            if (auto ClassTemplate = ClassPattern->getDescribedClassTemplate())
                if (auto MemberTemplate = ClassTemplate->getInstantiatedFromMemberTemplate())
                    return ScopeChecker(MemberTemplate)(D);

        return false;
    }
};

// Messy... and also used for expression mapping, because TypeQualified's purpose is very similar
// to expressions, could TypeQualified be merged with DotIdExp & co?
class TypeQualifiedBuilder
{
public:
    TypeMapper::FromType &from;
    TypeMapper &tm;

    ScopeChecker RootEquals;
    const clang::TemplateArgument *TopTempArgBegin,
        *TopTempArgEnd;

    void add(TypeQualified *&tqual,
                  RootObject *o);
    void addIdent(TypeQualified *&tqual,
                  Identifier *ident);
    void addInst(TypeQualified *&tqual,
                  TemplateInstance *tempinst);

    void pushInst(TypeQualified *&tqual,
                RootObject *o,
                const clang::TemplateDecl *Temp,
                const clang::TemplateArgument *ArgBegin,
                const clang::TemplateArgument *ArgEnd,
                clang::NamedDecl *Spec = nullptr);

    TypeQualifiedBuilder(TypeMapper::FromType &from, const clang::Decl* Root,
        const clang::TemplateArgument *TempArgBegin = nullptr,
        const clang::TemplateArgument *TempArgEnd = nullptr)
        : from(from), tm(from.tm), RootEquals(Root),
          TopTempArgBegin(TempArgBegin),
          TopTempArgEnd(TempArgEnd) {}

    TypeQualified *get(const clang::NamedDecl *ND);
};

void TypeQualifiedBuilder::add(TypeQualified *&tqual, RootObject *o)
{
    if (o->dyncast() == DYNCAST_IDENTIFIER)
        addIdent(tqual, static_cast<Identifier*>(o));
    else
        addInst(tqual, static_cast<TemplateInstance*>(o));
}

void TypeQualifiedBuilder::addIdent(TypeQualified *&tqual,
                                    Identifier *ident)
{
    if (!tqual)
        tqual = new TypeIdentifier(Loc(), ident);
    else
        tqual->addIdent(ident);
}

void TypeQualifiedBuilder::addInst(TypeQualified *&tqual,
                                   TemplateInstance *tempinst)
{
    if (!tqual)
        tqual = new TypeInstance(Loc(), tempinst);
    else
        tqual->addInst(tempinst);
}

void TypeQualifiedBuilder::pushInst(TypeQualified *&tqual,
                RootObject *o,
                const clang::TemplateDecl *Temp,
                const clang::TemplateArgument *ArgBegin,
                const clang::TemplateArgument *ArgEnd,
                clang::NamedDecl *Spec)
{
    auto loc = fromLoc(Spec ? Spec->getLocation() : Temp->getLocation());
    auto tiargs = from.fromTemplateArguments(ArgBegin, ArgEnd,
            Temp->getTemplateParameters());

    cpp::TemplateInstance *tempinst;
    if (o->dyncast() == DYNCAST_IDENTIFIER)
    {
        auto ident = static_cast<Identifier*>(o);
        tempinst = new cpp::TemplateInstance(loc, ident);
        tempinst->tiargs = tiargs;
    }
    else // templated overloaded operator
    {
        tempinst = static_cast<cpp::TemplateInstance*>(o);
        tempinst->tiargs->append(tiargs);
    }

    // NOTE: To reduce DMD -> Clang translations to a minimum we don't instantiate ourselves whenever possible,
    // i.e when the template instance is already declared or defined in the PCH. If it's only declared, we tell Sema to
    // complete its instantiation.
    if (Spec)
    {
        tempinst->Inst = Spec;
        tempinst->completeInst();
    }

    addInst(tqual, tempinst);
}

RootObject *getIdentOrTempinst(const clang::NamedDecl *D)
{
    const char *op = nullptr; // overloaded operator
    auto ident = getIdentifierOrNull(D, &op);
    if (!ident)
        return nullptr;

    if (op)
    {
        auto loc = fromLoc(D->getLocation());
        auto tempinst = new cpp::TemplateInstance(loc, ident);
        tempinst->tiargs = new Objects;
        tempinst->tiargs->push(new StringExp(loc, const_cast<char*>(op)));
        return tempinst;
    }
    else
        return ident;
}

TypeQualified *TypeQualifiedBuilder::get(const clang::NamedDecl *ND)
{
    TypeQualified *tqual;

    if (from.prefix)
        tqual = from.prefix; // special case where the prefix has already been determined from a NNS
    else if (RootEquals(ND))
        tqual = nullptr;
    else
    {
        auto Key = tm.GetImplicitImportKeyForDecl(ND);
        ScopeChecker KeyEquals(Key);

        if (KeyEquals(ND))  // we'll need a fully qualified type
        {
            // build a fake import
            auto im = tm.BuildImplicitImport(Key);

            tqual = nullptr;
            for (size_t i = 1; i < im->packages->dim; i++)
                addIdent(tqual, (*im->packages)[i]);
            addIdent(tqual, im->id);

            if (isa<clang::NamespaceDecl>(Key))
                return tqual;
        }
        else
            tqual = get(cast<clang::NamedDecl>(
                    getDeclContextNamedOrTU(ND)));
    }

    auto o = getIdentOrTempinst(ND);
    if (!o)
        return tqual;

    clang::NamedDecl *Spec = nullptr;
    const clang::TemplateDecl *SpecTemp = nullptr;
    llvm::ArrayRef<clang::TemplateArgument> TempArgs;

    llvm::SmallVector<clang::TemplateArgument, 4> ArgsArray; // temporary array used for function specs only

    if (auto ClassSpec = dyn_cast<clang::ClassTemplateSpecializationDecl>(ND))
    {
        assert(!isa<clang::ClassTemplatePartialSpecializationDecl>(ND)); // should never happen?

        if (!tm.isInjectedClassName(ClassSpec))
        {
            Spec = const_cast<clang::ClassTemplateSpecializationDecl*>(ClassSpec);
            SpecTemp = ClassSpec->getSpecializedTemplate();
            TempArgs = ClassSpec->getTemplateArgs().asArray();
        }
    }
    else if (auto Func = dyn_cast<clang::FunctionDecl>(ND)) // functions will always be at the top
    {
        auto ExplicitArgs = Func->getTemplateSpecializationArgsAsWritten();

        if (ExplicitArgs)
        {
            Spec = const_cast<clang::FunctionDecl*>(Func);
            SpecTemp = Func->getPrimaryTemplate();

            for (unsigned i = 0; i < ExplicitArgs->NumTemplateArgs; i++)
                ArgsArray.push_back((*ExplicitArgs)[i].getArgument());
            TempArgs = ArgsArray;
        }
        // NOTE: Function specs without explicit arguments will be mapped to Identifier
        // and that's okay (+ avoids argument deduction).
    }

    if (auto Temp = dyn_cast<clang::TemplateDecl>(ND))
    {
        pushInst(tqual, o, Temp, TopTempArgBegin, TopTempArgEnd);
        TopTempArgBegin = TopTempArgEnd = nullptr;  // e.g there could be multiple TypeAliasTemplateDecl in the same qualified type
    }
    else if (Spec)
    {
        pushInst(tqual, o, SpecTemp, TempArgs.begin(), TempArgs.end(), Spec);
    }
    else
        add(tqual, o);

    return tqual;
}

Type *TypeMapper::FromType::typeQualifiedFor(clang::NamedDecl* ND,
    const clang::TemplateArgument *TempArgBegin,
    const clang::TemplateArgument *TempArgEnd)
{
    if (!TempArgBegin)
        if (auto subst = tm.trySubstitute(ND)) // HACK for correctTiargs
            return subst;

    const clang::Decl *Root;
    decltype(CXXScope) ScopeStack(tm.CXXScope);

    clang::DeclarationName Name;
    if (ND->getIdentifier() ||
            ND->getDeclName().getNameKind() == clang::DeclarationName::CXXOperatorName)
        Name =  ND->getDeclName();
    else if (auto Tag = llvm::dyn_cast<clang::TagDecl>(ND))
        if (auto Typedef = Tag->getTypedefNameForAnonDecl())
            Name = Typedef->getDeclName();

    if (Name.isEmpty()) // might be an anonymous enum decl for fromExpressionDeclRef
            // TODO: check that this doesn't happen when called from TypeMapper would be more solid
        return nullptr;

    // This is currently the only place where a "C++ scope" is used, this is
    // especially needed for identifier lookups during template instantiations
    while (!ScopeStack.empty())
    {
        auto ScopeDecl = ScopeStack.top();
        ScopeStack.pop();
        ScopeChecker ScopeDeclEquals(ScopeDecl);

        const clang::Decl *DCDecl = ND,
                            *Previous = ND;
        while(!isa<clang::TranslationUnitDecl>(DCDecl))
        {
            if (ScopeDeclEquals(DCDecl))
            {
                Root = Previous;
                goto LrootDone;
            }

            Previous = DCDecl;
            auto DC = DCDecl->getDeclContext();
            while (!isa<clang::Decl>(DC))
                DC = DC->getParent();
            DCDecl = cast<clang::Decl>(DC);
        }

        bool fullyQualify = false;

        auto ScopeDC = cast<clang::DeclContext>(ScopeDecl);
        auto LookupResult = ScopeDC->lookup(Name);
        for (auto Decl: LookupResult)
        {
            if (Decl->isImplicit())
                continue;

            if (ND->getCanonicalDecl() != Decl->getCanonicalDecl())
            {
                fullyQualify = true;
                break;
            }
        }

        if (auto Named = dyn_cast<clang::NamedDecl>(ScopeDecl))
            if (clang::DeclarationName::compare(Named->getDeclName(), Name) == 0)
                fullyQualify = true;

        if (fullyQualify)
        {
            Root = ND->getTranslationUnitDecl(); // to avoid name collisions, we fully qualify the type
            goto LrootDone;
        }
    }

    {
        auto NonNestedDC = tm.GetNonNestedContext(ND);
        Root = !isa<clang::TagDecl>(NonNestedDC) ?
                ND->getTranslationUnitDecl() : NonNestedDC;
    }

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
    auto Typedef = T->getDecl();
    // Temporary HACK to avoid importing "_" just because of typedefs (eg size_t)
    // which doesn't even work atm
    if (getDeclContextNamedOrTU(Typedef)->isTranslationUnit())
        return fromType(T->desugar());

    return typeQualifiedFor(Typedef);
}

Type* TypeMapper::FromType::fromTypeEnum(const clang::EnumType* T)
{
    return typeQualifiedFor(T->getDecl());
}

Type *TypeMapper::FromType::fromTypeRecord(const clang::RecordType *T)
{
    return adjustAggregateType(typeQualifiedFor(T->getDecl()),
                    T->getDecl());
}

Type *TypeMapper::FromType::fromTypeElaborated(const clang::ElaboratedType *T)
{
    // NOTE: Why must we sometimes respect NestedNameSpecifiers? Because of this situation:
    //     template<typename _Iterator>
    //       class reverse_iterator
    //       : public iterator<typename iterator_traits<_Iterator>::iterator_category>
    //
    // When mapping the template DMD will add an import to iterator_traits, but when
    // the instance of iterator will be mapped, iterator_category will have the *base* class
    // of the specialization of iterator_traits, __iterator_traits as its parent decl context,
    // I.e __iterator_traits::iterator_category
    // But iterator isn't aware of __iterator_traits so lookup error.
    // The NNS will always be known, so use it.

    TypeQualified *tqual = nullptr;
    if (auto NNS = T->getQualifier())
    {
        if (NNS->getKind() == clang::NestedNameSpecifier::TypeSpec ||
                NNS->getKind() == clang::NestedNameSpecifier::TypeSpecWithTemplate)
            tqual = fromNestedNameSpecifier(NNS);
    }

    return FromType(tm, tqual)(T->getNamedType());
}

TypeQualified *TypeMapper::FromType::fromTemplateName(const clang::TemplateName Name,
                const clang::TemplateArgument *ArgBegin,
                const clang::TemplateArgument *ArgEnd)
{
    Identifier *tempIdent;

    switch (Name.getKind())
    {
        case clang::TemplateName::Template:
            return (TypeQualified *) typeQualifiedFor(Name.getAsTemplateDecl(),
                ArgBegin, ArgEnd);

        case clang::TemplateName::QualifiedTemplate:
        {
            TypeQualified *tqual = nullptr;
            auto NNS = Name.getAsQualifiedTemplateName()->getQualifier();

            if (NNS->getKind() == clang::NestedNameSpecifier::TypeSpec ||
                    NNS->getKind() == clang::NestedNameSpecifier::TypeSpecWithTemplate)
                tqual = fromNestedNameSpecifier(NNS);

            return static_cast<TypeQualified *>(FromType(tm, tqual).typeQualifiedFor(Name.getAsTemplateDecl(),
                ArgBegin, ArgEnd)); // FIXME the cast is temporary, typeQualifiedFor should return TypeQualified
        }

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
        // NOTE: To reduce DMD -> Clang translations to a minimum we don't instantiate ourselves whenever possible, i.e when
        // the template instance is already declared or defined in the PCH. If it's only declared, there's a chance the specialization
        // wasn't emitted in the C++ libraries, so we tell Sema to complete its instantiation.

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

            ti->Inst = RT->getDecl();
            ti->completeInst();
        }

        if (!T->isTypeAlias())
        {
            if (!RT)
            {
                auto ICNT = T->castAs<clang::InjectedClassNameType>();
                return adjustAggregateType(tqual, ICNT->getDecl());
            }

            return adjustAggregateType(tqual, RT->getDecl());
        }
    }

    return adjustAggregateType(tqual);
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
    // NOTE: it's necessary to "revert" resolved symbol names of C++ template instantiations by Sema to the parameter name because D severes the link between the template instance scope and its members, and the only links that remain are the AliasDeclarations created by TemplateInstance::declareParameters

    // One exception is when the type managed to escape the template declaration, e.g with decltype(). In this fairly rare case T has to be desugared.
    bool isEscaped = true;

    auto ParmDecl = T->getReplacedParameter()->getDecl();
    auto Temp = cast<clang::Decl>(ParmDecl->getDeclContext());

    decltype(CXXScope) ScopeStack(tm.CXXScope);
    while (!ScopeStack.empty())
    {
        auto ScopeDecl = ScopeStack.top();
        ScopeStack.pop();
        ScopeChecker ScopeDeclEquals(ScopeDecl);

        if (ScopeDeclEquals.extended(Temp))
        {
            isEscaped = false;
            break;
        }
    }

    if (isEscaped)
    {
        // If the substitued argument comes from decltype(some function template call), then the fragile link that makes perfect C++ template mapping possible (type sugar) is broken.
        // Clang has lost the template instance at this point, so first we get it back from the decltype expr.
        if (auto CE = llvm::dyn_cast_or_null<clang::CallExpr>(TypeOfExpr))
            if (auto Callee = CE->getDirectCallee())
                if (Callee->isTemplateInstantiation())
                {
                    // Secondly the substitued template argument is the one of the function template arg, but if the argument was deduced from the call args then type sugar is lost forever (either typedef, subst template arg, and maybe other kinds of sugar), this is where it gets complicated.
                    // The laziest and "should work in most cases" solution is to use DMD's own overloading and template argument deduction from the original decltype expression.

                    ExprMapper em(tm);

                    auto e = em.fromExpression(TypeOfExpr);
                    auto loc = fromLoc(TypeOfExpr->getExprLoc());

                    return new TypeTypeof(loc, e);

                    // NOTE: Sugar can't be preserved because Clang could have call arg with typedef types where the typedef decl isn't usable to get back the template arg sugar, e.g template<T> void Func(T *a); decltype(Func(someTypedef));
                    // Another possible solution would be to make a deduction listener that records the deduction actions to apply them on the call arg types, but it's much more complex.
                }

        return fromType(T->getReplacementType());
    }

    return fromTypeTemplateTypeParm(T->getReplacedParameter());
}

Type* TypeMapper::FromType::fromTypeInjectedClassName(const clang::InjectedClassNameType* T) // e.g in template <...> class A { A &next; } next has an injected class name type
{
    auto className = fromIdentifier(T->getDecl()->getIdentifier());
    return adjustAggregateType(new TypeIdentifier(Loc(), className), T->getDecl());
}

TypeQualified *TypeMapper::FromType::fromNestedNameSpecifierImpl(const clang::NestedNameSpecifier *NNS)
{
    TypeQualified *result = nullptr;

    switch (NNS->getKind())
    {
        case clang::NestedNameSpecifier::Identifier:
        {
            auto ident = fromIdentifier(NNS->getAsIdentifier());
            result = new TypeIdentifier(Loc(), ident);
            break;
        }

        case clang::NestedNameSpecifier::TypeSpec:
        case clang::NestedNameSpecifier::TypeSpecWithTemplate:
        {
            auto t = fromTypeUnqual(NNS->getAsType());
            if (t->ty == Tvalueof)
                t = t->nextOf();
            assert(t->ty == Tinstance || t->ty == Tident || t->ty == Ttypeof);
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

TypeQualified* TypeMapper::FromType::fromNestedNameSpecifier(const clang::NestedNameSpecifier* NNS)
{
    if (auto Prefix = NNS->getPrefix())
        if (auto tqual = fromNestedNameSpecifier(Prefix))
            return FromType(tm, tqual).fromNestedNameSpecifierImpl(NNS);

    return fromNestedNameSpecifierImpl(NNS);
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

    return adjustAggregateType(tqual);
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

    return adjustAggregateType(tqual);
}

template <typename _Type>
Type *TypeMapper::FromType::fromTypeOfExpr(const _Type *T)
{
    if (T->isSugared())  // TODO: remove this for reflection?
    {
        FromType underlying(tm);
        underlying.TypeOfExpr = T->getUnderlyingExpr(); // needed for SubstTemplateTypeParm

        return underlying(T->desugar());
    }

    auto exp = ExprMapper(tm).fromExpression(T->getUnderlyingExpr());
    assert(exp);
    return new TypeTypeof(Loc(), exp);
}

Type* TypeMapper::FromType::fromTypeTypeOfExpr(const clang::TypeOfExprType* T)
{
    return fromTypeOfExpr(T);
}

Type* TypeMapper::FromType::fromTypeDecltype(const clang::DecltypeType* T)
{
    return fromTypeOfExpr(T);
}

Type* TypeMapper::FromType::fromTypePackExpansion(const clang::PackExpansionType* T)
{
    return fromType(T->getPattern());
}

// This is to check whether template arguments have to be omitted
// There may be a more elegant way but for now that'll do
bool TypeMapper::isInjectedClassName(const clang::Decl *D)
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

TypeFunction *TypeMapper::FromType::fromTypeFunction(const clang::FunctionProtoType* T,
        const clang::FunctionDecl *FD)
{
    auto& S = calypso.pch.AST->getSema();

    auto params = new Parameters;
    params->reserve(T->getNumParams());

    decltype(FD->param_begin()) PI;
    if (FD)
        PI = FD->param_begin();

    // FIXME we're ignoring functions with unhandled types i.e class values
    if (isNonSupportedType(T->getReturnType()))
        return nullptr;

    for (auto I = T->param_type_begin(), E = T->param_type_end();
                I != E; I++)
    {
        if (isNonSupportedType(*I))
            return nullptr;

        StorageClass stc = STCundefined;
        auto at = tm.fromType(*I);
        Identifier *ident = nullptr;
        Expression *defaultArg = nullptr;

        if (FD)
        {
            ident = getIdentifierOrNull(*PI);

            if ((*PI)->hasDefaultArg())
            {
                clang::Expr *DefaultArgExpr;
                if ((*PI)->hasUninstantiatedDefaultArg() &&
                        (FD->getInstantiatedFromMemberFunction() || FD->isTemplateInstantiation()))
                    DefaultArgExpr = S.BuildCXXDefaultArgExpr(FD->getPointOfInstantiation(),
                                                              const_cast<clang::FunctionDecl*>(FD), *PI).get();
                else
                    DefaultArgExpr = (*PI)->hasUninstantiatedDefaultArg() ?
                                (*PI)->getUninstantiatedDefaultArg() : (*PI)->getDefaultArg();

                defaultArg = ExprMapper(tm).fromExpression(DefaultArgExpr, at);
            }

            PI++;
        }

        params->push(new Parameter(stc, at, ident, defaultArg));
    }

    StorageClass stc = STCundefined;
    if (T->isConst())
        stc |= STCconst;

    auto tf = new TypeFunction(params, FromType(tm)(T->getReturnType()),
                               0, LINKd, stc);
    tf = static_cast<TypeFunction*>(tf->addSTC(stc));
    return tf;
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

    auto im = BuildImplicitImport(D);
    implicitImports[D] = im;
    mod->members->shift(im);
}

const clang::Decl* TypeMapper::GetImplicitImportKeyForDecl(const clang::NamedDecl* D)
{
    auto TopMost = GetNonNestedContext(D);

    if (auto Spec = dyn_cast<clang::ClassTemplateSpecializationDecl>(TopMost))
        return GetImplicitImportKeyForDecl(Spec->getSpecializedTemplate());

    return TopMost->getCanonicalDecl();
}

// typedef class/struct/enum { ...anon record... } SymbolName
// are special cases, they're mapped to D aggregates instead of aliases
const clang::TagDecl *isAnonTagTypedef(const clang::TypedefNameDecl* D)
{
    if (auto TagTy = dyn_cast<clang::TagType>
            (D->getUnderlyingType()))
    {
        auto Tag = TagTy->getDecl();

        if (Tag->getTypedefNameForAnonDecl())
            return Tag;
    }

    return nullptr;
}

// Returns the topmost parent tagdecl, or the bottom-most namespace or the TU
const clang::Decl *TypeMapper::GetNonNestedContext(const clang::Decl *D)
{
    if (isa<clang::TranslationUnitDecl>(D) ||
                isa<clang::NamespaceDecl>(D))
        return D;

    if (auto ClassTemp = dyn_cast<clang::ClassTemplateDecl>(D))
        return GetNonNestedContext(ClassTemp->getTemplatedDecl());

    if (auto Typedef = dyn_cast<clang::TypedefNameDecl>(D))
        if (auto AnonTag = isAnonTagTypedef(Typedef))
            D = AnonTag;

    auto ParentDC = cast<clang::Decl>(
                        getDeclContextNamedOrTU(D));

    if (auto ParentTag = dyn_cast<clang::TagDecl>(ParentDC))
        return GetNonNestedContext(ParentTag);

    if (isa<clang::TagDecl>(D))
        return D;

    return GetNonNestedContext(ParentDC);
}

static Identifier *BuildImplicitImportInternal(const clang::DeclContext *DC,
                                               Loc loc, Identifiers *sPackages)
{
    if (DC->isTranslationUnit()) return nullptr;
    assert(!DC->isFunctionOrMethod() && "Building import for a decl nested inside a func?");

    if (auto sModule = BuildImplicitImportInternal(
                getDeclContextNamedOrTU(cast<clang::Decl>(DC)), loc, sPackages))
        return sModule;

    if (auto NS = dyn_cast<clang::NamespaceDecl>(DC))
    {
        if (NS->isAnonymousNamespace())
            error(loc, "Cannot import symbols from anonymous namespaces");

        if (!NS->isInline())
            sPackages->push(getIdentifier(NS));

        return nullptr;
    }
    else if (isa<clang::TagDecl>(DC))
        return getIdentifier(cast<clang::NamedDecl>(DC));

    llvm_unreachable("Unhandled case");
}

::Import *TypeMapper::BuildImplicitImport(const clang::Decl *D)
{
    auto loc = fromLoc(D->getLocation());

    auto DC = cast<clang::DeclContext>(D);

    auto sPackages = new Identifiers;
    auto sModule = BuildImplicitImportInternal(DC, loc, sPackages);

    if (!sModule)
    {
        if (isa<clang::ClassTemplateDecl>(D))
            sModule = getIdentifier(cast<clang::NamedDecl>(D));
        else
            // ND is neither a tag nor a class template, we need to import the namespace's functions and vars
            sModule = Lexer::idPool("_");
    }

    return new cpp::Import(loc, sPackages, sModule, nullptr, 0);
}

/***** DMD -> Clang types *****/

clang::QualType TypeMapper::toType(Loc loc, Type* t, Scope *sc, StorageClass stc)
{
    auto& Context = calypso.pch.AST->getASTContext();

    if (stc & STCref)
    {
        t = new TypeReference(t);
        stc &= ~STCref;
    }

    if (t->isConst() || t->isImmutable())
    {
        t = t->nullAttributes();
        t->mod &= ~(MODconst|MODimmutable);
        return toType(loc, t, sc, stc).withConst();
    }

    t = t->merge2();

    if (auto builtin = calypso.builtinTypes.toClang[t])
        return clang::QualType(builtin, 0);

    switch (t->ty)
    {
        case Tstruct:
        {
            auto sd = static_cast<TypeStruct*>(t)->sym;
            assert(isCPP(sd)); // FIXME: C++ template instanciation are supposed to happen only with types known to Clang for the time being
            auto RD = static_cast<cpp::StructDeclaration*>(sd)->RD;

            return Context.getRecordType(RD);
        }
        case Tclass:
        {
            auto cd = static_cast<TypeClass*>(t)->sym;
            assert(isCPP(cd));
            auto RD = static_cast<cpp::ClassDeclaration*>(cd)->RD;

            auto RT = Context.getRecordType(RD);
            return Context.getLValueReferenceType(RT);
        }
        case Tvalueof:
        {
            auto RefTy = cast<clang::ReferenceType>(toType(loc,
                                    static_cast<TypeValueof*>(t)->nextOf(), sc, stc));
            return RefTy->getPointeeType();
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
            return toType(loc, td->basetype, sc, stc);
        }
        case Tident:
        case Tinstance:
        {
            t = t->semantic(loc, sc);
            return toType(loc, t, sc, stc);
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

    assert(isa<clang::NamedDecl>(DC) || isa<clang::TranslationUnitDecl>(DC));
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
