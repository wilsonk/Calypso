// Contributed by Elie Morisse, same license DMD uses

#include "cpp/astunit.h"
#include "aggregate.h"
#include "attrib.h"
#include "declaration.h"
#include "enum.h"
#include "identifier.h"
#include "import.h"
#include "init.h"
#include "lexer.h"
#include "template.h"
#include "scope.h"
#include "statement.h"
#include "id.h"

#include "cpp/calypso.h"
#include "cpp/cppmodule.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppimport.h"
#include "cpp/cppaggregate.h"
#include "cpp/cppexpression.h"
#include "cpp/cpptemplate.h"

#include <stdlib.h>
#include <string>

#include "llvm/ADT/DenseMap.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/Lookup.h"

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

namespace
{

inline Dsymbols *oneSymbol(Dsymbol *s)
{
    auto decldefs = new Dsymbols;
    decldefs->push(s);
    return decldefs;
}

}

Dsymbols *DeclMapper::VisitDeclContext(const clang::DeclContext *DC)
{
    auto decldefs = new Dsymbols;

    for (auto D = DC->decls_begin(), DEnd = DC->decls_end();
        D != DEnd; ++D)
    {
        if (auto d = VisitDecl(*D))
            decldefs->append(d);
    }

    return decldefs;
}

Dsymbols *DeclMapper::VisitDecl(const clang::Decl *D, unsigned flags)
{
    if (!D->isCanonicalDecl())
        return nullptr;

    Dsymbols *s = nullptr;

#define DECL(BASE) \
    else if (const clang::BASE##Decl *BASE##D = \
                            dyn_cast<clang::BASE##Decl>(D)) \
        s = Visit##BASE##Decl(BASE##D);
#define DECLWF(BASE) \
    else if (const clang::BASE##Decl *BASE##D = \
                            dyn_cast<clang::BASE##Decl>(D)) \
        s = Visit##BASE##Decl(BASE##D, flags);

    if (0) ;
    DECL(TypedefName)
    DECL(ClassTemplateSpecialization)
    DECLWF(Record)
    DECL(Function)
    DECL(RedeclarableTemplate)
    DECL(Enum)
    DECL(Value)

#undef DECL
#undef DECLWF

    return s;
}

Dsymbols *DeclMapper::VisitValueDecl(const clang::ValueDecl *D)
{
    auto& Context = calypso.getASTContext();
    ExprMapper expmap(*this);

    if (isNonSupportedType(D->getType()))
        return nullptr;

    if (isa<clang::IndirectFieldDecl>(D)) // implicit fields injected from anon unions/structs, which are already mapped
        return nullptr;

    auto loc = fromLoc(D->getLocation());
    auto Ty = D->getType().getDesugaredType(Context);

    if (Ty->isUnionType() || Ty->isStructureType())
    {
        if (auto RT = Ty->getAs<clang::RecordType>()) // dyn_cast because it can also be a typedeftype, which always have names
        {
            auto RD = RT->getDecl();

            // NOTE: in union {...} myUnion isAnonymousStructOrUnion() will be false, see Decl.h
            if (RD->isAnonymousStructOrUnion() || !RD->getIdentifier())
                return nullptr;

            // TODO how to handle union {...} myUnion?
            // We could try adding the aggregate without an id, and assign a TypeClass/TypeStruct straightaway.
            // Ignore them for now.
        }
    }

    auto id = fromIdentifier(D->getIdentifier());
    auto t = fromType(D->getType());

    if (!t)
        return nullptr;

    if (t->isConst())
        t = t->immutableOf();

    auto a = new VarDeclaration(loc, id, D, t);

    if (auto Var = dyn_cast<clang::VarDecl>(D))
    {
        if (Var->hasExternalStorage())
            a->storage_class |= STCextern;

        if (Var->getTLSKind() == clang::VarDecl::TLS_Dynamic)
            a->storage_class |= STCtls;

        if (Var->isStaticDataMember())
            a->storage_class |= STCstatic;

        if ((Var->isConstexpr() || t->isImmutable()) &&
                Var->getAnyInitializer())
        {
            // we avoid initializer expressions except for const/constexpr variables
            auto e = expmap.fromExpression(Var->getAnyInitializer(),
                                           nullptr, true);
            if (e && e->op != TOKnull)
                a->init = new ExpInitializer(loc, e);
        }
    }

    return oneSymbol(a);
}

Dsymbols *DeclMapper::VisitRecordDecl(const clang::RecordDecl *D, unsigned flags)
{
    auto& Context = calypso.pch.AST->getASTContext();
    auto& S = calypso.pch.AST->getSema();
    auto Canon = D->getCanonicalDecl();

    if (D->isImplicit() && !(flags & MapImplicit))
        return nullptr;

    auto loc = fromLoc(D->getLocation());

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
        ::warning(loc, "Value with anon record type, no D equivalent (needs workaround), discarding");
        return nullptr;
    }

    auto CRD = dyn_cast<clang::CXXRecordDecl>(D);
        // NOTE: CXXRecordDecl will disappear in a future version of Clang and only
        // RecordDecl will remain to be used for both C and C++.

    if ((flags & ForceNonPOD) ||
            (CRD && isDefined && !CRD->isPOD()))
        isPOD = false;

    CXXScope.push(D);

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
                    if (brt->ty == Tvalueof)
                        brt = brt->nextOf();

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

    for (auto I = D->field_begin(), E = D->field_end();
            I != E; ++I)
    {
        if (I->getCanonicalDecl() != *I)
            continue;

        auto field = VisitValueDecl(*I);
        if (field)
            members->append(field);
    }

    if (CRD && /* TEMPORARY HACK */ !D->isUnion())
    {
        if (!isPOD && !CRD->isDependentType())
            // Clang declares and defines the implicit default constructor lazily, so do it here
            // before adding methods.
            S.LookupDefaultConstructor(const_cast<clang::CXXRecordDecl *>(CRD));

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
                members->append(fd);
        }
    }

    // Add specific decls: vars, tags, templates, typedefs
#define SPECIFIC_ADD(DECL) \
    typedef clang::DeclContext::specific_decl_iterator<clang::DECL##Decl> DECL##_iterator; \
    for (DECL##_iterator I(D->decls_begin()), E(D->decls_end()); \
                I != E; I++) \
    { \
        if (cast<clang::Decl>((*I)->getDeclContext())->getCanonicalDecl() != Canon) \
            continue;  /* only map declarations that are semantically within the RecordDecl */ \
        if (auto s = VisitDecl(*I)) \
            members->append(s); \
    }

    SPECIFIC_ADD(Tag)
    SPECIFIC_ADD(Var)
    SPECIFIC_ADD(RedeclarableTemplate)
    SPECIFIC_ADD(TypedefName)

#undef SPECIFIC_ADD

Ldeclaration:
    CXXScope.pop();

    if (anon)
        return oneSymbol(new AnonDeclaration(loc, anon == 2, members));

    a->members = members;

    return oneSymbol(a);
}

Dsymbols *DeclMapper::VisitTypedefNameDecl(const clang::TypedefNameDecl* D)
{
    auto& Context = calypso.pch.AST->getASTContext();
    auto Ty = D->getUnderlyingType();

    if (isAnonTagTypedef(D))
        return nullptr;  // the anon tag will be mapped by VisitRecordDecl to an aggregate named after the typedef identifier

    if (auto TagTy = Ty->getAs<clang::TagType>())
    {
        auto Tag = TagTy->getDecl();

        auto Parent = cast<clang::Decl>(D->getDeclContext());
        auto TagParent = cast<clang::Decl>(Tag->getDeclContext());

        if (Tag->getName() == D->getName() &&
                TagParent->getCanonicalDecl() == Parent->getCanonicalDecl())
            return nullptr; // e.g typedef union pthread_attr_t pthread_attr_t needs to be discarded
    }

    if (D->isImplicit())
        if (Ty == Context.Int128Ty || Ty == Context.UnsignedInt128Ty)
            return nullptr; // any function using those typedefs will be discarded anyway

    auto loc = fromLoc(D->getLocation());
    auto id = fromIdentifier(D->getIdentifier());
    auto t = fromType(D->getUnderlyingType());

    if (!t)
        return nullptr;

    auto a = new AliasDeclaration(loc, id, t, D);
    return oneSymbol(a);
}

TemplateParameters *initTempParamsForOO(Loc loc, const char *op)
{
    auto dstringty = new TypeIdentifier(loc, Id::object);
    dstringty->addIdent(Lexer::idPool("string"));

    auto tpl = new TemplateParameters;
    auto tp_specvalue = new StringExp(loc, const_cast<char*>(op));
    tpl->push(new TemplateValueParameter(loc, Lexer::idPool("op"),
                                        dstringty, tp_specvalue, nullptr));

    return tpl;
}

Dsymbols *DeclMapper::VisitFunctionDecl(const clang::FunctionDecl *D)
{
    auto& S = calypso.pch.AST->getSema();

    if (isa<clang::CXXConversionDecl>(D))
        return nullptr; // TODO

    if (isa<clang::FunctionNoProtoType>(D->getType()))
        return nullptr; // functions without prototypes are afaik builtins, and since D needs a prototype they can't be mapped

    auto loc = fromLoc(D->getLocation());

    auto FPT = D->getType()->castAs<clang::FunctionProtoType>();
    auto MD = dyn_cast<clang::CXXMethodDecl>(D);
    
    auto tf = FromType(*this).fromTypeFunction(FPT, D);
    if (!tf)
    {
        ::warning(loc, "Discarding %s, non-supported argument or return type (e.g int128_t)",
                            D->getDeclName().getAsString().c_str());
        return nullptr;
    }
    assert(tf->ty == Tfunction);

    if (!D->getDeclContext()->isDependentContext())
    {
        auto D_ = const_cast<clang::FunctionDecl*>(D);
        D_->setTrivial(false);  // force its definition and Sema to resolve its exception spec
        S.MarkFunctionReferenced(clang::SourceLocation(), D_);
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

        if (isa<clang::CXXDestructorDecl>(D))
            stc &= ~(STCoverride|STCabstract); // dtors aren't added to D's VTBLs
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
    else if (D->isOverloadedOperator())
    {
        const char *op;
        auto opIdent = getIdentifierOrNull(D, &op); // will return nullptr if the operator isn't supported by D
                            // TODO map the unsupported operators anyway

        if (D->isImplicit() || !opIdent)
            return nullptr;

        // NOTE: C++ overloaded operators might be virtual, unlike D which are always final (being templates)
        //   Mapping the C++ operator to opBinary()() directly would make D lose info and overriding the C++ method impossible

        bool wrapInTemp = (op != nullptr) &&
                    !D->getDescribedFunctionTemplate();  // if it's a templated overloaded operator then the template declaration is already taken care of

        Identifier *fullIdent;
        if (wrapInTemp)
            fullIdent = getExtendedIdentifier(D);
        else
            fullIdent = opIdent;

        // Add the overridable method (or the static function)
        auto a = new Dsymbols;
        fd = new FuncDeclaration(loc, fullIdent, stc, tf, D);
        a->push(fd);

        if (wrapInTemp)
        {
            // Add the opUnary/opBinary/... template declaration
            auto tpl = initTempParamsForOO(loc, op);

            auto tf_fwd = static_cast<TypeFunction*>(tf->syntaxCopy());
            auto f_fwd = new ::FuncDeclaration(loc, loc, opIdent, STCfinal, tf_fwd);

            // Build the body of the forwarding function
            auto callargs = new Expressions;
            callargs->reserve(tf_fwd->parameters->dim);
            for (auto *p: *tf_fwd->parameters)
                callargs->push(new IdentifierExp(loc, p->ident));

            Expression *e = new IdentifierExp(loc, fullIdent);
            e = new CallExp(loc, e, callargs);

            f_fwd->fbody = new ReturnStatement(loc, e);

            // Enclose the forwarding function within the template declaration
            auto decldefs = new Dsymbols;
            decldefs->push(f_fwd);

            auto tempdecl = new ::TemplateDeclaration(loc, opIdent, tpl, nullptr, decldefs);
            a->push(tempdecl);
        }

        return a;
    }
    else
    {
        auto id = fromIdentifier(D->getIdentifier());
        fd = new FuncDeclaration(loc, id, stc, tf, D);
    }

    return oneSymbol(fd);
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

Dsymbols *DeclMapper::VisitRedeclarableTemplateDecl(const clang::RedeclarableTemplateDecl *D)
{
    if (!isa<clang::ClassTemplateDecl>(D) && !isa<clang::TypeAliasTemplateDecl>(D)
         && !isa<clang::FunctionTemplateDecl>(D))
        return nullptr; // temporary

    if (auto FTD = dyn_cast<clang::FunctionTemplateDecl>(D))
        if (isa<clang::CXXConversionDecl>(FTD->getTemplatedDecl()))
            return nullptr;

    auto loc = fromLoc(D->getLocation());
    const char *op = nullptr;
    auto id = getIdentifierOrNull(D, &op);

    if (!id)
        return nullptr; // TODO: map unsupported overloaded operators

    auto Def = D;
    if (auto CTD = dyn_cast<clang::ClassTemplateDecl>(D))
        Def = getDefinition(CTD);

    auto tpl = !op ? new TemplateParameters : initTempParamsForOO(loc, op);
    auto TPL = Def->getTemplateParameters();

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

    auto s = VisitDecl(Def->getTemplatedDecl()->getCanonicalDecl());

    if (!s)
        return nullptr;

    auto decldefs = new Dsymbols;
    decldefs->append(s);

    templateParameters.pop_back();

    auto a = new TemplateDeclaration(loc, id, tpl, decldefs, D);
    return oneSymbol(a);
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

    // Recreate the scope stack, esp. important for nested template instances
    struct CXXScopeRebuilder
    {
        decltype(CXXScope) &S;
        CXXScopeRebuilder(decltype(S) &S) : S(S) {}

        void build(const clang::Decl *D, bool push = false)
        {
            auto Parent = cast<clang::Decl>(D->getDeclContext());
            if (!isa<clang::TranslationUnitDecl>(Parent))
                build(Parent, true);

            if (push && isa<clang::CXXRecordDecl>(D))
                S.push(D);
        }
    };
    CXXScopeRebuilder(CXXScope).build(D);

    templateParameters.push_back(CT->getTemplateParameters());
    auto a = VisitRecordDecl(D, flags);
    templateParameters.pop_back();

    assert(a->dim);
    return (*a)[0];
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
Dsymbols *DeclMapper::VisitClassTemplateSpecializationDecl(const clang::ClassTemplateSpecializationDecl *D)
{   TemplateDeclaration *a;

    D = getDefinition(D);

    if (!D->isExplicitSpecialization())
        return nullptr;

    auto Partial = dyn_cast<clang::ClassTemplatePartialSpecializationDecl>(D);
        // NOTE: D's partial specializations != C++'s partial specializations
        // The mapping provides a "close" but not exact approximation of equivalent template specs in D (for reflection),
        // but TemplateDeclaration::findBestMatch is skipped since the choice is done by Clang anyway.

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
    decldefs->append(ad);

    templateParameters.pop_back();

    a = new TemplateDeclaration(loc, id, tpl, decldefs, D);
    return oneSymbol(a);
}

Dsymbols *DeclMapper::VisitEnumDecl(const clang::EnumDecl* D)
{
    if (!D->isCompleteDefinition())
    {
        D = D->getDefinition();
        if (!D)
            return nullptr; // forward declaration without definition, skip
    }

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
    CXXScope.push(D);

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

    CXXScope.pop();
    return oneSymbol(e);
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
static clang::DeclContext::lookup_const_result lookup(const clang::DeclContext *DC,
                                                   Identifier *id)
{
    auto& AST = calypso.pch.AST;
    auto& Table = AST->getPreprocessor().getIdentifierTable();
    auto& II = Table.get(id->string);

    return DC->lookup(clang::DeclarationName(&II));
}

bool isOverloadedOperatorWithTagOperand(const clang::Decl *D,
                                const clang::NamedDecl *SpecificTag = nullptr)
{
    auto& Context = calypso.getASTContext();

    auto Func = dyn_cast<clang::FunctionDecl>(D);
    if (auto FuncTemp = dyn_cast<clang::FunctionTemplateDecl>(D))
        Func = FuncTemp->getTemplatedDecl();

    if (!Func || !Func->isOverloadedOperator())
        return false;

    assert(Func->getNumParams() > 0);
    if (Func->getNumParams() > 2)
        return false; // [] and () cannot be non-member (FIXME: not entirely sure about (), couldn't find the source)

    const clang::NamedDecl *OpTyDecl = nullptr;

    for (unsigned I = 0; I < Func->getNumParams(); I++)
    {
        auto ParamTy = Func->getParamDecl(I)->getType().getNonReferenceType()
                                .getDesugaredType(Context).getCanonicalType();
        auto Ty = ParamTy.getTypePtr();

        if (auto TagTy = dyn_cast<clang::TagType>(Ty))
        {
            OpTyDecl = TagTy->getDecl();
            break;
        }

        if (auto TempSpec = dyn_cast<clang::TemplateSpecializationType>(Ty))
            if (auto Temp = TempSpec->getTemplateName().getAsTemplateDecl())
            {
                if (isa<clang::ClassTemplateDecl>(Temp))
                {
                    OpTyDecl = Temp;
                    break;
                }

                if (isa<clang::TypeAliasTemplateDecl>(Temp))
                    assert(false && "Wrongly assumed that it would desugared");
            }
    }

    if (!OpTyDecl)
        return false; // neither LHS nor RHS has a tag type

    if (!SpecificTag)
        return true; // we're not looking for a specific type

    return OpTyDecl->getCanonicalDecl() == SpecificTag->getCanonicalDecl();
}

static clang::Module *tryFindClangModule(Loc loc, Identifiers *packages, Identifier *id, Package *&p)
{
    auto MMap = calypso.pch.MMap;

    if (!MMap)
        return nullptr;

    Package *pkg = Module::rootPackage;
    clang::Module *M = nullptr;

    for (size_t i = 1; i < packages->dim; i++)
    {
        Identifier *pid = (*packages)[i];

        llvm::StringRef name(pid->string, pid->len);
        M = MMap->lookupModuleQualified(name, M);
        if (!M)
            return nullptr;

        pkg = static_cast<Package*>(pkg->symtab->lookup(pid));
        assert(pkg);
    }

    llvm::StringRef name(id->string, id->len);
    M = MMap->lookupModuleQualified(name, M);
    if (!M)
        return nullptr;

    p = pkg;
    return M;
}

static inline bool isTopLevelInNamespaceModule (const clang::Decl *D)
{
    auto Tag = dyn_cast<clang::TagDecl>(D);
    if (Tag && Tag->getIdentifier())
        return false; // anonymous tags are added as well

    if (!Tag && !isa<clang::FunctionDecl>(D) &&
            !isa<clang::VarDecl>(D) &&
            !isa<clang::TypedefNameDecl>(D) &&
            !isa<clang::FunctionTemplateDecl>(D) &&
            !isa<clang::TypeAliasTemplateDecl>(D))
        return false;

    if (isOverloadedOperatorWithTagOperand(D))
        return false;  // non-member overloaded operators with class/struct/enum operands are included in their own module

    return true;
}

static void mapNamespace(DeclMapper &mapper,
                             const clang::DeclContext *DC,
                             Dsymbols *members)
{
    auto CanonDC = cast<clang::Decl>(DC)->getCanonicalDecl();
    auto MMap = calypso.pch.MMap;
    auto& SrcMgr = calypso.getASTUnit()->getSourceManager();

    auto D = DC->decls_begin(),
            DE = DC->decls_end();

    for (; D != DE; ++D)
    {
        if (cast<clang::Decl>(D->getDeclContext())->getCanonicalDecl()
                != CanonDC)
            continue;  // only map declarations that are semantically within the DeclContext

        auto DLoc = SrcMgr.getFileLoc((*D)->getLocation());
        if (MMap && DLoc.isFileID()
                && MMap->findModuleForHeader(
                    SrcMgr.getFileEntryForID(SrcMgr.getFileID(DLoc))))
            continue;  // skip decls which are parts of a Clang module

        if (auto LinkSpec = dyn_cast<clang::LinkageSpecDecl>(*D))
        {
            mapNamespace(mapper, LinkSpec, members);
            continue;
        }
        else if (!isTopLevelInNamespaceModule(*D))
            continue;

        if (auto s = mapper.VisitDecl(*D))
            members->append(s);
    }
}

static void mapClangModule(DeclMapper &mapper,
                             clang::Module *M,
                             Dsymbols *members)
{
    auto AST = calypso.getASTUnit();
    auto& SrcMgr = AST->getSourceManager();

    llvm::SmallVector<clang::Decl*, 32> Decls;

    for (auto Header: M->Headers[clang::Module::HK_Normal])
        AST->findFileRegionDecls(SrcMgr.translateFile(Header.Entry),
            0, 0, Decls);

    std::function<void(const clang::Decl *)> Map = [&] (const clang::Decl *D)
    {
        if (auto LinkSpec = dyn_cast<clang::LinkageSpecDecl>(D))
        {
            for (auto LD: LinkSpec->decls())
                Map(LD);
            return;
        }

        auto DC = D->getDeclContext();
        if (!isa<clang::TranslationUnitDecl>(DC)
                && !isa<clang::NamespaceDecl>(DC)
                && !isa<clang::LinkageSpecDecl>(DC))
            return;

        if (!isTopLevelInNamespaceModule(D))
            return;

        if (auto s = mapper.VisitDecl(D))
            members->append(s);
    };

    for (auto D: Decls)
        Map(D);
}

Module *Module::load(Loc loc, Identifiers *packages, Identifier *id)
{
    auto& Context = calypso.getASTContext();
    auto& S = calypso.pch.AST->getSema();
    auto& Diags = calypso.pch.Diags;

    S.CurContext = Context.getTranslationUnitDecl(); // HACK? Needed for declaring implicit ctors and dtors
    if (!S.TUScope)
        // Clang BUG? TUScope isn't set when no Parser is used, but required by template instantiations (e.g LazilyCreateBuiltin)
        S.TUScope = new clang::Scope(nullptr, clang::Scope::DeclScope, *Diags);

    const clang::DeclContext *DC = Context.getTranslationUnitDecl();
    Package *pkg = rootPackage;

    assert(packages && packages->dim);

    // First check if it's a Clang module
    auto M = tryFindClangModule(loc, packages, id, pkg);

    // If not, follow the decls
    if (!M)
    {
        for (size_t i = 1; i < packages->dim; i++)
        {
            Identifier *pid = (*packages)[i];

            auto R = lookup(DC, pid);
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

            pkg = static_cast<Package*>(pkg->symtab->lookup(pid));
            assert(pkg);

            DC = NSN;
        }
    }

    auto m = new Module(moduleName(packages, id).c_str(),
                        id, packages);
    m->members = new Dsymbols;
    m->parent = pkg;
    m->loc = loc;

    DeclMapper mapper(m);

    if (M)
    {
        m->rootMod = M;
        mapClangModule(mapper, M, m->members);
    }
    else if (strcmp(id->string, "_") == 0)  // Hardcoded module with all the top-level non-tag decls + the anonymous tags of a namespace which aren't in a Clang module
    {
        m->rootDecl = cast<clang::Decl>(DC)->getCanonicalDecl();

        // All non-tag declarations inside the namespace go in _ (this is horrible for C functions of course, this will be fixed by the switch to Clang module system)
        auto NS = dyn_cast<clang::NamespaceDecl>(DC);

        if (!NS)
        {
            assert(isa<clang::TranslationUnitDecl>(DC));

#if 1
            mapNamespace(mapper, DC, m->members);
#endif
        }
        else
        {
            auto I = NS->redecls_begin(),
                    E = NS->redecls_end();

            for (; I != E; ++I)
            {
                DC = *I;
                mapNamespace(mapper, DC, m->members);
            }
        }
    }
    else
    {
        clang::NamedDecl *D;

        // Lookups can't find the implicit __va_list_tag record
        if (id == Lexer::idPool("__va_list_tag") && packages->dim == 1)
        {
            D = Context.getVaListTagType()
                ->getAs<clang::RecordType>()->getDecl();
        }
        else
        {
            auto R = lookup(DC, id);
            if (R.empty())
            {
                ::error(loc, "no C++ module named %s", id->toChars());
                fatal();
            }

            D = R[0];
        }

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

        if (auto s = mapper.VisitDecl(D, DeclMapper::MapImplicit))
            m->members->append(s);

        // Special case for class template, we need to add explicit specializations to the module as well
        if (auto CTD = dyn_cast<clang::ClassTemplateDecl>(D))
        {
            llvm::SmallVector<clang::ClassTemplatePartialSpecializationDecl *, 2> PS;
            CTD->getPartialSpecializations(PS);

            for (auto PartialSpec: PS)
                if (auto s = mapper.VisitDecl(PartialSpec->getCanonicalDecl()))
                    m->members->append(s);

            for (auto Spec: CTD->specializations())
                if (auto s = mapper.VisitDecl(Spec->getCanonicalDecl()))
                    m->members->append(s);
        }

        // Add the non-member overloaded operators that are meant to work with this record/enum
        for (int Op = 1; Op < clang::NUM_OVERLOADED_OPERATORS; Op++)
        {
            auto OpName = Context.DeclarationNames.getCXXOperatorName(
                        static_cast<clang::OverloadedOperatorKind>(Op));
            auto Operators = getDeclContextNamedOrTU(D)->lookup(OpName);

            // WARNING: lookups will only search in the top-most namespace for overloaded operators
            // Can non-member operators be located in a parent namespace or the TU?
            // Never seen that happening but if it's allowed then it's a FIXME

            for (auto OverOp: Operators)
                if (isOverloadedOperatorWithTagOperand(OverOp, D))
                    if (auto s = mapper.VisitDecl(OverOp))
                        m->members->append(s);
        }

//         srcFilename = AST->getSourceManager().getFilename(TD->getLocation());
    }
    
    amodules.push_back(m);
    pkg->symtab->insert(m);
    return m;
}

}
