// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_TYPES_H
#define DMD_CPP_TYPES_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include <map>
#include <stack>
#include "cpp/calypso.h"
#include "cpp/cppmodule.h"
#include "mars.h"
#include "mtype.h"
#include "arraytypes.h"
#include "clang/AST/Type.h"
#include "clang/Basic/TargetInfo.h"

class Module;
class Dsymbol;
class Identifier;
class Import;
class Scope;
class Type;
class TypeQualified;
class TypeFunction;

namespace clang
{
class Decl;
class ClassTemplateSpecializationDecl;
class TemplateParameterList;
}

namespace cpp
{
class TypeQualifiedBuilder;

class BuiltinTypes
{
public:
    std::map<const clang::BuiltinType *, Type*> toD;
    std::map<Type*, const clang::BuiltinType *> toClang;

    void build(clang::ASTContext &Context);

protected:
    inline void map(clang::CanQualType &CQT, Type* t);
    Type *toInt(clang::TargetInfo::IntType intTy);
};

class TypeMapper
{
public:
    TypeMapper(cpp::Module *mod = nullptr);  // mod can be null if no implicit import is needed

    bool addImplicitDecls = true;
    Dsymbols *substsyms = nullptr; // only for TempateInstance::correctTiargs (partial spec arg deduction)

    std::stack<const clang::Decl *> CXXScope;
    void rebuildCXXScope(const clang::Decl *RightMost);

    // Clang -> DMD
    Type *fromType(const clang::QualType T);

    class FromType // type-specific state
    {
    public:
        TypeMapper &tm;
        TypeQualified *prefix; // special case for NNS qualified types

        const clang::Expr *TypeOfExpr = nullptr;

        FromType(TypeMapper &tm, TypeQualified *prefix = nullptr);

        Type *operator()(const clang::QualType T);
        Type *fromTypeUnqual(const clang::Type *T);
        Type *fromTypeBuiltin(const clang::BuiltinType *T);
        Type *fromTypeComplex(const clang::ComplexType *T);
        Type *fromTypeArray(const clang::ArrayType *T);
        Type *fromTypeVector(const clang::VectorType *T);
        Type *fromTypeTypedef(const clang::TypedefType *T);
        Type *fromTypeEnum(const clang::EnumType *T);
        Type *fromTypeRecord(const clang::RecordType *T);
        Type *fromTypeElaborated(const clang::ElaboratedType *T);
        Type *fromTypeTemplateSpecialization(const clang::TemplateSpecializationType *T);
        Type *fromTypeTemplateTypeParm(const clang::TemplateTypeParmType *T);
        Type *fromTypeSubstTemplateTypeParm(const clang::SubstTemplateTypeParmType *T);
        Type *fromTypeInjectedClassName(const clang::InjectedClassNameType *T);
        Type *fromTypeDependentName(const clang::DependentNameType *T);
        Type *fromTypeDependentTemplateSpecialization(const clang::DependentTemplateSpecializationType *T);
        Type *fromTypeTypeOfExpr(const clang::TypeOfExprType *T);
        Type *fromTypeDecltype(const clang::DecltypeType *T);
        Type *fromTypePackExpansion(const clang::PackExpansionType *T);
        TypeFunction *fromTypeFunction(const clang::FunctionProtoType *T,
                        const clang::FunctionDecl *FD = nullptr);

        RootObject *fromTemplateArgument(const clang::TemplateArgument *Arg,
                    const clang::NamedDecl *Param = nullptr);  // NOTE: Param is required when the parameter type is an enum, because in the AST enum template arguments are resolved to uint while DMD expects an enum constant or it won't find the template decl. Is this a choice or a compiler bug/limitation btw?
        Objects *fromTemplateArguments(const clang::TemplateArgument *First,
                    const clang::TemplateArgument *End,
                    const clang::TemplateParameterList *ParamList = nullptr);
        TypeQualified *fromNestedNameSpecifier(const clang::NestedNameSpecifier *NNS);
        TypeQualified *fromTemplateName(const clang::TemplateName Name,
                    const clang::TemplateArgument *ArgBegin = nullptr,
                    const clang::TemplateArgument *ArgEnd = nullptr);  // returns a template or a template instance
                // if it's a template it's not actually a type but a symbol, but that's how parsing TemplateAliasParameter works anyway

        Type *typeQualifiedFor(clang::NamedDecl* D,
                            const clang::TemplateArgument* ArgBegin = nullptr,
                            const clang::TemplateArgument* ArgEnd = nullptr);

        template<typename _Type>
         Type *fromTypeOfExpr(const _Type *T);

    private:
        Type *fromType(const clang::QualType T);  // private alias

        TypeQualified *fromNestedNameSpecifierImpl(const clang::NestedNameSpecifier *NNS);
    };

    // DMD -> Clang
    clang::QualType toType(Loc loc, Type* t, Scope *sc, StorageClass stc = STCundefined);

protected:
    cpp::Module *mod;

    llvm::SmallDenseMap<Module::RootKey, Import*, 8> implicitImports;
    llvm::DenseMap<const clang::NamedDecl*, Dsymbol*> declMap;  // fast lookup of mirror decls

    llvm::SmallVector<const clang::TemplateParameterList*, 4> templateParameters;
    Identifier *getIdentifierForTemplateTypeParm(const clang::TemplateTypeParmType *T);
    Identifier *getIdentifierForTemplateTemplateParm(const clang::TemplateTemplateParmDecl *D);

    bool isNonExplicitInjectedClassName(const clang::Decl *D); // misleading name? not InjectedClassNameType
    bool isRecursivelyInstantiated(const clang::TemplateName Name,
                    const clang::TemplateArgument *ArgBegin,
                    const clang::TemplateArgument *ArgEnd);

    const clang::Decl *GetRootForTypeQualified(clang::NamedDecl* D);

    ::Import *AddImplicitImportForDecl(const clang::NamedDecl *D, bool fake = false);
    ::Import *BuildImplicitImport(const clang::Decl *D);
    ::Import *BuildImplicitImport(const clang::Decl *D, const clang::Module *Mod);
    const clang::Decl *GetImplicitImportKeyForDecl(const clang::NamedDecl *D);
    const clang::Decl *GetNonNestedContext(const clang::Decl *D);  // returns the "root" for qualified types

    Type *trySubstitute(const clang::Decl *D);

    friend class cpp::TypeQualifiedBuilder;
};

bool isNonPODRecord(const clang::QualType T);
bool isNonSupportedType(clang::QualType T);
bool isClassReferenceType(const clang::QualType T);
const clang::DeclContext *getDeclContextNamedOrTU(const clang::Decl *D); // to skip LinkageSpec
const clang::NamedDecl *getTemplateSpecializedDecl(const clang::ClassTemplateSpecializationDecl *Spec);
const clang::TagDecl *isAnonTagTypedef(const clang::TypedefNameDecl* D);

}

#endif
