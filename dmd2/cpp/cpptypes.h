// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_TYPES_H
#define DMD_CPP_TYPES_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include <map>
#include <stack>
#include "mars.h"
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
class Module;
class TypeQualifiedBuilder;

class BuiltinTypes
{
public:
    // We need both maps, the second one is used by gen/
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
    ::Module *instMod = nullptr; // only used during the semantic pass for C++ template instantiations

    ::Module *getInstantiatingModule() { return instMod ? instMod : (::Module*) mod; }

    std::stack<const clang::Decl *> CXXScope;

    // Clang -> DMD
    Type *fromType(const clang::QualType T);

    class FromType // type-specific state
    {
    public:
        TypeMapper &tm;
        TypeQualified *prefix; // special case for NNS qualified types

        FromType(TypeMapper &tm, TypeQualified *prefix = nullptr);

        Type *operator()(const clang::QualType T);
        Type *fromTypeUnqual(const clang::Type *T);
        Type *fromTypeBuiltin(const clang::BuiltinType *T);
        Type *fromTypeComplex(const clang::ComplexType *T);
        Type *fromTypeArray(const clang::ArrayType *T);
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
        Type *fromTypeDecltype(const clang::DecltypeType *T);
        Type *fromTypePackExpansion(const clang::PackExpansionType *T);
        TypeFunction *fromTypeFunction(const clang::FunctionProtoType *T);

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

        Type *typeQualifiedFor(clang::NamedDecl* ND,
                            const clang::TemplateArgument* ArgBegin = nullptr,
                            const clang::TemplateArgument* ArgEnd = nullptr);

    private:
        Type *fromType(const clang::QualType T);  // private alias
    };

    // DMD -> Clang
    clang::QualType toType(Loc loc, Type* t, Scope *sc);

protected:
    cpp::Module *mod;

    llvm::SmallDenseMap<const clang::Decl*, Import*, 8> implicitImports;
    llvm::DenseMap<const clang::NamedDecl*, Dsymbol*> declMap;  // fast lookup of mirror decls

    llvm::SmallVector<const clang::TemplateParameterList*, 4> templateParameters;
    Identifier *getIdentifierForTemplateTypeParm(const clang::TemplateTypeParmType *T);
    Identifier *getIdentifierForTemplateTemplateParm(const clang::TemplateTemplateParmDecl *D);

    bool isInjectedClassName(clang::Decl *D); // misleading name although that's what it is

    void AddImplicitImportForDecl(const clang::NamedDecl* ND);

    ::Import *BuildImplicitImport(const clang::Decl *ND);
    bool BuildImplicitImportInternal(const clang::DeclContext *DC, Loc loc,
                                     Identifiers *sPackages, Identifier *&sModule);
    
    bool isNonPODRecord(const clang::QualType T);
    const clang::Decl *GetImplicitImportKeyForDecl(const clang::NamedDecl *D);
    const clang::Decl *GetTopMostDeclContext(const clang::Decl *D);  // returns the "root" for qualified types

    friend class cpp::TypeQualifiedBuilder;
};

const clang::DeclContext *getDeclContextNamedOrTU(const clang::Decl *D); // to skip LinkageSpec
const clang::NamedDecl *getTemplateSpecializedDecl(const clang::ClassTemplateSpecializationDecl *Spec);

}

#endif