// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_TYPES_H
#define DMD_CPP_TYPES_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include <map>
#include "mars.h"
#include "clang/AST/Type.h"
#include "clang/Basic/TargetInfo.h"

class Module;
class Dsymbol;
class Identifier;
class Import;
class Type;
class TypeQualified;
class TypeFunction;

namespace clang
{
class Decl;
}

namespace cpp
{
class Module;

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

    bool addImplicitDecls = true; // this is a temporary safety for TypeMapper uses that should not add implicit decls, a simple mod == null will replace the assert and this variable later

    // Type conversions
    Type *toType(const clang::QualType T);  // main type conversion method

    Type *toTypeUnqual(const clang::Type *T);
    Type *toTypeBuiltin(const clang::BuiltinType *T);
    Type *toTypeComplex(const clang::ComplexType *T);
    Type *toTypeTypedef(const clang::TypedefType *T);
    Type *toTypeEnum(const clang::EnumType *T);
    Type *toTypeRecord(const clang::RecordType *T);
    Type *toTypeElaborated(const clang::ElaboratedType *T);
    Type *toTypeTemplateSpecialization(const clang::TemplateSpecializationType *T);
    Type *toTypeTemplateTypeParm(const clang::TemplateTypeParmType *T);
    Type *toTypeSubstTemplateTypeParm(const clang::SubstTemplateTypeParmType *T);
    Type *toTypeDependentName(const clang::DependentNameType *T);
    Type *toTypeInjectedClassName(const clang::InjectedClassNameType *T);
    Type *toTypeAdjusted(const clang::AdjustedType *T);
    TypeFunction *toTypeFunction(const clang::FunctionProtoType *T);

protected:
    cpp::Module *mod;

    llvm::SmallDenseMap<const clang::Decl*, Import*, 8> implicitImports;
    llvm::DenseMap<const clang::NamedDecl*, Dsymbol*> declMap;  // fast lookup of mirror decls

    void AddImplicitImportForDecl(const clang::NamedDecl* ND);
    const clang::Decl* GetImplicitImportKeyForDecl(const clang::NamedDecl* ND);

public: // HACK
    ::Import* BuildImplicitImport(const clang::Decl* ND);
protected:
    bool BuildImplicitImportInternal(const clang::DeclContext* DC, Loc loc,
            Identifiers* sPackages, Identifier*& sModule);
    TypeQualified *typeQualifiedFor(const clang::NamedDecl* ND,
                                    const clang::TemplateArgument* TempArgBegin = nullptr,
                                    const clang::TemplateArgument* TempArgEnd = nullptr);
    
    bool isNonPODRecord(const clang::QualType T);
};

}

#endif