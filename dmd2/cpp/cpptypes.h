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
class TypeFunction;

namespace clang
{
class Decl;
}

namespace cpp
{

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
    TypeMapper(::Module *mod = nullptr);  // mod can be null if no implicit import is needed

    // Type conversions
    Type *toType(const clang::QualType T);  // main type conversion method

    Type *toTypeUnqual(const clang::Type *T);
    Type *toTypeBuiltin(const clang::BuiltinType *T);
    Type *toTypeComplex(const clang::ComplexType *T);
    Type *toTypeRecord(const clang::RecordType *T);
    TypeFunction *toTypeFunction(const clang::FunctionProtoType *T);

protected:
    Module *mod;

    llvm::SmallDenseMap<const clang::Decl*, Import*, 8> implicitImports;
    llvm::DenseMap<const clang::NamedDecl*, Dsymbol*> declMap;  // fast lookup of mirror decls

    void AddImplicitImportForDecl(const clang::NamedDecl* ND);
    const clang::Decl* GetImplicitImportKeyForDecl(const clang::NamedDecl* ND);

    Import* BuildImplicitImport(const clang::Decl* ND);
    bool BuildImplicitImportInternal(const clang::DeclContext* DC, Loc loc,
            Identifiers* sPackages, Identifier*& sModule);
};

}

#endif