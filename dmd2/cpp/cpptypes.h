// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_TYPES_H
#define DMD_CPP_TYPES_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include <map>
#include "mars.h"
#include "arraytypes.h"
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

    bool addImplicitDecls = true; // this is a temporary safety for TypeMapper uses that should not add implicit decls, a simple mod == null will replace the assert and this variable later

    // Type conversions
    Type *fromType(const clang::QualType T);

    Type *fromTypeUnqual(const clang::Type *T);
    Type *fromTypeBuiltin(const clang::BuiltinType *T);
    Type *fromTypeComplex(const clang::ComplexType *T);
    Type *fromTypeArray(const clang::ArrayType *T);
    Type *fromTypeTypedef(const clang::TypedefType *T);
    Type *fromTypeEnum(const clang::EnumType *T);
    Type *fromTypeRecord(const clang::RecordType *T);
    Type *fromTypeTemplateSpecialization(const clang::TemplateSpecializationType *T);
    Type *fromTypeTemplateTypeParm(const clang::TemplateTypeParmType *T);
    Type *fromTypeSubstTemplateTypeParm(const clang::SubstTemplateTypeParmType *T);
    Type *fromTypeInjectedClassName(const clang::InjectedClassNameType *T);
    Type *fromTypeDependentName(const clang::DependentNameType *T);
    Type *fromTypeDependentTemplateSpecialization(const clang::DependentTemplateSpecializationType *T);
    Type *fromTypeDecltype(const clang::DecltypeType *T);
    TypeFunction *fromTypeFunction(const clang::FunctionProtoType *T);

    RootObject *fromTemplateArgument(const clang::TemplateArgument *Arg,
                const clang::NamedDecl *Param = nullptr);  // NOTE: Param is required when the parameter type is an enum, because in the AST enum template arguments are resolved to uint while DMD expects an enum constant or it won't find the template decl. Is this a choice or a compiler bug/limitation btw?
    TypeQualified *fromNestedNameSpecifier(const clang::NestedNameSpecifier *NNS);
    TypeQualified *fromTemplateName(const clang::TemplateName Name,
                const clang::TemplateArgument *ArgBegin = nullptr,
                const clang::TemplateArgument *ArgEnd = nullptr);  // returns a template or a template instance
            // if it's a template it's not actually a type but a symbol, but that's how parsing TemplateAliasParameter works anyway

    const clang::Decl* GetImplicitImportKeyForDecl(const clang::NamedDecl* ND);
    Type *typeQualifiedFor(clang::NamedDecl* ND,
                        const clang::TemplateArgument* ArgBegin = nullptr,
                        const clang::TemplateArgument* ArgEnd = nullptr);

protected:
    cpp::Module *mod;

    llvm::SmallDenseMap<const clang::Decl*, Import*, 8> implicitImports;
    llvm::DenseMap<const clang::NamedDecl*, Dsymbol*> declMap;  // fast lookup of mirror decls

    llvm::SmallVector<const clang::TemplateParameterList*, 4> templateParameters;
    Identifier *getIdentifierForTemplateTypeParm(const clang::TemplateTypeParmType *T);
    Identifier *getIdentifierForTemplateTemplateParm(const clang::TemplateTemplateParmDecl *D);

    Objects *fromTemplateArguments(const clang::TemplateArgument *First,
                const clang::TemplateArgument *End,
                const clang::TemplateDecl *TD = nullptr);

    void AddImplicitImportForDecl(const clang::NamedDecl* ND);

    ::Import* BuildImplicitImport(const clang::Decl* ND);
    bool BuildImplicitImportInternal(const clang::DeclContext* DC, Loc loc,
            Identifiers* sPackages, Identifier*& sModule);
    
    bool isNonPODRecord(const clang::QualType T);

    friend class cpp::TypeQualifiedBuilder;
};

class TypeQualifiedBuilder
{
public:
    TypeMapper &tm;

    const clang::Decl* Root;
    const clang::TemplateArgument *TopTempArgBegin,
        *TopTempArgEnd;

protected:
    void addInst(TypeQualified *&tqual,
                 clang::NamedDecl* D,
                 const clang::TemplateArgument *TempArgBegin,
                 const clang::TemplateArgument *TempArgEnd);

public:
    TypeQualifiedBuilder(TypeMapper &tm, const clang::Decl* Root,
        const clang::TemplateArgument *TempArgBegin = nullptr,
        const clang::TemplateArgument *TempArgEnd = nullptr)
        : tm(tm), Root(Root),
          TopTempArgBegin(TempArgBegin),
          TopTempArgEnd(TempArgEnd) {}

    TypeQualified *get(clang::NamedDecl* ND);
};

const clang::DeclContext *getDeclContextNamedOrTU(const clang::Decl *D); // to skip LinkageSpec

}

#endif