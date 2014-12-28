// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPDECLARATION_H
#define DMD_CPP_CPPDECLARATION_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root.h"
#include "calypso.h"
#include "dsymbol.h"

#include "../declaration.h"

namespace clang
{
    class ValueDecl;
    class FunctionProtoType;
    class FunctionDecl;
    class CXXConstructorDecl;
    class CXXDestructorDecl;
}

namespace cpp
{

class VarDeclaration : public ::VarDeclaration
{
public:
    CALYPSO_LANGPLUGIN
    
    const clang::ValueDecl *VD;

    VarDeclaration(Loc loc, Identifier *id,
                   const clang::ValueDecl *VD, Type *t);
    VarDeclaration(const VarDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
};

class FuncDeclaration : public ::FuncDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::FunctionDecl *FD;

    FuncDeclaration(Loc loc, Identifier *id, StorageClass storage_class,
                    Type* type, const clang::FunctionDecl *FD);
    FuncDeclaration(const FuncDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
};

class CtorDeclaration : public ::CtorDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::CXXConstructorDecl *CCD;

    CtorDeclaration(Loc loc, StorageClass storage_class,
                    Type* type, const clang::CXXConstructorDecl *CCD);
    CtorDeclaration(const CtorDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
};

class DtorDeclaration : public ::DtorDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::CXXDestructorDecl *CDD;

    DtorDeclaration(Loc loc, StorageClass storage_class,
                    Identifier *id, const clang::CXXDestructorDecl *CDD);
    DtorDeclaration(const DtorDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
};

#define IMPLEMENT_syntaxCopy(Class) \
    Dsymbol* Class::syntaxCopy(Dsymbol* s) \
    { \
        if (!s) \
            s = new cpp::Class(*this); \
        return ::Class::syntaxCopy(s); \
    }
// NOTE: we use copy constructors only to copy the arguments passed to the main constructor, the rest is handled by syntaxCopy

class DeclMapper : public TypeMapper
{
public:
    DeclMapper(Module *mod) : TypeMapper(mod) {} // hmm why does putting into .cpp give me a link error?

    inline PROT toPROT(clang::AccessSpecifier AS);

    // Declarations
    Dsymbols *VisitDeclContext(const clang::DeclContext *DC);

    Dsymbol *VisitDecl(const clang::Decl *D);

    Dsymbol *VisitValueDecl(const clang::ValueDecl *D);
    Dsymbol *VisitRecordDecl(const clang::RecordDecl* D, unsigned flags = 0);
    Dsymbol *VisitTypedefNameDecl(const clang::TypedefNameDecl *D);
    Dsymbol *VisitFunctionDecl(const clang::FunctionDecl *D);
    Dsymbol *VisitTemplateDecl(const clang::TemplateDecl *D);
    Dsymbol *VisitClassTemplateSpecializationDecl(const clang::ClassTemplateSpecializationDecl *D);
    Dsymbol *VisitEnumDecl(const clang::EnumDecl *D);

    TemplateParameter *VisitTemplateParameter(const clang::NamedDecl *Param,
                                                                    const clang::TemplateArgument *SpecArg = nullptr); // in DMD explicit specializations use parameters, whereas Clang uses args

    static const unsigned ForceNonPOD = 1 << 0; // When a templace declaration is non-POD, we want the explicit template specializations to be non-POD too even if isPOD() is true

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

protected:
    Scope *sc = nullptr;
};

}

#endif /* DMD_CPP_CPPDECLARATION_H */
