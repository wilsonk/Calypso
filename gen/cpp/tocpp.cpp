#include "cpp/calypso.h"
#include "cpp/cppaggregate.h"
#include "mtype.h"
#include "clang/AST/Type.h"

namespace cpp
{

clang::QualType LangPlugin::fromType(Type *t)
{
    auto& Context = getASTContext();

    if (auto builtin = builtinTypes.toClang[t])
        return clang::QualType(builtin, 0);

    if (t->ty == Tpointer || t->ty == Treference)
    {
        auto Pointee = fromType(t->nextOf());

        if (t->ty == Tpointer)
            return Context.getPointerType(Pointee);
        else
            return Context.getLValueReferenceType(Pointee);
    }

    if (t->ty == Tstruct || t->ty == Tclass)
    {
        const clang::RecordDecl *RD;

        if (t->ty == Tstruct)
        {
            auto ts = static_cast<TypeStruct*>(t);
            RD = static_cast<cpp::StructDeclaration*>(ts->sym)->RD;
        }
        else
        {
            auto tc = static_cast<TypeClass*>(t);
            RD = static_cast<cpp::ClassDeclaration*>(tc->sym)->RD;
        }

        return Context.getRecordType(RD);
    }

    assert(false && "Unhandled D -> Clang type conversion");
    return clang::QualType();
}

}