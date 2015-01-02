// Contributed by Elie Morisse, same license DMD uses

#include "cpp/cppexpression.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cpptypes.h"
#include "id.h"
#include "template.h"

#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

static Type *getAPIntDType(const llvm::APInt &i);

Objects *fromASTTemplateArgumentListInfo(
            const clang::ASTTemplateArgumentListInfo &Args,
            TypeMapper &tymap)
{
    auto tiargs = new Objects;

    for (unsigned i = 0; i < Args.NumTemplateArgs; i++)
    {
        auto Arg = &Args[i].getArgument();
        tiargs->push(tymap.fromTemplateArgument(Arg));
    }

    return tiargs;
}

Expression* ExprMapper::fromUnaExp(const clang::UnaryOperator *E)
{
    auto loc = fromLoc(E->getLocStart());
    auto sub = fromExpression(E->getSubExpr());
    switch (E->getOpcode())
    {
        case clang::UO_Plus: return sub;
        case clang::UO_Minus: return new NegExp(loc, sub);
        case clang::UO_Not: return new NotExp(loc, sub);
        case clang::UO_LNot: return new NotExp(loc, sub);
    }

    llvm::llvm_unreachable_internal("Unhandled C++ unary operation exp");
}

Expression* ExprMapper::fromBinExp(const clang::BinaryOperator* E)
{
    auto loc = fromLoc(E->getLocStart());
    auto lhs = fromExpression(E->getLHS());
    auto rhs = fromExpression(E->getRHS());

    switch (E->getOpcode())
    {
        case clang::BO_Add: return new AddExp(loc, lhs, rhs);
        case clang::BO_Sub: return new MinExp(loc, lhs, rhs);
        case clang::BO_Mul: return new MulExp(loc, lhs, rhs);
        case clang::BO_Div: return new DivExp(loc, lhs, rhs);
        case clang::BO_Rem: return new ModExp(loc, lhs, rhs);

        case clang::BO_And: return new AndExp(loc, lhs, rhs);
        case clang::BO_Or: return new OrExp(loc, lhs, rhs);
        case clang::BO_Xor: return new XorExp(loc, lhs, rhs);

        case clang::BO_LAnd: return new AndAndExp(loc, lhs, rhs);
        case clang::BO_LOr: return new OrOrExp(loc, lhs, rhs);

        case clang::BO_Shl: return new ShlExp(loc, lhs, rhs);
        case clang::BO_Shr: return new ShrExp(loc, lhs, rhs);

        case clang::BO_LT: return new CmpExp(TOKlt, loc, lhs, rhs);
        case clang::BO_GT: return new CmpExp(TOKgt, loc, lhs, rhs);
        case clang::BO_LE: return new CmpExp(TOKle, loc, lhs, rhs);
        case clang::BO_GE: return new CmpExp(TOKge, loc, lhs, rhs);

        case clang::BO_EQ: return new EqualExp(TOKequal, loc, lhs, rhs);
        case clang::BO_NE: return new EqualExp(TOKnotequal, loc, lhs, rhs);
    }

    llvm::llvm_unreachable_internal("Unhandled C++ binary operation exp");
}

Expression* ExprMapper::fromExpression(const clang::Expr* E, Type *t)
{
    auto loc = fromLoc(E->getLocStart());

    if (auto PE = dyn_cast<clang::ParenExpr>(E))
        return fromExpression(PE->getSubExpr());
    else if (auto ICE = dyn_cast<clang::CastExpr>(E))
        return fromExpression(ICE->getSubExpr(),
                            tymap.fromType(ICE->getType()));

    if (auto UO = dyn_cast<clang::UnaryOperator>(E))
        return fromUnaExp(UO);
    else if (auto BO = dyn_cast<clang::BinaryOperator>(E))
        return fromBinExp(BO);
    else if (auto CO = dyn_cast<clang::ConditionalOperator>(E))
    {
        auto econd = fromExpression(CO->getCond());
        auto e1 = fromExpression(CO->getTrueExpr());
        auto e2 = fromExpression(CO->getFalseExpr());
        
        return new CondExp(loc, econd, e1, e2);
    }

    if (auto IL = dyn_cast<clang::IntegerLiteral>(E))
    {
        auto Val = IL->getValue();

        if (!t)
            t = getAPIntDType(Val);

        return new IntegerExp(loc,
                Val.isNegative() ? Val.getSExtValue() : Val.getZExtValue(),
                getAPIntDType(Val));
    }
    else if (auto CL = dyn_cast<clang::CharacterLiteral>(E))
    {
        auto Char = IL->getValue().getZExtValue();

        switch (CL->getKind())
        {
            case clang::CharacterLiteral::Ascii:
                return new IntegerExp(loc, (d_uns8)Char, Type::tchar);
            case clang::CharacterLiteral::Wide:
            case clang::CharacterLiteral::UTF16:
                return new IntegerExp(loc, (d_uns16)Char, Type::twchar);
            case clang::CharacterLiteral::UTF32:
                return new IntegerExp(loc, (d_uns32)Char, Type::tdchar);
        }
    }
    else if (auto BL = dyn_cast<clang::CXXBoolLiteralExpr>(E))
    {
        return new IntegerExp(loc, BL->getValue() ? 1 : 0, Type::tbool);
    }
    else if (auto FL = dyn_cast<clang::FloatingLiteral>(E))
    {
        // hmm APFloat have no precision info, so use double?
        auto FVal = FL->getValue();
        return new RealExp(loc, FVal.convertToDouble(), Type::tfloat64);
    }
    else if (auto SL = dyn_cast<clang::StringLiteral>(E))
    {
        utf8_t postfix = 0;
        if (SL->getCharByteWidth() == 2)
            postfix = 'w';
        else if (SL->getCharByteWidth() == 4)
            postfix = 'd';
        else assert(SL->getCharByteWidth() == 0);

        return new StringExp(loc, const_cast<char*>(SL->getString().data()),
                             SL->getLength(), postfix);
    }
    else if (auto NPL = dyn_cast<clang::CXXNullPtrLiteralExpr>(E))
    {
        return new NullExp(loc);
    }
    else if (auto TT = dyn_cast<clang::TypeTraitExpr>(E))
    {
        if (!TT->isValueDependent())
            return new IntegerExp(loc, TT->getValue() ? 1 : 0, Type::tbool);
        else
            return new NullExp(loc);  // TODO replace by D traits
    }
    else if (auto UEOTT = dyn_cast<clang::UnaryExprOrTypeTraitExpr>(E))
    {
        auto t = tymap.fromType(UEOTT->getTypeOfArgument());
        auto e1 = new TypeExp(loc, t);

        switch (UEOTT->getKind())
        {
            case clang::UETT_SizeOf:
                return new DotIdExp(loc, e1, Id::__sizeof);
            case clang::UETT_AlignOf:
                return new DotIdExp(loc, e1, Id::__xalignof);
            default:
                assert(false && "Unsupported");
        }
    }
    else if (auto DR = dyn_cast<clang::DeclRefExpr>(E))
    {
        return fromExpressionDeclRef(loc,
                            const_cast<clang::ValueDecl*>(DR->getDecl()));
    }
    else if (auto SNTTP = dyn_cast<clang::SubstNonTypeTemplateParmExpr>(E))
    {
        return fromExpressionNonTypeTemplateParm(loc,
                            SNTTP->getParameter());
    }
    else if (auto DSDR = dyn_cast<clang::DependentScopeDeclRefExpr>(E))
    {
        Expression *e1 = nullptr;
        Identifier *ident;
        TemplateInstance *tempinst = nullptr;

        if (auto NNS = DSDR->getQualifier())
        {
            auto tqual = tymap.fromNestedNameSpecifier(NNS);
            e1 = new TypeExp(loc, tqual);
        }

        if (DSDR->getDeclName().isIdentifier())
            ident = fromIdentifier(DSDR->getDeclName().getAsIdentifierInfo());
        else
            assert(false && "Unhandled DeclarationName kind");

        if (DSDR->hasExplicitTemplateArgs())
        {
            auto tiargs = fromASTTemplateArgumentListInfo(
                              DSDR->getExplicitTemplateArgs(), tymap);

            tempinst = new ::TemplateInstance(loc, ident);
            tempinst->tiargs = tiargs;
        }

        if (e1)
        {
            if (tempinst)
                return new DotTemplateInstanceExp(loc, e1, tempinst);
            else
                return new DotIdExp(loc, e1, ident);
        }
        else
        {
            if (tempinst)
                return new TypeExp(loc, new TypeInstance(loc, tempinst));
            else
                return new IdentifierExp(loc, ident);
        }
    }

    if (isa<clang::CallExpr>(E)) // TODO implement evaluation
        return nullptr;

    llvm::llvm_unreachable_internal("Unhandled C++ expression");
}

Type *getAPIntDType(const llvm::APInt &i)
{
    bool needs64bits = i.getBitWidth() > 32;

    if (i.isNegative())
        return needs64bits ? Type::tint64 : Type::tint32;
    else
        return needs64bits ? Type::tuns64 : Type::tuns32;
}

Expression* ExprMapper::fromAPInt(const APInt& Val)
{
    return new IntegerExp(Loc(),
            Val.isNegative() ? Val.getSExtValue() : Val.getZExtValue(),
            getAPIntDType(Val));
}

Expression* ExprMapper::fromExpressionDeclRef(Loc loc, clang::NamedDecl *D)
{
    if (auto NTTP = dyn_cast<clang::NonTypeTemplateParmDecl>(D))
        return fromExpressionNonTypeTemplateParm(loc, NTTP);

    clang::NamedDecl *Parent = nullptr;

    clang::DeclContext *DCParent = D->getDeclContext();
    while (!isa<clang::NamedDecl>(DCParent))
        DCParent = DCParent->getParent();

    Parent = cast<clang::NamedDecl>(DCParent);
    auto t = tymap.typeQualifiedFor(Parent);

    auto ident = getIdentifier(D);
    if (t)
    {
        auto e1 = new TypeExp(loc, t);
        return new DotIdExp(loc, e1, ident);
    }
    else
        return new IdentifierExp(loc, ident);
}

Expression* ExprMapper::fromExpressionNonTypeTemplateParm(Loc loc, const clang::NonTypeTemplateParmDecl* D)
{
    auto ident = DeclMapper::getIdentifierForTemplateNonTypeParm(D);
    return new IdentifierExp(loc, ident);
}

}
