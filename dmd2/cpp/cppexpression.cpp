// Contributed by Elie Morisse, same license DMD uses

#include "cpp/cppexpression.h"
#include "cpp/cpptypes.h"

#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

static Type *getAPIntDType(const llvm::APInt &i);

Expression* toUnaExp(const clang::UnaryOperator *E)
{
    auto loc = toLoc(E->getLocStart());
    auto sub = toExpression(E->getSubExpr());
    switch (E->getOpcode())
    {
        case clang::UO_Plus: return sub;
        case clang::UO_Minus: return new NegExp(loc, sub);
        case clang::UO_Not: return new NotExp(loc, sub);
        case clang::UO_LNot: return new NotExp(loc, sub);
    }

    llvm::llvm_unreachable_internal("Unhandled C++ unary operation exp");
}

Expression* toBinExp(const clang::BinaryOperator* E)
{
    auto loc = toLoc(E->getLocStart());
    auto lhs = toExpression(E->getLHS());
    auto rhs = toExpression(E->getRHS());

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

Expression* toExpression(const clang::Expr* E, Type *t)
{
    TypeMapper tymap;
    auto loc = toLoc(E->getLocStart());

    if (auto PE = dyn_cast<clang::ParenExpr>(E))
        return toExpression(PE->getSubExpr());
    else if (auto ICE = dyn_cast<clang::CastExpr>(E))
        return toExpression(ICE->getSubExpr(),
                            tymap.toType(ICE->getType()));

    if (auto UO = dyn_cast<clang::UnaryOperator>(E))
        return toUnaExp(UO);
    else if (auto BO = dyn_cast<clang::BinaryOperator>(E))
        return toBinExp(BO);

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
    else if (auto DR = dyn_cast<clang::DeclRefExpr>(E)) // FIXME? this was added for NonTypeTemplateParm, but that's probably not the only use case
    {
        auto ident = getIdentifier(DR->getDecl());
        return new IdentifierExp(loc, ident);
    }

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

Expression* APIntToExpression(const APInt& Val)
{
    return new IntegerExp(Loc(),
            Val.isNegative() ? Val.getSExtValue() : Val.getZExtValue(),
            getAPIntDType(Val));
}

}
