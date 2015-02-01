// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPEXPRESSION_H
#define DMD_CPP_CPPEXPRESSION_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root.h"
#include "expression.h"
#include "cpp/calypso.h"
#include "cpp/cpptypes.h"

// NOTE: We're focusing on literal expressions, since expression conversion
// is needed for default function and template args.

namespace llvm
{
class APInt;
}

namespace clang
{
class Expr;
}

namespace cpp
{

class ExprMapper
{
public:
    ExprMapper(TypeMapper &tymap)
        : tymap(tymap) {}

    bool enableCallExpr = false; // temporary

    // Clang -> DMD
    Expression *fromExpression(const clang::Expr *E, Type *t = nullptr, bool interpret = false);

    Expression *fromExpressionDeclRef(Loc loc, clang::NamedDecl* D);
    Expression *fromExpressionNonTypeTemplateParm(Loc loc, const clang::NonTypeTemplateParmDecl* D);

    static Expression *fromAPInt(const llvm::APInt &Val);

    // DMD -> Clang
    clang::Expr *toExpression(Expression *e);

protected:
    TypeMapper &tymap;

    Expression* fromUnaExp(const clang::UnaryOperator *E);
    Expression* fromBinExp(const clang::BinaryOperator* E);
};

}

#endif