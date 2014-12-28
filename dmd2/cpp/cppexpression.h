// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPEXPRESSION_H
#define DMD_CPP_CPPEXPRESSION_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root.h"
#include "calypso.h"
#include "../expression.h"

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

    Expression *toExpression(const clang::Expr *E, Type *t = nullptr);

protected:
    TypeMapper &tymap;

    Expression* toUnaExp(const clang::UnaryOperator *E);
    Expression* toBinExp(const clang::BinaryOperator* E);
};

Expression *APIntToExpression(const llvm::APInt &Val);

}

#endif