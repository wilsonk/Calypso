// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPEXPRESSION_H
#define DMD_CPP_CPPEXPRESSION_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "../expression.h"
#include "cppdeclaration.h"

namespace clang
{
class Expr;
class CXXOperatorCallExpr;
}

namespace cpp
{

// Common interface to retrieve clang::Expr 
struct Expression
{
    virtual clang::Expr *toClang() = 0;
};

// DMD rewrites a() to a.opCall() during semantic() if a is an aggregate
// C++'s operators not being reserved function names, they need to be translated into OperatorCallExp instead
struct OperatorCallExp : ::CallExp
{
    const clang::CXXOperatorCallExpr *OCE;

    OperatorCallExp(Loc loc, const clang::CXXOperatorCallExpr *OCE);
    
    elem *toElem(IRState *irs);
};

}

#endif /* DMD_CPP_CPPEXPRESSION_H */
