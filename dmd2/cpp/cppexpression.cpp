// Contributed by Elie Morisse, same license DMD uses

#include "calypso.h"
#include "cppexpression.h"

#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/Scope.h"

namespace cpp
{

// Expression *DotVarExp::semantic(Scope *sc)
// {
//     clang::Scope *S;
//     clang::CXXScopeSpec SS;
//
//     clang::Expr *LHS =
//
//     calypso.TheSema->ActOnMemberAccessExpr(getCurScope(), LHS.take(), OpLoc,
//                         OpKind, SS, TemplateKWLoc, Name,
//                                  CurParsedObjCImpl ? CurParsedObjCImpl->Dcl : 0,
//                                             Tok.is(tok::l_paren));
// }

}
