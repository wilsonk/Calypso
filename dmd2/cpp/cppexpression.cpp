// Contributed by Elie Morisse, same license DMD uses

#include "cpp/astunit.h"
#include "cpp/cppexpression.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cpptemplate.h"
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

Expression *dotIdentOrInst(Loc loc, Expression *e1, RootObject *o)
{
    if (o->dyncast() == DYNCAST_IDENTIFIER)
        return new DotIdExp(loc, e1,
                        static_cast<Identifier*>(o));
    else
        return new DotTemplateInstanceExp(loc, e1,
                        static_cast<TemplateInstance*>(o));
}

static RootObject *typeQualifierRoot(TypeQualified *tqual)
{
    if (tqual->ty == Tident)
        return static_cast<TypeIdentifier*>(tqual)->ident;
    else if (tqual->ty == Tinstance)
        return static_cast<TypeInstance*>(tqual)->tempinst;

    llvm_unreachable("FIXME TypeOf");
}

Objects *fromASTTemplateArgumentListInfo(
            const clang::ASTTemplateArgumentListInfo &Args,
            TypeMapper &tymap)
{
    auto tiargs = new Objects;

    for (unsigned i = 0; i < Args.NumTemplateArgs; i++)
    {
        auto Arg = &Args[i].getArgument();
        tiargs->push(TypeMapper::FromType(tymap).fromTemplateArgument(Arg));
    }

    return tiargs;
}

// In Clang AST enum values in template arguments are resolved to integer literals
// If the parameter has an enum type, we need to revert integer literals to DeclRefs pointing to enum constants
// or else DMD won't find the template decl since from its point of view uint != Enum
// Also useful for function default zero arguments.
Expression *ExprMapper::fixIntegerExp(IntegerExp *e, clang::QualType T)
{
    auto ET = dyn_cast<clang::EnumType>(T);
    if (!ET)
        return e;

    for (auto ECD: ET->getDecl()->enumerators())
    {
        auto Val = ECD->getInitVal().getZExtValue();

        if (Val == ((IntegerExp *)e)->getInteger())
            return fromExpressionDeclRef(Loc(), ECD);
    }

    llvm_unreachable("Couldn't find the corresponding enum constant");
}

Expression *ExprMapper::fromUnaExp(clang::SourceLocation Loc,
                                   const clang::UnaryOperator::Opcode Op,
                                   const clang::Expr *SubExpr)
{
    auto loc = fromLoc(Loc);
    auto sub = fromExpression(SubExpr);

    if (!sub)
        return nullptr;

    switch (Op)
    {
        case clang::UO_Plus: return sub;
        case clang::UO_Minus: return new NegExp(loc, sub);
        case clang::UO_Not: return new NotExp(loc, sub);
        case clang::UO_LNot: return new NotExp(loc, sub);
        case clang::UO_AddrOf: return new AddrExp(loc, sub);
        case clang::UO_Deref: return new PtrExp(loc, sub);
    }

    llvm::llvm_unreachable_internal("Unhandled C++ unary operation exp");
}

Expression* ExprMapper::fromUnaExp(const clang::UnaryOperator *E)
{
    return fromUnaExp(E->getLocStart(), E->getOpcode(), E->getSubExpr());
}

Expression *ExprMapper::fromBinExp(clang::SourceLocation Loc,
                                const clang::BinaryOperator::Opcode Op,
                                const clang::Expr *LHS, const clang::Expr *RHS)
{
    auto loc = fromLoc(Loc);
    auto lhs = fromExpression(LHS);
    auto rhs = fromExpression(RHS);

    if (!lhs || !rhs)
        return nullptr;

    switch (Op)
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

        case clang::BO_Assign: return new AssignExp(loc, lhs, rhs);
        case clang::BO_MulAssign: return new MulAssignExp(loc, lhs, rhs);
        case clang::BO_AddAssign: return new AddAssignExp(loc, lhs, rhs);
        case clang::BO_RemAssign: return new MinAssignExp(loc, lhs, rhs);
        case clang::BO_DivAssign: return new DivAssignExp(loc, lhs, rhs);

        case clang::BO_Comma: return new CommaExp(loc, lhs, rhs);
    }

    llvm::llvm_unreachable_internal("Unhandled C++ binary operation exp");
}

Expression* ExprMapper::fromBinExp(const clang::BinaryOperator* E)
{
    return fromBinExp(E->getLocStart(), E->getOpcode(),
                    E->getLHS(), E->getRHS());
}

Expression* ExprMapper::fromExpression(const clang::Expr *E, clang::QualType DestTy,
                                            bool interpret)  // TODO implement interpret properly
{
    auto& Context = calypso.pch.AST->getASTContext();
    auto loc = fromLoc(E->getLocStart());

    Expression *e = nullptr;
    Type *t = nullptr;
    clang::QualType Ty;

    Type *destType = nullptr;
    if (!DestTy.isNull())
        destType = tymap.fromType(DestTy);

    if (auto Cast = dyn_cast<clang::CastExpr>(E))
    {
        clang::QualType CastDestTy;
        auto Kind = Cast->getCastKind();

        if (Kind != clang::CK_NoOp && Kind != clang::CK_ConstructorConversion &&
                Kind != clang::CK_LValueToRValue)
        {
            CastDestTy = Cast->getType();
            assert(Cast->getSubExpr()->getType().getCanonicalType()
                            != CastDestTy.getCanonicalType()); // we should be ignoring all casts that do not alter the type
        }

        if (Kind == clang::CK_NullToPointer)
            e = new NullExp(loc);
        else
            e = fromExpression(Cast->getSubExpr(), CastDestTy);
    }

    else if (auto PE = dyn_cast<clang::ParenExpr>(E))
        e = fromExpression(PE->getSubExpr());
    else if (auto CDA = dyn_cast<clang::CXXDefaultArgExpr>(E))
        e = fromExpression(CDA->getExpr());

    else if (auto UO = dyn_cast<clang::UnaryOperator>(E))
        e = fromUnaExp(UO);
    else if (auto BO = dyn_cast<clang::BinaryOperator>(E))
        e = fromBinExp(BO);
    else if (auto CO = dyn_cast<clang::ConditionalOperator>(E))
    {
        auto econd = fromExpression(CO->getCond());
        auto e1 = fromExpression(CO->getTrueExpr());
        auto e2 = fromExpression(CO->getFalseExpr());

        if (!econd || !e1 || !e2)
            return nullptr;
        
        e = new CondExp(loc, econd, e1, e2);
    }

    else if (auto IL = dyn_cast<clang::IntegerLiteral>(E))
    {
        auto Val = IL->getValue();

        t = (!destType || !destType->isintegral()) ? getAPIntDType(Val) : destType;

        e = new IntegerExp(loc,
                Val.isNegative() ? Val.getSExtValue() : Val.getZExtValue(), t);
    }
    else if (auto CL = dyn_cast<clang::CharacterLiteral>(E))
    {
        auto Char = CL->getValue();

        switch (CL->getKind())
        {
            case clang::CharacterLiteral::Ascii:
                t = Type::tchar;
                e = new IntegerExp(loc, (d_uns8)Char, t);
                break;
            case clang::CharacterLiteral::Wide:
            case clang::CharacterLiteral::UTF16:
                t = Type::twchar;
                e = new IntegerExp(loc, (d_uns16)Char, t);
                break;
            case clang::CharacterLiteral::UTF32:
                t = Type::tdchar;
                e = new IntegerExp(loc, (d_uns32)Char, t);
                break;
        }
    }
    else if (auto BL = dyn_cast<clang::CXXBoolLiteralExpr>(E))
    {
        t = Type::tbool;
        e = new IntegerExp(loc, BL->getValue() ? 1 : 0, t);
    }
    else if (auto FL = dyn_cast<clang::FloatingLiteral>(E))
    {
        auto APFVal = FL->getValue();

        real_t val;
        t = Type::tfloat32;

        if (APFVal.isZero())
            val = 0.0;
        else if (&APFVal.getSemantics() == &llvm::APFloat::IEEEsingle)
            val = APFVal.convertToFloat();
        else if (&APFVal.getSemantics() == &llvm::APFloat::IEEEdouble)
        {
            val = APFVal.convertToDouble();
            t = Type::tfloat64;
        }
        else
        {
            ::warning(loc, "Floating point semantics for non-zero APFloat handled by converting to string and strtold");

            llvm::SmallString<16> Str;
            APFVal.toString(Str, 0, llvm::APFloat::semanticsPrecision(APFVal.getSemantics()));
            val = strtold(Str.c_str(), nullptr);
            t = Type::tfloat80;
        }

        e = new RealExp(loc, val, t);
    }
    else if (auto SL = dyn_cast<clang::StringLiteral>(E))
    {
        utf8_t postfix = 0;
        if (SL->getCharByteWidth() == 2)
        {
            postfix = 'w';
            t = new TypeDArray(Type::tdchar->immutableOf());
                // WARNING: string literals with postfixes need to be explicitly cast to pointer types (DMD BUG?)
                // which is being done at the ImplicitCastExpr level.
                // See https://issues.dlang.org/show_bug.cgi?id=6032
        }
        else if (SL->getCharByteWidth() == 4)
        {
            postfix = 'd';
            t = new TypeDArray(Type::twchar->immutableOf());
        }
        else assert(SL->getCharByteWidth() == 1);

        e = new StringExp(loc, const_cast<char*>(SL->getBytes().data()),
                             SL->getLength(), postfix);
    }
    else if (isa<clang::CXXNullPtrLiteralExpr>(E) || isa<clang::GNUNullExpr>(E))
    {
        return new NullExp(loc);
    }

    else if (auto TT = dyn_cast<clang::TypeTraitExpr>(E))
    {
        if (!TT->isValueDependent())
            e = new IntegerExp(loc, TT->getValue() ? 1 : 0, Type::tbool);
        else
            return new NullExp(loc);  // TODO replace by D traits
    }
    else if (auto SOP = dyn_cast<clang::SizeOfPackExpr>(E))
    {
        if (!SOP->isValueDependent())
            e = new IntegerExp(loc, SOP->getPackLength(), Type::tuns64);
        else
            return new NullExp(loc);  // TODO replace by D traits
    }
    else if (auto NE = dyn_cast<clang::CXXNoexceptExpr>(E))
    {
        if (!NE->isValueDependent())
            e = new IntegerExp(loc, NE->getValue() ? 1 : 0, Type::tbool);
        else
            return new NullExp(loc);
    }
    else if (auto UEOTT = dyn_cast<clang::UnaryExprOrTypeTraitExpr>(E))
    {
        auto t = tymap.fromType(UEOTT->getTypeOfArgument());
        auto e1 = new TypeExp(loc, t);

        switch (UEOTT->getKind())
        {
            case clang::UETT_SizeOf:
                e = new DotIdExp(loc, e1, Id::__sizeof);
                break;
            case clang::UETT_AlignOf:
                e = new DotIdExp(loc, e1, Id::__xalignof);
                break;
            default:
                assert(false && "Unsupported");
        }
    }

    else if (auto DR = dyn_cast<clang::DeclRefExpr>(E))
    {
        Ty = DR->getType();
        t = tymap.fromType(Ty);

        e = fromExpressionDeclRef(loc, const_cast<clang::ValueDecl*>(DR->getDecl()),
                        DR->getQualifier());
        // FIXME overloaded operators
    }

    else if (auto PE = dyn_cast<clang::PackExpansionExpr>(E))
    {
        e = fromExpression(PE->getPattern());
    }
    else if (auto SNTTP = dyn_cast<clang::SubstNonTypeTemplateParmExpr>(E))
    {
        if (!interpret || SNTTP->isValueDependent())
            e = fromExpressionNonTypeTemplateParm(loc,
                                SNTTP->getParameter());
        else
            e = fromExpression(SNTTP->getReplacement());
    }
    else if (auto CDSM = dyn_cast<clang::CXXDependentScopeMemberExpr>(E))
    {
        auto member = getIdentOrTempinst(loc, CDSM->getMember());
        auto e1 = fromExpression(CDSM->getBase());

        if (!e1 || !member)
            return nullptr;

        if (CDSM->hasExplicitTemplateArgs())
        {
            assert(member->dyncast() == DYNCAST_IDENTIFIER);

            auto tempinst = new TemplateInstance(loc,
                            static_cast<Identifier*>(member));
            tempinst->tiargs = fromASTTemplateArgumentListInfo(
                        CDSM->getExplicitTemplateArgs(), tymap);

            member = tempinst;
        }

        if (auto NNS = CDSM->getQualifier())
        {
            auto tqual = TypeMapper::FromType(tymap).fromNestedNameSpecifier(NNS);
            e1 = dotIdentOrInst(loc, e1, typeQualifierRoot(tqual));

            for (auto id: tqual->idents)
                e1 = dotIdentOrInst(loc, e1, id);
        }

        e = dotIdentOrInst(loc, e1, member);
    }
    else if (auto DSDR = dyn_cast<clang::DependentScopeDeclRefExpr>(E))
    {
        Expression *e1 = nullptr;
        Identifier *ident;
        ::TemplateInstance *tempinst = nullptr;

        if (auto NNS = DSDR->getQualifier())
        {
            auto tqual = TypeMapper::FromType(tymap).fromNestedNameSpecifier(NNS);
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
                e = new DotTemplateInstanceExp(loc, e1, tempinst);
            else
                e = new DotIdExp(loc, e1, ident);
        }
        else
        {
            if (tempinst)
                e = new TypeExp(loc, new TypeInstance(loc, tempinst));
            else
                e = new IdentifierExp(loc, ident);
        }
    }
    else if (auto C = dyn_cast<clang::CallExpr>(E))
    {
        auto OC = dyn_cast<clang::CXXOperatorCallExpr>(E);
        if (OC && E->isInstantiationDependent()) // in dependent contexts operator calls aren't resolved yet to UnaryOperator and BinaryOperator
        {
            auto OO = OC->getOperator();
            if (C->getNumArgs() == 2 && OO >= clang::OO_Plus && OO <= clang::OO_Arrow &&
                    OO != clang::OO_PlusPlus && OO != clang::OO_MinusMinus)
            {
                auto Op = clang::BinaryOperator::getOverloadedOpcode(OO);
                auto LHS = C->getArg(0), RHS = C->getArg(1);
                e = fromBinExp(E->getLocStart(), Op, LHS, RHS);
            }
            else if (C->getNumArgs() == 1 && OO < clang::OO_Call)
            {
                auto Op = clang::UnaryOperator::getOverloadedOpcode(OO, false); // WARNING: how to determine whether prefix or postfix?
                auto Sub = C->getArg(0);
                e = fromUnaExp(E->getLocStart(), Op, Sub);
            }
        }
        else
        {
            auto callee = fromExpression(C->getCallee());
            if (!callee)
                return nullptr; // FIXME temporary hack skipping overloaded operators

            auto args = new Expressions;
            for (auto Arg: C->arguments())
                args->push(fromExpression(Arg));

            e = new CallExp(loc, callee, args);
        }
    }
    else if (auto UL = dyn_cast<clang::UnresolvedLookupExpr>(E))
    {
        if (!UL->getName().isIdentifier())
        {
            ::warning(loc, "Unsupported DeclarationName for UnresolvedLookupExpr");
            return new NullExp(loc);
        }

        // FIXME NNS
        auto id = fromIdentifier(UL->getName().getAsIdentifierInfo());

        if (UL->hasExplicitTemplateArgs())
        {
            auto& ExplicitTempArgs = UL->getExplicitTemplateArgs();

            auto tempinst = new ::TemplateInstance(loc, id);
            tempinst->tiargs = fromASTTemplateArgumentListInfo(ExplicitTempArgs, tymap);
            e = new ScopeExp(loc, tempinst);
        }
        else
            e = new IdentifierExp(loc, id);
    }
    else if (isa<clang::CXXScalarValueInitExpr>(E))
    {
        if (E->getType()->getAs<clang::ReferenceType>())
            return new NullExp(loc);

        t = tymap.fromType(E->getType().withoutLocalFastQualifiers());
        e = new CallExp(loc, new TypeExp(loc, t));
    }
    else if (auto MT = dyn_cast<clang::MaterializeTemporaryExpr>(E))
    {
        auto Ty = E->getType();
        e = fromExpression(MT->GetTemporaryExpr());

        if (!e)
            return nullptr;

        if (Ty->getAs<clang::ReferenceType>() && e->op == TOKnull)
            return nullptr; // FIXME once we may finally pass rvalues to ref arguments
                    // for other types there are workarounds but for null class references
                    // I couldn't find any way to turn them into lvalues.

        if (!e->isLvalue())
        {
            if (Ty->getAs<clang::RecordType>())
            {
                assert(e->op == TOKcall);
                auto call = static_cast<CallExp*>(e);
                assert(call->e1->op == TOKtype);

                e = new NewExp(loc, nullptr, nullptr,
                               call->e1->type, call->arguments);
            }
            else
            {
                t = tymap.fromType(Ty.withoutLocalFastQualifiers());

                auto args = new Expressions;
                args->push(e);

                e = new NewExp(loc, nullptr, nullptr, t, args);
            }

            e = new PtrExp(loc, e);
        }
    }
    else if (auto CBT = dyn_cast<clang::CXXBindTemporaryExpr>(E))
    {
        e = fromExpression(CBT->getSubExpr());
    }
    else if (auto CCE = dyn_cast<clang::CXXConstructExpr>(E))
    {
        if (CCE->isElidable())
            e = fromExpression(CCE->getArg(0));
        else
        {
            t = tymap.fromType(E->getType().withoutLocalFastQualifiers());

            auto args = new Expressions;
            for (auto Arg: CCE->arguments())
                args->push(fromExpression(Arg));

            e = new CallExp(loc, new TypeExp(loc, t), args);
        }
    }
    else if (auto CNE = dyn_cast<clang::CXXNewExpr>(E))
    {
        auto Ty = CNE->getAllocatedType();
        t = tymap.fromType(Ty.withoutLocalFastQualifiers());

        Expressions *args = nullptr;
        auto Construct = CNE->getConstructExpr();
        if (Construct && !Construct->isElidable())
        {
            args = new Expressions;
            for (auto Arg: Construct->arguments())
                args->push(fromExpression(Arg));
        }

        e = new NewExp(loc, nullptr, nullptr, t, args);
    }

    else if (isa<clang::InitListExpr>(E)) // TODO
        return new NullExp(loc);

    else if (isa<clang::CXXUnresolvedConstructExpr>(E)
        || isa<clang::CXXPseudoDestructorExpr>(E))
        return new NullExp(loc) /* nullptr */;

    else
        llvm::llvm_unreachable_internal("Unhandled C++ expression");

    if (!e)
        return nullptr;

    // When t is an unresolved TypeQualified we may want to emulate
    // implicitConvTo with the Clang types instead of the D ones
    if (!Ty.isNull() && !DestTy.isNull())
    {
        if (auto DestRecordTy = DestTy->getAs<clang::RecordType>())
        {
            if (DestTy->getAs<clang::ReferenceType>())
                DestRecordTy = DestTy->getPointeeType()->getAs<clang::RecordType>();

            if (!DestRecordTy)
                goto Lcast;

            auto ExprRecord = Ty->castAs<clang::RecordType>()->getDecl();
            auto ExprCXXRecord = dyn_cast<clang::CXXRecordDecl>(ExprRecord);
            auto DestRecord = DestRecordTy->getDecl();
            auto DestCXXRecord = dyn_cast<clang::CXXRecordDecl>(DestRecord);

            if (!DestRecord)
                goto Lcast;

            if (DestRecord->getCanonicalDecl() == ExprRecord->getCanonicalDecl())
                return e;

            if (!DestCXXRecord || !ExprCXXRecord || !ExprCXXRecord->isDerivedFrom(DestCXXRecord))
                goto Lcast;

            return e;
        }
    }

    if (destType && destType->ty == Treference)
        destType = destType->nextOf();

    // D won't be as lenient as C++ is towards signed constants overflowing into negative values,
    // so even if Type::implicitConvTo matches we should still check the evaluated expression
    // (see _ISwgraph in wctype.h for an example of this special case)
    if (!E->isInstantiationDependent() && !DestTy.isNull() &&
                destType->isintegral() && !destType->isunsigned())
    {
        llvm::APSInt V;
        if (E->EvaluateAsInt(V, Context))
            if (V.isUnsigned() && V.getActiveBits() == Context.getIntWidth(DestTy))
                goto Lcast;
    }

    if (!t || !destType ||
            t->implicitConvTo(destType) >= MATCHconst)
        return e;

Lcast:
    return new CastExp(loc, e, destType);
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

Expression* ExprMapper::fromExpressionDeclRef(Loc loc, clang::NamedDecl *D,
                                    const clang::NestedNameSpecifier *NNS)
{
    if (auto NTTP = dyn_cast<clang::NonTypeTemplateParmDecl>(D))
        return fromExpressionNonTypeTemplateParm(loc, NTTP);

    // BUG FIXME: TypeExp is convenient but not enough because TypeQualified::resolve doesn't resolve function calls
    // For now we're ignoring overloaded operator calls
    if (auto FD = dyn_cast<clang::FunctionDecl>(D))
        if (FD->isOverloadedOperator())
            return nullptr;

    TypeQualified *prefix = nullptr;
    if (NNS)
        prefix = TypeMapper::FromType(tymap).fromNestedNameSpecifier(NNS);

    auto tqual = TypeMapper::FromType(tymap, prefix).typeQualifiedFor(D);
    assert(tqual && "DeclRefExpr decl without a DeclarationName");

    // TODO: Build a proper expression from the type (mostly for reflection and to mimic parse.c, since TypeExp seems to work too)
    return new TypeExp(loc, tqual);
}

Expression* ExprMapper::fromExpressionNonTypeTemplateParm(Loc loc, const clang::NonTypeTemplateParmDecl* D)
{
    auto ident = DeclMapper::getIdentifierForTemplateNonTypeParm(D);
    return new IdentifierExp(loc, ident);
}

clang::Expr* ExprMapper::toExpression(Expression* e)
{
    auto& Context = calypso.pch.AST->getASTContext();
    clang::SourceLocation Loc;

    switch (e->op)
    {
        case TOKint64:
        {
            auto exp = static_cast<IntegerExp*>(e);
            auto value = exp->getInteger();

            if (e->type->ty == Tbool)
                return new (Context) clang::CXXBoolLiteralExpr(value != 0,
                                                               Context.BoolTy, Loc);
            else
            {
                unsigned IntSize = Context.getTargetInfo().getIntWidth();
                return clang::IntegerLiteral::Create(Context,
                                                llvm::APInt(IntSize, value),
                                                Context.IntTy, Loc);
            }
        }
        case TOKfloat64:
        {
            auto exp = static_cast<RealExp *>(e);
            llvm::APFloat Val((double) exp->value);
            return clang::FloatingLiteral::Create(Context,
                                            Val, true,
                                            Context.FloatTy, Loc);
        }
        default:
            llvm::llvm_unreachable_internal("Unhandled D -> Clang expression conversion");
    }
}

}
