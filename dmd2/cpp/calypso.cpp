// Contributed by Elie Morisse, same license DMD uses

#include "cpp/astunit.h"
#include "cpp/calypso.h"
#include "cpp/cppimport.h"
#include "cpp/cppmodule.h"
#include "cpp/cppaggregate.h"
#include "cpp/cpptypes.h"

#include "aggregate.h"
#include "declaration.h"
#include "id.h"
#include "lexer.h"
#include "expression.h"

#include "../../driver/tool.h"
#include "../../driver/cl_options.h"

#include "clang/AST/DeclTemplate.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "llvm/Support/Program.h"
#include "llvm/IR/LLVMContext.h"

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

LangPlugin calypso;
BuiltinTypes builtinTypes;

static inline ASTUnit* ast() { return calypso.pch.AST; }

Identifier *fromIdentifier(const clang::IdentifierInfo *II)
{
    return Lexer::idPool(II->getNameStart());
        // NOTE: Every C++ identifier passing through DMD gets its own redundant copy in memory
        // Is this the cost of interfacing with Clang or is there another way? (probably not an easy one)
}

static const char *getOperatorName(const clang::OverloadedOperatorKind OO)
{
    switch (OO)
    {
#   define OVERLOADED_OPERATOR(Name,Spelling,Token,Unary,Binary,MemberOnly) \
        case clang::OO_##Name: return #Name;
#   include "clang/Basic/OperatorKinds.def"
        default: return "None";
    }
}

static const char *getDOperatorSpelling(const clang::OverloadedOperatorKind OO)
{
    switch (OO)
    {
        case clang::OO_PlusEqual: return "+";
        case clang::OO_MinusEqual: return "-";
        case clang::OO_StarEqual: return "*";
        case clang::OO_SlashEqual: return "/";
        case clang::OO_PercentEqual: return "%";
        case clang::OO_CaretEqual: return "^";
        case clang::OO_AmpEqual: return "&";
        case clang::OO_PipeEqual: return "|";
        case clang::OO_LessLessEqual: return "<<";
        case clang::OO_GreaterGreaterEqual: return ">>";
        default:
            return clang::getOperatorSpelling(OO);
    }
}

static Identifier *getOperatorIdentifier(const clang::FunctionDecl *FD,
                                                    const char *&op)
{
    auto& Context = calypso.getASTContext();
    auto OO = FD->getOverloadedOperator();

    Identifier *opIdent;
    bool wrapInTemp = false;

    auto MD = dyn_cast<clang::CXXMethodDecl>(FD);
    bool isNonMember = !MD || MD->isStatic();

    auto NumParams = FD->getNumParams();
    if (!isNonMember)
        NumParams++;

    if (OO == clang::OO_Call)
        opIdent = Id::call;
    else if(OO == clang::OO_Subscript)
        opIdent = Id::index;
    else
    {
        bool isUnary = NumParams == 1;
        bool isBinary = NumParams == 2;

        wrapInTemp = true; // except for opAssign

        if (isUnary)
        {
            switch (OO)
            {
                case clang::OO_Plus:
                case clang::OO_Minus:
                case clang::OO_Star:
                case clang::OO_Tilde:
                case clang::OO_PlusPlus:
                case clang::OO_MinusMinus:
                    opIdent = Id::opUnary;
                    break;
                default:
    //                     ::warning(loc, "Ignoring C++ unary operator%s", clang::getOperatorSpelling(OO));
                    return nullptr;
            }
        }
        else if (isBinary)
        {
            switch (OO)
            {
                case clang::OO_Plus:
                case clang::OO_Minus:
                case clang::OO_Star:
                case clang::OO_Slash:
                case clang::OO_Percent:
                case clang::OO_Caret:
                case clang::OO_Amp:
                case clang::OO_Pipe:
                case clang::OO_Tilde:
                case clang::OO_LessLess:
                case clang::OO_GreaterGreater:
                    opIdent = Id::opBinary;
                    break;
                case clang::OO_Equal:
                    // D doesn't allow overloading of identity assignment, and since it might still be fundamental
                    // for some types (e.g std::map), map it to another method.
                    // NOTE: C++ assignment operators can't be non-members.
                {
                    bool isIdentityAssign = false;

                    if (auto RHSLValue = dyn_cast<clang::LValueReferenceType>(
                                    MD->getParamDecl(0)->getType().getDesugaredType(Context).getTypePtr()))
                    {
                        auto LHSType = Context.getTypeDeclType(MD->getParent());
                        auto RHSType = RHSLValue->getPointeeType().withoutLocalFastQualifiers();

                        if (LHSType.getCanonicalType() == RHSType.getCanonicalType())
                            isIdentityAssign = true;

                        // If RHS is a template specialization with dependent args there's a chance one specialization
                        // of the templated overloaded operator ends up being the identity assignment
                        auto LHSTempSpec = dyn_cast<clang::ClassTemplateSpecializationDecl>(MD->getParent());
                        auto RHSTempSpecTy = RHSType->getAs<clang::TemplateSpecializationType>();

                        if (LHSTempSpec && RHSTempSpecTy)
                        {
                            auto LHSTemp = LHSTempSpec->getSpecializedTemplate();
                            auto RHSTemp = RHSTempSpecTy->getTemplateName().getAsTemplateDecl();

                            if (RHSTemp && LHSTemp->getCanonicalDecl() == RHSTemp->getCanonicalDecl())
                                isIdentityAssign = true;
                        }
                    }

                    if (isIdentityAssign)
                        opIdent = Lexer::idPool("__opAssign");
                    else
                        opIdent = Id::assign;

                    wrapInTemp = false;
                    break;
                }
                case clang::OO_PlusEqual:
                case clang::OO_MinusEqual:
                case clang::OO_StarEqual:
                case clang::OO_SlashEqual:
                case clang::OO_PercentEqual:
                case clang::OO_CaretEqual:
                case clang::OO_AmpEqual:
                case clang::OO_PipeEqual:
                case clang::OO_LessLessEqual:
                case clang::OO_GreaterGreaterEqual:
                    opIdent = Id::opOpAssign;
                    break;
                default:
    //                     ::warning(loc, "Ignoring C++ binary operator%s", clang::getOperatorSpelling(OO));
                    return nullptr;
            }
        }
        else
            return nullptr; // operator new or delete
    }

    op = wrapInTemp ? getDOperatorSpelling(OO) : nullptr;
    return opIdent;
}

Identifier *getIdentifierOrNull(const clang::NamedDecl *D, const char **op)
{
    if (auto FTD = dyn_cast<clang::FunctionTemplateDecl>(D))
        D = FTD->getTemplatedDecl(); // same ident, can dyn_cast

    if (isa<clang::CXXConstructorDecl>(D))
        return Id::ctor;
    else if (isa<clang::CXXDestructorDecl>(D))
        return Id::dtor;
    else if (auto FD = dyn_cast<clang::FunctionDecl>(D))
        if (FD->isOverloadedOperator())
        {
            assert(op);
            return getOperatorIdentifier(FD, *op);
        }

    clang::IdentifierInfo *II = nullptr;

    if (D->getIdentifier())
        II = D->getIdentifier();
    else if (auto Tag = llvm::dyn_cast<clang::TagDecl>(D))
        if (auto Typedef = Tag->getTypedefNameForAnonDecl())
            II = Typedef->getIdentifier();

    return II ? fromIdentifier(II) : nullptr;
}

Identifier *getIdentifier(const clang::NamedDecl *D, const char **op)
{
    auto result = getIdentifierOrNull(D, op);
    assert(result);

    return result;
}

Identifier *getExtendedIdentifier(const clang::NamedDecl *D)
{
    const char *op;
    auto ident = getIdentifier(D, &op);

    auto FD = dyn_cast<clang::FunctionDecl>(D);
    if (op && FD)
    {
        auto OO = FD->getOverloadedOperator();
        std::string fullName(ident->string, ident->len);
        fullName += "_";
        fullName += getOperatorName(OO);
        ident = Lexer::idPool(fullName.c_str());
    }

    return ident;
}

Loc fromLoc(clang::SourceLocation L)
{    Loc loc;

    clang::StringRef S(ast()->getSourceManager().getFilename(L));
    loc.filename.data = S.data();
    loc.filename.size = S.size();
    loc.linnum = ast()->getSourceManager().getSpellingLineNumber(L);

    return loc;
}

#define MAX_FILENAME_SIZE 4096

#define CACHE_SUFFIXED_FILENAME(fn_var, suffix) \
    std::string fn_var(calypso.cachePrefix); \
    fn_var.append(suffix);

void PCH::init()
{
//     CACHE_SUFFIXED_FILENAME(headerList, ".list");
    auto& headerList = calypso.cachePrefix;

    auto fheaderList = fopen(headerList, "r"); // ordered list of headers
        // currently cached as one big PCH (neither modules nor chained PCH can be
        // used without modifying Clang).
    if (!fheaderList)
        return;

    char linebuf[MAX_FILENAME_SIZE];
    while (fgets(linebuf, MAX_FILENAME_SIZE, fheaderList) != NULL)
    {
        linebuf[strcspn(linebuf, "\n")] = '\0';
        if (linebuf[0] == '\0')
            continue;

        headers.push(strdup(linebuf));
    }

    fclose(fheaderList);
}

void PCH::add(StringRef header)
{
    bool found = false;

    for (unsigned i = 0; i < headers.dim; i++)
    {
        if (strncmp(header.data, headers[i], header.size) == 0)
        {
            found = true;
            break;
        }
    }

    if (found)
        return;

    headers.push(header);
    needEmit = true;
}

void PCH::update()
{
    auto& cachePrefix = calypso.cachePrefix;

    if (headers.empty())
        return;

    if (!needEmit && AST)
        return;

    // FIXME
    assert(!(needEmit && AST) && "Need AST merging FIXME");

#define ADD_SUFFIX_THEN_CHECK(fn_var, suffix) \
    CACHE_SUFFIXED_FILENAME(fn_var, suffix); \
    { \
        using namespace llvm::sys::fs; \
        file_status result; \
        status(fn_var, result); \
        if (is_directory(result)) \
        { \
            ::error(Loc(), "%s is a directory\n", fn_var.c_str()); \
            fatal(); \
        } \
        else if (!exists(result)) \
            needEmit = true; \
    }
    // NOTE: there's File::exists but it is incomplete and unused, hence llvm::sys::fs

    ADD_SUFFIX_THEN_CHECK(pchHeader, ".h");
    ADD_SUFFIX_THEN_CHECK(pchFilename, ".h.pch");
#undef ADD_SUFFIX_THEN_CHECK

    if (needEmit)
    {
//         const char *compiler = getenv("CC");
//         if (!strlen(compiler))
//             compiler = "clang";

        auto compiler = llvm::sys::findProgramByName("clang");
        if (!compiler)
        {
            ::error(Loc(), "Clang compiler not found");
            fatal();
        }

        /* PCH generation */

        // Re-emit the source file with #include directives
        auto fmono = fopen(pchHeader.c_str(), "w");
        if (!fmono)
        {
            ::error(Loc(), "C++ monolithic header couldn't be created");
            fatal();
        }

        for (unsigned i = 0; i < headers.dim; ++i)
        {
            if (headers[i][0] == '<')
                fprintf(fmono, "#include %s\n", headers[i]);
            else
                fprintf(fmono, "#include \"%s\"\n", headers[i]);
        }

        fclose(fmono);

        // Compiler flags
        std::vector<std::string> Argv;
        Argv.reserve(opts::cppArgs.size() + 7);
        unsigned i = 0;
#define ARGV_ADD(a) { Argv.emplace_back(a); i++; }
        for (unsigned j = 0; j < opts::cppArgs.size(); ++j)
            ARGV_ADD(opts::cppArgs[j]);
        ARGV_ADD("-x");
        ARGV_ADD("c++-header");
        ARGV_ADD("-Xclang");
        ARGV_ADD("-emit-pch");
        ARGV_ADD("-o");
        ARGV_ADD(pchFilename);
        ARGV_ADD(pchHeader);
#undef ARGV_ADD

        if (executeToolAndWait(*compiler, Argv,
                global.params.verbose) == -1)
        {
            ::error(Loc(), "execv Error!");
            fatal();
        }

        /* Update the list of headers */

        auto fheaderlist = fopen(cachePrefix, "w");
        if (fheaderlist == NULL)
        {
            ::error(Loc(), "C/C++ header list cache file couldn't be opened/created");
            fatal();
        }

        for (unsigned i = 0; i < headers.dim; ++i)
            fprintf(fheaderlist, "%s\n", headers[i]);

        fclose(fheaderlist);
    }

    needEmit = false;

    /* PCH was generated successfully, let's load it */

    clang::FileSystemOptions FileSystemOpts;

    clang::IntrusiveRefCntPtr<clang::DiagnosticOptions> DiagOpts(new clang::DiagnosticOptions);
    auto DiagClient = new clang::TextDiagnosticPrinter(llvm::errs(), &*DiagOpts);
    clang::IntrusiveRefCntPtr<clang::DiagnosticIDs> DiagID(new clang::DiagnosticIDs);
    Diags = new clang::DiagnosticsEngine(DiagID,
                                         &*DiagOpts, DiagClient);

    AST = ASTUnit::LoadFromASTFile(pchFilename,
                                Diags, FileSystemOpts, &instCollector);

    // Build the builtin type map
    calypso.builtinTypes.build(AST->getASTContext());
}

#undef CACHE_SUFFIXED_FILENAME
#undef MAX_FILENAME_SIZE

int LangPlugin::doesHandleImport(const utf8_t* tree)
{
    if (strcmp((const char *) tree, "C") == 0
        || strcmp((const char *) tree, "C++") == 0)
        return true;

    return false;
}

::Import* LangPlugin::createImport(int treeId, Loc loc, Identifiers* packages,
                                   Identifier* id, Identifier* aliasId, int isstatic)
{
    return new Import(loc,
                packages, id, aliasId, isstatic, false);
}

int LangPlugin::doesHandleModmap(const utf8_t* lang)
{
    if (strcmp((const char *) lang, "C") == 0
        || strcmp((const char *) lang, "C++") == 0)
        return true;

    return false;
}

::Modmap* LangPlugin::createModmap(int langId, Loc loc, Expression* arg)
{
    return new Modmap(loc,
                static_cast<StringExp*>(arg));
}

LangPlugin::LangPlugin()
    : builtinTypes(cpp::builtinTypes)
{

}

void LangPlugin::init()
{
    Module::init();
    pch.init();
}

clang::ASTContext& LangPlugin::getASTContext()
{
    return getASTUnit()->getASTContext();
}

bool isCPP(Type* t) { return t->langPlugin() == &calypso; }
bool isCPP(Dsymbol* s) { return s->langPlugin() == &calypso; }

cpp::ClassDeclaration *isDCXX(Dsymbol* s)
{
    auto cd = s->isClassDeclaration();
    assert(cd);

    if (isCPP(cd))
        return nullptr;  // Pure C++ class

    auto base = cd->baseClass;
    while (base && !isCPP(base))
        base = toAggregateBase(base);
    if (!base)
        return nullptr;  // Pure D class

    return static_cast<cpp::ClassDeclaration*>(base);
}

}
