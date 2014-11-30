// Contributed by Elie Morisse, same license DMD uses

#include "calypso.h"
#include "cppimport.h"
#include "cppmodule.h"

#include "../aggregate.h"
#include "../declaration.h"
#include "../lexer.h"
#include "../expression.h"

#include "../../driver/tool.h"
#include "../../driver/cl_options.h"

#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "llvm/Support/Program.h"
#include "llvm/IR/LLVMContext.h"

namespace cpp
{

LangPlugin calypso;

static inline clang::ASTUnit* ast() { return calypso.pch.AST; }

Identifier *toIdentifier(clang::IdentifierInfo *II)
{
    return Lexer::idPool(II->getNameStart());
        // NOTE: Every C++ identifier passing through DMD gets its own redundant copy in memory
        // Is this the cost of interfacing with Clang or is there another way? (probably not an easy one)
}

Loc toLoc(clang::SourceLocation L)
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
    CACHE_SUFFIXED_FILENAME(headerList, ".list");

    auto fheaderList = fopen(headerList.c_str(), "r"); // ordered list of headers
        // currently cached as one big PCH (neither modules nor chained PCH can be
        // used without modifying Clang).
    if (!fheaderList)
        return;
//     {
//         ::error(Loc(), "C/C++ header list cache file couldn't be opened/created");
//         fatal();
//     }

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

        auto compiler = llvm::sys::FindProgramByName("clang");

        /* PCH generation */

        // Re-emit the source file with #include directives
        auto fmono = fopen(pchHeader.c_str(), "w");
        if (!fmono)
        {
            ::error(Loc(), "C++ monolithic header couldn't be created");
            fatal();
        }

        for (unsigned i = 0; i < headers.dim; ++i)
            fprintf(fmono, "#include \"%s\"\n", headers[i]);

        fclose(fmono);

        // Compiler flags
        std::vector<std::string> Argv;
        Argv.reserve(opts::cppArgs.size() + 7);
        unsigned i = 0;
#define ARGV_ADD(a) { Argv.emplace_back(a); i++; }
        ARGV_ADD("-cc1");
        for (unsigned j = 0; j < opts::cppArgs.size(); ++j)
            ARGV_ADD(opts::cppArgs[j]);
        ARGV_ADD("-x");
        ARGV_ADD("c++-header");
        ARGV_ADD("-emit-pch");
        ARGV_ADD("-o");
        ARGV_ADD(pchFilename);
        ARGV_ADD(pchHeader);
#undef ARGV_ADD

        if (executeToolAndWait(compiler, Argv,
                global.params.verbose) == -1) // NOTE: I also have a compiler-agnostic version in my backups with fork(), which was deemed useless since Calypso is tied to LDC and LLVM
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

    AST = clang::ASTUnit::LoadFromASTFile(pchFilename,
                                          Diags, FileSystemOpts);

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
                packages, id, aliasId, isstatic);
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

void LangPlugin::init()
{
    Module::init();
    pch.init();
}

clang::ASTContext& LangPlugin::getASTContext()
{
    return getASTUnit()->getASTContext();
}

bool isCPP(Dsymbol* s)
{
    ::LangPlugin *lp = nullptr;

    if (auto cd = s->isClassDeclaration())
        lp = cd->langPlugin();

    return lp == &calypso;
}

}
