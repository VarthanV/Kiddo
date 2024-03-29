#include "list.h"
#include "eval.h"
#include "memory.h"
#include "resolver.h"

void for_stmts(List *stmts, void *stmtObj)
{
    Statement *stmt = (Statement *)stmtObj;
    int resolved = resolve(stmt);
    if (resolved)
    {
        eval(stmt);
    }
}
void interp(const char *code)
{
    Tokenization toknz = toknzr(code, 1);
    ParsingContext ctx = parse(toknz);
    if (ctx.stmts != NULL)
    {
        list_foreach(ctx.stmts, for_stmts);
    }
    parser_destroy(&ctx);
    toknzr_destroy(toknz);
}
