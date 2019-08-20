#include "eval.h"
#include "resolver.h"
#include "lexer.h"
#include "visitor.h"
#include "memory.h"
#include "parser.h"
#include <math.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define LINEBUFFSIZE 1048
void *visitBinary(Expression *expr);
void *visitUnary(Expression *expr);
void *visitGrouping(Expression *expr);
void *visitLiteral(Expression *expr);
void *visitVarExpression(Expression *expr);
void *visitAssign(Expression *expr);
void *visitLogical(Expression *expr);
void *visitCallable(Expression *expr);
void *visitGet(Expression *expr);
void *visitSet(Expression *expr);
void *visitPrint(Statement *stmt);
void *visitExpr(Statement *stmt);
void *visitVar(Statement *stmt);
void *visitBlock(Statement *stmt);
void *visitIfElse(Statement *stmt);
void *visitWhile(Statement *stmt);
void *visitFun(Statement *stmt);
void *visitReturn(Statement *stmt);

static Object *execute_block(BlockStatements *stmt);
static Object *instance_get(Object *instance, Token name);
static Object *find_method(Class type, Object *instanceObj, char *name);
static void instance_set(ClassInstance *instance, Token name, Object *value);
static Object *lookup_var(int order, char *name);
static void callable_bind(Object *instanceObj, Callable *method);

ExpressionVisitor EvaluvateVisitor = {
    visitBinary,
    visitUnary,
    visitLiteral,
    visitGrouping,
    visitVarExpression,
    visitAssign,

    visitLogical,
    visitCallable,
    visitGet,
    visitSet};
StatementVisitor EvaluateStmtVisitor = {
    visitPrint,
    visitVar,
    visitExpr,
    visitBlock,
    visitIfElse,
    visitWhile,
    visitFun,
    visitReturn,

};
ExecutionEnvironment GlobalExecutionEnvironment = {NULL, NULL};
ExecutionEnvironment *CurrentEnv = &GlobalExecutionEnvironment;
static Object *new_void()
{
    return obj_new(OBJECT_VOID, NULL, 0);
}
static Object *new_number(double value)
{
    double *holder = (double *)alloc(sizeof(double));
    *holder = value;
    return obj_new(OBJECT_NUMBER, holder, sizeof(double));
}
static Object* new_bool(int truthy)
{
    char* value = (char*)alloc(sizeof(char));
    *value = (char)truthy;
    return obj_new(OBJECT_BOOL, value, 1);
}

static Object* eval_expr(Expression* expr)
{
    return (Object*)accept_expr(EvaluvateVisitor, expr);
}

Object *runtime_error(const char *format, Object **obj, int line, ...)
{
    const char *runtimeError = line != -1 ? "Runtime Error (at Line %d): " : "Runtime Error: ";
    size_t len = 0;
    char buffer[LINEBUFFSIZE];
    Object *temp = NULL;
    va_list fields;
    memset(buffer, 0, LINEBUFFSIZE);
    sprintf(buffer, runtimeError, line);
    va_start(fields, line);
    vsnprintf((char *const)(buffer + strlen(buffer)), LINEBUFFSIZE, format, fields);
    va_end(fields);
    len = strlen(buffer) + 1;
    if (obj == NULL)
    {
        obj = &temp;
    }

    if (*obj == NULL)
    {
        *obj = (Object *)alloc(sizeof(Object));
    }
    (*obj)->type = OBJECT_ERROR;
    (*obj)->value = clone(buffer, len);
    (*obj)->valueSize = len;
    (*obj)->shallow = 1;
    return *obj;
}
void *visitBinary(Expression *expr)
{
    const BinaryExpression * bExpr =(BinaryExpression *) (expr -> expr);
    Object * rObject=eval_expr(bExpr -> rightExpr);
    Object *lObject =eval_expr(bExpr -> leftExpr);
    Object * result = NULL;
    double *lValue =NULL,*value =NULL;
    
}