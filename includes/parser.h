#ifndef PARSE_H
#define PARSE_H
#include "lexer.h"
#include <stdio.h>
#include "list.h"
typedef enum
{
    EXPR_BINARY,
    EXPR_GROUPING,
    EXPR_LITERAL,
    EXPR_VARIABLE,
    EXPR_ASSIGNMENT,
    EXPR_LOGICAL,

} ExpressionType;
typedef enum
{
    NOTHING,
    TRUE,
    FALSE,
    NUMBER,
    STRING

} LiteralType;

typedef struct
{
    ExpressionType type;
    void *expr;
    int order;

} Expression;

typedef struct
{
    Token op;
    Expression *leftExpr;
    Expression *rightExpr;
} BinaryExpression;
typedef struct
{
    Token varName;

} Variable;
typedef struct
{
    void *value;
    LiteralType type;
    size_t valueSize;

} LiteralExpr;
typedef struct
{
    Expression *left;
    Expression *right;
    Token op;

} LogicalExpr;
typedef enum
{
    STMT_PRINT,
    STMT_DECLARATION,
    STMT_BLOCK,
    STMT_EXPR,
    STMT_METHOD,
    STMT_REPEAT,
    STMT_GIVE,

} StatementType;

typedef struct
{
    StatementType type;
    void *statement;

} Statement;
typedef struct {
Expression *expr;

}PrintStatement;

typedef struct{
 Expression *expr;   
}ExpressionStatement;
typedef struct{
Expression *initializer;
Token varName;

}VarDeclarationStatement;
typedef struct{
List*  innerStatements;
}BlockStatements;

typedef struct{
Expression *condition;
Statement * thenStmt;
Statement *elseStmt;
}IfElseStatement;

typedef struct{
Expression * condition;
Statement *body;

}RepeatStatement;
typedef struct{
List *args;
Token name;
Statement * body;

} MethodStatement;

typedef struct{
Token keyword;
Expression * value;
}GiveStatement;

typedef struct  {
    List* stmts;
    Expression* expr;
} ParsingContext;

ParsingContext parse(Tokenization toknz);
void paserDestroy(ParsingContext *ctx);
void parseError(Token *token,const char *msg);
# define ENDOFTOKEN(x) ((x)==  ENDOFFILE)
#define MATCH(x, type) ((x) == type)
#define UNKNOWN_IDENTIFIER "Error ! You are using undeclared variable"
#define ERROR_AT_EOF " Error at end of file: %s\n"
#define ERROR_AT_LINE "Error at  (Line %d): %s '%s'\n"
#define MAX_ARGS 8 

#endif