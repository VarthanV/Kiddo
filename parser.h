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
    EXPR_UNARY,
    EXPR_ASSIGNMENT,
    EXPR_LOGICAL,
    EXPR_CALL,
    EXPR_GET,
    EXPR_SET
} ExpressionType;
typedef enum
{
    LITERAL_NOTHING,
    BOOL,
    LITERAL_NUMBER,
    LITERAL_STRING

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

typedef struct expression_call_t
{
    Expression *callee;
    Token paren;
    List *args;
} CallExpression;

typedef struct
{
    void *value;
    LiteralType type;
    size_t valueSize;

} LiteralExpression;
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
    STMT_VAR_DECLARATION,
    STMT_IF_ELSE,
    STMT_WHILE
} StatementType;
typedef struct stmt_print_t {
    Expression* expr;
} PrintStatement;


typedef struct stmt_while_t {
    Expression* condition;
    Statement* body;
} WhileStatement;
typedef struct expression_variable_t
{
    Token variableName;
} VariableExpr;
typedef struct stmt_fun_t {
    List* args;
    Token name;
    Statement* body;
} FunStatement;

typedef struct
{
    StatementType type;
    void *statement;

} Statement;
typedef struct stmt_var_declaration_t
{
    Expression *initializer;
    Token varName;
} VarDeclarationStmt;

typedef struct
{
    Expression *expr;

} DisplayStatement;

typedef struct
{
    Expression *expr;
} ExpressionStatement;

typedef struct
{
    List *innerStatements;
} BlockStatements;

typedef struct
{
    Expression *condition;
    Statement *thenStmt;
    Statement *elseStmt;
} IfElseStatement;
typedef struct expression_set_t
{
    Expression *object;
    Token name;
    Expression *value;
} SetExpression;

typedef struct expression_unary_t
{
    Token op;
    Expression *expr;
} UnaryExpression;

typedef struct
{
    Expression *condition;
    Statement *body;

} RepeatStatement;
typedef struct
{
    List *args;
    Token name;
    Statement *body;

} MethodStatement;

typedef struct expression_assignmnt_t
{
    Token variableName;
    Expression *rightExpr;
} AssignmentExpr;

typedef struct
{
    Token keyword;
    Expression *value;
} GiveStatement;

typedef struct
{
    List *stmts;
    Expression *expr;
} ParsingContext;
typedef struct expression_get_t
{
    Expression *object;
    Token name;
} GetExpression;
typedef struct expression_grouping_t
{
    Expression *expr;
} GroupingExpression;

ParsingContext parse(Tokenization toknz);
void paserDestroy(ParsingContext *ctx);
void parseError(Token *token, const char *msg);
#define ENDOFTOKEN(x) ((x) == ENDOFFILE)
#define DOMATCH(x, type) ((x) == type)
#define UNKNOWN_IDENTIFIER "Error ! You are using undeclared variable"
#define ERROR_AT_EOF " Error at end of file: %s\n"
#define ERROR_AT_LINE "Error at  (Line %d): %s '%s'\n"
#define MAX_ARGS 8

#endif