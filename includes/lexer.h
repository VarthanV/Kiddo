#ifndef TOKNZR_H
#define TOKNZR_H
#include "includes/list.h"

typedef enum tokentype
{
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_MINUS,
    TOKEN_PLUS,
    TOKEN_SEMICOLON,
    TOKEN_SLASH,
    TOKEN_STAR,

    TOKEN_BANG,
    TOKEN_BANG_EQUAL,
    TOKEN_EQUAL,
    TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,

    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_NUMBER,

    TOKEN_AND,
    TOKEN_CLASS,
    TOKEN_ELSE,
    TOKEN_FALSE,
    TOKEN_FOR,
    TOKEN_METHOD,
    TOKEN_IF,
    TOKEN_NIL,
    TOKEN_OR,
    TOKEN_DISPLAY,
    TOKEN_GIVE,
    TOKEN_TRUE,
    TOKEN_VAR,
    TOKEN_WHILE,

    TOKEN_ERROR,
    TOKEN_ENDOFFILE
} TokenType;

typedef struct tokenization
{
    List *values;
    int lines;
} Tokenization;

typedef struct token
{
    TokenType type;
    char *literal;
    char *lexeme;
    int column;
    int line;
} Token;

Tokenization toknzr(const char *code, int verbose);
void toknzr_destroy(Tokenization toknz);

#define IS_AT_END(x, codeLength) ((x) >= (codeLength))
#define IS_ALPHA_NUMERIC(x) (isalpha((x)) || isdigit((x) || (x) == '_'))

#define AND_KEY "AND"
#define ELSE_KEY "ELSE"
#define FALSE_KEY "FALSE"
#define FUN_KEY "METHOD"
#define FOR_KEY "FOR"
#define IF_KEY "IF"
#define NIL_KEY "NIL"
#define OR_KEY "OR"
#define PRINT_KEY "DISPLAY"
#define RETURN_KEY "GIVE"
#define TRUE_KEY "TRUE"
#define VAR_KEY "VAR"
#define WHILE_KEY "WHILE"

#endif
