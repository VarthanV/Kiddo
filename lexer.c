#include "includes/lexer.h"
#include "includes/list.h"
#include "includes/common.h"
#include "includes/memory.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define LINEBUFSIZE 1048
static Token *token(TokenType type, char *literal, int line, int column, char *lexeme)
{
    size_t length = 0;
    Token *token = (Token *)alloc(sizeof(Token));
    token->type = type;
    if (literal == NULL)
    {
        token->literal = NULL;
    }
    else
    {
        length = strlen(literal) + 1;
        token->literal = (char *)alloc(length);
        strncpy(token->literal, literal, length);
    }
    if (lexeme == NULL)
    {
        token->lexeme = NULL;
    }
    else
    {
        length = strlen(lexeme) + 1;
        token->lexeme = (char *)alloc(length);
        strncpy(token->lexeme, lexeme, length);
    }
    token->line = line;
    token->column = column;
    return token;
}

static Token *tokenSimple(TokenType type, int line, int column, char *lexeme)
{
    return token(type, NULL, line, column, lexeme);
}

static void token_destroy(Token *token)
{
    fr(token->literal);
    fr(token->lexeme);
    fr(token);
}

static void toknzrError(int line, int column, char c)
{
    char buf[LINEBUFSIZE];
    memset(buf, 0, LINEBUFSIZE);
    snprintf(buf, LINEBUFSIZE, "Syntax Error at (%d, %d): %c is unexpected", line, column + 1, c);
    puts(buf);
}

static int match_next(const char *code, char next, size_t length, int *current)
{
    if (current == NULL || (*current) > length)
    {
        return 0;
    }

    if (code[(*current) + 1] != next)
    {
        return 0;
    }

    (*current)++;
    return 1;
}

static char *read_number(const char *code, size_t codeLength, int *current)
{
    char *literal = NULL;
    int start = *current, length = 0;
    do
    {
        (*current)++;
    } while (!IS_AT_END(*current, codeLength) && isdigit(code[*current]));
    if (code[*current] == '.' && isdigit(code[(*current) + 1]))
    {
        do
        {
            (*current)++;
        } while (!IS_AT_END(*current, codeLength) && isdigit(code[*current]));
    }
    length = *current - start + 1;
    (*current)--;
    literal = (char *)alloc(length);
    memcpy(literal, &(code[start]), length);
    literal[length - 1] = '\0';
    return literal;
}

static char *read_other(const char *code, size_t codeLength, int *current)
{
    char *literal = NULL;
    int start = *current, length = 0;
    do
    {
        (*current)++;
    } while (!IS_AT_END(*current, codeLength) && IS_ALPHA_NUMERIC(code[*current]));
    length = *current - start + 1;
    (*current)--;
    literal = (char *)alloc(length);
    memcpy(literal, &(code[start]), length);
    literal[length - 1] = '\0';
    return literal;
}

Tokenization toknzr(const char *code, int verbose)
{
    char *literal = NULL;
    Token *token = NULL;
    TokenType type = TOKEN_ENDOFFILE;
    size_t length = strlen(code);
    int current = 0, start = 0, line = 1;
    Tokenization toknz;
    toknz.values = list();
    toknz.lines = 0;
    while (!IS_AT_END(current, length))
    {
        char c = code[current];
        switch (c)
        {
        case '(':
            token = token_simple(TOKEN_LEFT_PAREN, line, current, (char *)"(");
            break;
        case ')':
            token = token_simple(TOKEN_RIGHT_PAREN, line, current, (char *)")");
            break;
        case '{':
            token = token_simple(TOKEN_LEFT_BRACE, line, current, (char *)"{");
            break;
        case '}':
            token = token_simple(TOKEN_RIGHT_BRACE, line, current, (char *)"}");
            break;
        case ',':
            token = token_simple(TOKEN_COMMA, line, current, (char *)",");
            break;
        case '.':
            token = token_simple(TOKEN_DOT, line, current, (char *)".");
            break;
        case '-':
            token = token_simple(TOKEN_MINUS, line, current, (char *)"-");
            break;
        case '+':
            token = token_simple(TOKEN_PLUS, line, current, (char *)"+");
            break;
        case ';':
            token = token_simple(TOKEN_SEMICOLON, line, current, (char *)";");
            break;
        case '*':
            token = token_simple(TOKEN_STAR, line, current, (char *)"*");
            break;
        case '!':
            type = match_next(code, '=', length, &current) ? TOKEN_BANG_EQUAL : TOKEN_BANG;
            token = token_simple(type, line, current, (char *)"!");
            break;
        case '=':
            type = match_next(code, '=', length, &current) ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL;
            token = token_simple(type, line, current, (char *)"=");
            break;
        case '>':
            type = match_next(code, '=', length, &current) ? TOKEN_GREATER_EQUAL : TOKEN_GREATER;
            token = token_simple(type, line, current, (char *)">");
            break;
        case '<':
            type = match_next(code, '=', length, &current) ? TOKEN_LESS_EQUAL : TOKEN_LESS;
            token = token_simple(type, line, current, (char *)"<");
            break;
        case '/':
            type = match_next(code, '/', length, &current) ? TOKEN_ENDOFFILE : TOKEN_SLASH;
            if (type == TOKEN_ENDOFFILE)
            {
                do
                {
                    current++;
                } while (current != length && code[current] != '\n');
            }
            else
            {
                token = token_simple(TOKEN_SLASH, line, current, (char *)"/");
            }
            break;
        case '"':
            start = current;
            do
            {
                if (code[current] == '\n')
                {
                    line++;
                }
                current++;
            } while (code[current] != '"' && !IS_AT_END(current, length));
            if (IS_AT_END(current, length))
            {
                if (verbose)
                {
                    toknzr_error(line, current, code[current]);
                }
                else
                {
                    token = token_simple(TOKEN_ERROR, line, current, "Unterminated string.");
                }
            }
            else
            {
                literal = (char *)alloc(current - start);
                memcpy(literal, &(code[start + 1]), current - start);
                literal[current - start - 1] = 0;
                if (literal != NULL)
                {
                    token = token(TOKEN_STRING, literal, line, current, literal);
                }
            }
            break;
        case ' ':
        case '\r':
        case '\t':
            break;
        case '\n':
            line++;
            break;
        default:
            if (isdigit(c))
            {
                literal = read_number(code, length, &current);
                token = token(TOKEN_NUMBER, literal, line, current, literal);
            }
            else if (isalpha(c))
            {
                literal = read_other(code, length, &current);
                if (strcmp(literal, AND_KEY) == 0)
                {
                    token = token_simple(TOKEN_AND, line, current, (char *)AND_KEY);
                }
                else if (strcmp(literal, ELSE_KEY) == 0)
                {
                    token = token_simple(TOKEN_ELSE, line, current, (char *)ELSE_KEY);
                }
                else if (strcmp(literal, FALSE_KEY) == 0)
                {
                    token = token_simple(TOKEN_FALSE, line, current, (char *)FALSE_KEY);
                }
                else if (strcmp(literal, FUN_KEY) == 0)
                {
                    token = token_simple(TOKEN_METHOD, line, current, (char *)FUN_KEY);
                }
                else if (strcmp(literal, FOR_KEY) == 0)
                {
                    token = token_simple(TOKEN_FOR, line, current, (char *)FOR_KEY);
                }
                else if (strcmp(literal, IF_KEY) == 0)
                {
                    token = token_simple(TOKEN_IF, line, current, (char *)IF_KEY);
                }
                else if (strcmp(literal, NIL_KEY) == 0)
                {
                    token = token_simple(TOKEN_NIL, line, current, (char *)NIL_KEY);
                }
                else if (strcmp(literal, OR_KEY) == 0)
                {
                    token = token_simple(TOKEN_OR, line, current, (char *)OR_KEY);
                }
                else if (strcmp(literal, PRINT_KEY) == 0)
                {
                    token = token_simple(TOKEN_DISPLAY, line, current, (char *)PRINT_KEY);
                }
                else if (strcmp(literal, RETURN_KEY) == 0)
                {
                    token = token_simple(TOKEN_GIVE, line, current, (char *)RETURN_KEY);
                }
                else if (strcmp(literal, TRUE_KEY) == 0)
                {
                    token = token_simple(TOKEN_TRUE, line, current, (char *)TRUE_KEY);
                }
                else if (strcmp(literal, VAR_KEY) == 0)
                {
                    token = token_simple(TOKEN_VAR, line, current, (char *)VAR_KEY);
                }
                else if (strcmp(literal, WHILE_KEY) == 0)
                {
                    token = token_simple(TOKEN_WHILE, line, current, (char *)WHILE_KEY);
                }
                else
                {
                    token = token_simple(TOKEN_IDENTIFIER, line, current, literal);
                }
            }
            else
            {
                if (verbose)
                {
                    toknzr_error(line, current, c);
                }
                else
                {
                    token = token_simple(TOKEN_ERROR, line, current, "Unexpected character.");
                }
            }
            break;
        }
        fr(literal);
        literal = NULL;
        current++;
        if (token != NULL)
        {
            list_push(toknz.values, token);
            token = NULL;
        }
    }
    toknz.lines = line;
    list_push(toknz.values, token_simple(TOKEN_ENDOFFILE, line, current, (char *)"EOF"));
    return toknz;
}

static void tokens_foreach_token(List *tokens, void *tokenObj)
{
    Token *token = (Token *)tokenObj;
    token_destroy(token);
}

void toknzr_destroy(Tokenization toknz)
{
    list_foreach(toknz.values, tokens_foreach_token);
    list_destroy(toknz.values);
}
