#include "compiler.h"
#include "list.h"
#include "lexer.h"
#include "common.h"
#include "table.h"
#include "value.h"
#include "vm.h"
#include "debug.h"
#include <stdio.h>
#include <string.h>

typedef enum
{
    TYPE_FUNCTION,
    TYPE_SCRIPT,
} FunctionType;
static void variable(int assign);
static void literal(int assign);
static void string(int assign);
static void number(int assign);
static void binary(int assign);
static void unary(int assign);
static void grouping(int assign);
static void And(int assign);
static void Or(int assign);
static void call(int assign);
static void expression();
static void var_decalration();
static void declaration();
static void statement();
static void print_statement();
static void expression_statement();
static void function_statement(FunctionType type);
static void return_statement();
static int check(TokenType type);
static int match(TokenType type);
static void advance();
static void error(const char *msg);
static void error_at(Node *node, const char *msg);
static int identifier_equal(Token *token1, Token *token2);
static Byte identifier_constant(Node *node);
static Byte variable_parse(const char *msg);
static void variable_define(Byte id);
static void named_variable(Node *node, int assign);

typedef struct vm_parser
{
    Node *current;
    Node *previous;
    Node *last;
    int panicMode;
    int hadError;
} VmParser;

// ! Defining Precedence

typedef enum
{
    PREC_NONE,
    PREC_ASSIGNMENT,
    PREC_OR,
    PREC_AND,
    PREC_EQUALITY,
    PREC_COMPARISION,
    PREC_FACTOR,
    PREC_UNARY,
    PREC_PRIMARY,
    PREC_TERM,
    PREC_CALL,

} Precedence;
typedef void (*ParseFn)(int assign);
typedef struct parse_rule
{
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;

} ParseRule;
typedef struct
{
    Token name;
    int depth;
} Local;
typedef struct vm_compiler
{
    struct vm_compiler *enclosing;
    VmFunction *function;
    FunctionType type;
    Local locals[BYTE_COUNT];
    int localCount;
    int scopeDepth;
} VmCompiler;
static int variable_local_resolve(VmCompiler *compiler, Token *name);
ParseRule rules[] = {
    {grouping, call, PREC_CALL},
    {NULL, NULL, PREC_NONE},
    {NULL, NULL, PREC_NONE},
    {NULL, NULL, PREC_NONE},
    {NULL, NULL, PREC_NONE},
    {NULL, NULL, PREC_CALL},
    {unary, binary, PREC_TERM},
    {NULL, binary, PREC_TERM},
    {NULL, NULL, PREC_NONE},
    {NULL, binary, PREC_FACTOR},
    {NULL, binary, PREC_FACTOR},
    {unary, NULL, PREC_NONE},
    {NULL, binary, PREC_EQUALITY},
    {NULL, NULL, PREC_NONE},
    {NULL, binary, PREC_EQUALITY},
    {NULL, binary, PREC_COMPARISION},
    {NULL, binary, PREC_COMPARISION},
    {NULL, binary, PREC_COMPARISION},
    {NULL, binary, PREC_COMPARISION},
    {variable, NULL, PREC_NONE},
    {string, NULL, PREC_NONE},
    {number, NULL, PREC_NONE},
    {NULL, And, PREC_AND},

    {NULL, NULL, PREC_NONE},
    {literal, NULL, PREC_NONE},
    {NULL, NULL, PREC_NONE},
    {NULL, NULL, PREC_NONE},
    {NULL, NULL, PREC_NONE},
    {literal, NULL, PREC_NONE},
    {NULL, Or, PREC_OR},
    {NULL, NULL, PREC_NONE},
    {NULL, NULL, PREC_NONE},

    {literal, NULL, PREC_NONE},
    {NULL, NULL, PREC_NONE},
    {NULL, NULL, PREC_NONE},
    {NULL, NULL, PREC_NONE},
    {NULL, NULL, PREC_NONE},

};
static VmParser parser;
static VmCompiler *currentCompiler = NULL;
static ParseRule *parse_rule(TokenType type)
{
    return &rules[type];
}
static void prec_parse(Precedence prec)
{
    ParseFn prefixRule, infixRule;
    Token *token = NULL;
    int assign = 0;
    advance();
    token = (Token *)parser.previous->data;
    prefixRule = parse_rule(token->type)->prefix;
    if (prefixRule == NULL)
    {
        error("EXPECTED EXPRESSION");
        return;
    }
    assign = prec <= PREC_ASSIGNMENT;
    prefixRule(assign);
    while (prec <= parse_rule(((Token *)parser.current->data)->type)->precedence)
    {
        advance();
        infixRule = parse_rule(((Token *)parser.previous->data)->type)->infix;
        infixRule(assign);
    }
    if (assign && match(TOKEN_EQUAL))
    {
        error("CANT ASSIGN");
        expression();
    }
}
static void synchornize()
{
    TokenType currentType = ((Token *)parser.current->data)->type;
    TokenType prevType = ((Token *)parser.previous->data)->type;
    parser.panicMode = 0;
    while (currentType != TOKEN_ENDOFFILE)
    {
        if (prevType == TOKEN_SEMICOLON)
            return;

        switch (currentType)
        {
        case TOKEN_CLASS:
        case TOKEN_METHOD:
        case TOKEN_VAR:
        case TOKEN_FOR:
        case TOKEN_IF:
        case TOKEN_WHILE:
        case TOKEN_DISPLAY:
        case TOKEN_GIVE:
            return;

        default:

            ;
        }

        advance();
    }
}
static int check(TokenType type)
{
    Token *token = (Token *)parser.current->data;
    return token->type == type;
}
static int match(TokenType type)
{
    if (!check(type))
    {
        return 0;
    }
    advance();
    return 1;
}
static void error_at(Node *node, const char *message)
{
    Token *token = (Token *)node->data;
    if (parser.panicMode)
    {
        return;
    }

    parser.panicMode = 1;

    fprintf(stderr, "[line %d] ERROR", token->line);

    if (token->type == TOKEN_ENDOFFILE)
    {
        fprintf(stderr, " AT END");
    }
    else if (token->type == TOKEN_ERROR)
    {
        // Nothing.
    }
    else
    {
        fprintf(stderr, " AT '%s'", token->lexeme);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = 1;
}

static void error(const char *message)
{
    error_at(parser.previous, message);
}

static void error_at_current(const char *message)
{
    error_at(parser.current, message);
}

static void advance()
{
    Node *node = NULL;
    Token *token = NULL;

    parser.previous = parser.current;

    for (node = parser.current; node != parser.last;)
    {
        parser.current = node = node->next;
        token = (Token *)node->data;
        if (token->type != TOKEN_ERROR)
            break;

        error_at_current(token->lexeme);
    }
}
static void consume(TokenType type, const char *message)
{
    Token *token = (Token *)parser.current->data;
    if (token->type == type)
    {
        advance();
        return;
    }

    error_at_current(message);
}

static Chunk *current_chunk()
{
    return &currentCompiler->function->chunk;
}

static void foreach_token(List *toknz, void *toknObj)
{
    int line = -1;
    Token *token = (Token *)toknObj;

    if (token->line != line)
    {
        printf("%4d ", token->line);
        line = token->line;
    }
    else
    {
        printf("   | ");
    }
    printf("%2d '%s'\n", token->type, token->lexeme);
}

static Byte make_constant(Value value)
{
    int constant = chunk_constants_add(current_chunk(), value);
    if (constant > BYTE_MAX)
    {
        error("TOO MANY CONSTANTS TO MAKE");
        return 0;
    }
    return (Byte)constant;
}
static void emit_byte(Byte byte)
{
    Token *token = (Token *)parser.previous->data;
    chunk_write(current_chunk, byte, token->line);
}
static void emit_bytes(Byte byte1, Byte byte2)
{
    emit_byte(byte1);
    emit_byte(byte2);
}
static void emit_constant(Value value)
{
    emit_bytes(OP_CONSTANT, make_constant(value));
}
static void emit_return()
{
    emit_byte(OP_NIL);
    emit_byte(OP_RETURN);
}
static int emit_jump(Byte instruction)
{
    emit_byte(instruction);
    emit_byte(0xff);
    emit_byte(0xff);
    return current_chunk()->count - 2;
}
static void patch_jump(int offset)
{
    int jump = current_chunk()->count - offset - 2;

    if (jump > BYTE_MAX)
    {
        error("TOO MUCH CODE");
    }

    current_chunk()->code[offset] = (jump >> 8) & 0xff;
    current_chunk()->code[offset + 1] = jump & 0xff;
}

static void emit_loop(int loopStart)
{
    int offset = 0;
    emit_byte(OP_LOOP);

    offset = current_chunk()->count - loopStart + 2;

    if (offset > SHORT_MAX)
    {
        error("LOOP VERY LARGE");
    }

    emit_byte((offset >> 8) & 0xff);
    emit_byte(offset & 0xff);
}

static void compiler_init(VmCompiler *compiler, FunctionType type)
{
    Token *token = NULL;
    Local *local = NULL;
    memset(compiler, 0, sizeof(VmCompiler));
    compiler->enclosing = currentCompiler;
    compiler->type = type;
    compiler->function = NULL;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = vmfunction_new();

    if (type != TYPE_SCRIPT)
    {
        token = (Token *)parser.previous->data;
        compiler->function->name = vmstring_copy(token->lexeme, strlen(token->lexeme));
    }
    currentCompiler = compiler;

    local = &currentCompiler->locals[currentCompiler->localCount++];
    local->depth = 0;
    local->name.lexeme = "";
}

static VmFunction *compiler_end()
{
    VmFunction *function = NULL;
    emit_return();
    function = currentCompiler->function;

    if (!parser.hadError)
    {
        chunk_disassemble(current_chunk(), function->name != NULL ? function->name->chars : "<script>");
    }

    currentCompiler = currentCompiler->enclosing;

    return function;
}
static Byte argument_list()
{
    Byte argCount = 0;
    if (!check(TOKEN_RIGHT_PAREN))
    {
        do
        {
            expression();
            argCount++;
        } while (match(TOKEN_COMMA));
    }
    if (argCount == 255)
    {
        error("ARGS ABOVE 255 CHARS NOT ALLOWED");
    }
    consume(TOKEN_RIGHT_PAREN, "EXPECTED ) AFTER ARGUMENTS");
    return argCount;
}
static void call(int assign)
{
    Byte argCount = argument_list();
    emit_bytes(OP_CALL, argCount);
}
static void expression()
{
    prec_parse(PREC_ASSIGNMENT);
}
static void literal(int assign)
{
    Token *token = (Token *)parser.previous->data;
    switch (token->type)
    {
    case TOKEN_FALSE:
        emit_byte(OP_FALSE);
        break;
    case TOKEN_TRUE:
        emit_byte(OP_TRUE);
        break;
    case TOKEN_NIL:
        emit_byte(OP_NIL);
        break;

    default:
        return;
    }
}
static VmObject *new_vmobject(size_t size, VmObjectType type)
{
    VmObject *object = (VmObject *)reallocate(NULL, 0, size);
    object->type = type;
    object->next = vm.objects;
    vm.objects = object;
    return object;
}

#define ALLOC_OBJECT(type, objectType) ((type *)new_vmobject(sizeof(type), (objectType)))
VmFunction *vmfunction_new()
{
    VmFunction *function = ALLOC_OBJECT(VmFunction, OBJECT_FUNCTION);
    function->name = NULL;
    chunk_init(&function->chunk);
    return function;
}
VmNative *vmnative_new(NativeFn function)
{
    VmNative *native = ALLOC_OBJECT(VmNative, OBJECT_NATIVE);
    native->function = function;
    return native;
}
static Hash hash_string(const char *string, size_t length)
{
    unsigned int hash = 2166136261u;
    size_t i;

    for (i = 0; i < length; i++)
    {
        hash ^= string[i];
        hash *= 16777619;
    }

    return hash;
}
static VmString *new_vmstring(char *chars, size_t length, Hash hash)
{
    VmString *string = ALLOC_OBJECT(VmString, OBJECT_STRING);
    string->chars = chars;
    string->length = length;
    string->hash = hash;
    table_set(&vm.strings, string, nil_val());
    return string;
}
VmString *vmstring_copy(const char *chars, size_t length)
{
    char *heapChars = NULL;
    Hash hash = hash_string(chars, length);
    VmString *interned = table_find_string(&vm.strings, chars, length, hash);

    if (interned != NULL)
    {
        return interned;
    }

    heapChars = (char *)alloc(length + 1);
    memcpy(heapChars, chars, length);
    heapChars[length] = 0;
    return new_vmstring(heapChars, length, hash);
}
VmString *vmstring_take(char *chars, size_t length)
{
    Hash hash = hash_string(chars, length);
    VmString *interned = table_find_string(&vm.strings, chars, length, hash);

    if (interned != NULL)
    {
        return interned;
    }

    return new_vmstring(chars, length, hash);
}

static void variable(int assign)
{
    named_variable(parser.previous, assign);
}
static void named_variable(Node *node, int assign)
{
    Byte getOp, setOp;
    Token *name = (Token *)node->data;
    int arg = variable_local_resolve(currentCompiler, name);

    if (arg != -1)
    {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    }
    else
    {
        arg = identifier_constant(node);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }
    if (assign && match(TOKEN_EQUAL))
    {
        expression();
        emit_bytes(setOp, (Byte)arg);
    }
    else
    {
        emit_bytes(getOp, (Byte)arg);
    }
}

static void string(int assign)
{
    Token *token = (Token *)parser.previous->data;
    VmString *string = vmstring_copy(token->literal, strlen(token->literal));
    Value stringValue = object_val((VmObject *)string);
    emit_constant(stringValue);
}

static void number(int canAssign)
{
    Token *token = (Token *)parser.previous->data;
    double value = strtod(token->lexeme, NULL);
    emit_constant(number_val(value));
}

static void grouping(int assign)
{
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void unary(int assign)
{
    Token *token = (Token *)parser.previous->data;
    TokenType operatorType = token->type;

    prec_parse(PREC_UNARY);

    switch (operatorType)
    {
    case TOKEN_MINUS:
        emit_byte(OP_NEGATE);
        break;
    case TOKEN_BANG:
        emit_byte(OP_NOT);
        break;
    default:
        return;
    }
}

static void binary(int assign)
{
    Token *token = (Token *)parser.previous->data;
    TokenType operatorType = token->type;

    ParseRule *rule = parse_rule(operatorType);
    prec_parse((Precedence)(rule->precedence + 1));

    switch (operatorType)
    {
    case TOKEN_PLUS:
        emit_byte(OP_ADD);
        break;
    case TOKEN_MINUS:
        emit_byte(OP_SUBTRACT);
        break;
    case TOKEN_STAR:
        emit_byte(OP_MULTIPLY);
        break;
    case TOKEN_SLASH:
        emit_byte(OP_DIVIDE);
        break;
    case TOKEN_BANG_EQUAL:
        emit_bytes(OP_EQUAL, OP_NOT);
        break;
    case TOKEN_EQUAL_EQUAL:
        emit_byte(OP_EQUAL);
        break;
    case TOKEN_GREATER_EQUAL:
        emit_bytes(OP_LESS, OP_NOT);
        break;
    case TOKEN_GREATER:
        emit_byte(OP_GREATER);
        break;
    case TOKEN_LESS:
        emit_byte(OP_LESS);
        break;
    case TOKEN_LESS_EQUAL:
        emit_bytes(OP_GREATER, OP_NOT);
        break;
    default:
        return;
    }
}