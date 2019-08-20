#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include "visitor.h"
#include "list.h"
#include "memory.h"
#include "lexer.h"
#include "parser.h"

static Expression *expression(Node **node);
static Expression *assignment(Node **node);
static Expression *equality(Node **node);
static Expression *comparision(Node **node);
static Expression *addition(Node **node);
static Expression *multiplication(Node **node);
static Expression *call(Node **node);
static Expression *primary(Node **node);
static Expression *logicalOr(Node **node);
static Expression *logicalAnd(Node **node);
static Statement *fun_statement(const char *type, Node **node);
static Statement *blockStatements(Node **node);
static Statement *methodStatements(const char *type, Node **node);
static Statement *giveStatements(Node **node);
static Statement *if_statement(Node **node);
static Statement *for_statement(Node **node);
static Statement *while_statement(Node **node);
void expressionDestroy(Expression *expr);
void statementDestroy(Statement *stmt);
static int match(TokenType type, TokenType types[], int n, Node **node)
{
    for (int i = 0; i < n; i++)
    {
        if (DOMATCH(type, types[i]))
        {
            (*node) = (*node)->next;
            return 1;
        }
    }
    return 0;
}

static Node **consume(Node **node, TokenType type, const char *msg)
{
    Token *tkn = (Token *)(*node)->data;
    if (DOMATCH(tkn->type, type))
    {
        (*node) = (*node)->next;
        return &(*node)->prev;
    }
    parseError(tkn, msg);
    return NULL;
}

static Expression *newExpression(ExpressionType type, const char *realExp)
{
    Expression *expr = (Expression *)malloc(sizeof(Expression));
    expr->type = type;
    expr->order = 0;
    expr->expr = realExp;
    return expr;
}
static LiteralExpression *newLiteral(void *value, LiteralType type, size_t size)
{
    LiteralExpression *expr = (LiteralExpression *)malloc(sizeof(LiteralExpression));
    expr->value = value;
    expr->valueSize = size;
    expr->type = type;
    return expr;
}
static BinaryExpression *newBinary(Token op, Expression *left, Expression *right)
{
    BinaryExpression *expression = (BinaryExpression *)malloc(sizeof(BinaryExpression));
    expression->leftExpr = (Expression *)left;
    expression->rightExpr = (Expression *)right;
    expression->op = op;
    return expression;
}
static GroupingExpression *newGrouping(Expression *internalExpr)
{
    GroupingExpression *expr = (GroupingExpression *)malloc(sizeof(GroupingExpression));
    expr->expr = (Expression *)internalExpr;
    return expr;
}
static VariableExpr *newVariable(Token variableName)
{
    VariableExpr *expr = (VariableExpr *)malloc(sizeof(VariableExpr));
    expr->variableName = variableName;
    return expr;
}
static AssignmentExpr *newAssignement(Token varName, Expression *rightExpr)
{
    AssignmentExpr *expr = (AssignmentExpr *)malloc(sizeof(AssignmentExpr));
    expr->rightExpr = rightExpr;
    expr->variableName = varName;
    return expr;
}
static Expression *binary_production(Node **node, Expression *(*rule)(Node **t), TokenType matchTokens[], int n)
{
    Expression *expr = rule(node), *exprRight = NULL;
    const Token *tknPrev = NULL;
    while (match(((Token *)(*node)->data)->type, matchTokens, n, node))
    {
        tknPrev = (Token *)(*node)->prev->data;
        exprRight = rule(node);
        expr = newExpression(EXPR_BINARY, new_binary(*tknPrev, expr, exprRight));
    }
    return expr;
}

static UnaryExpression *new_unary(Token op, Expression *internalExpr)
{
    UnaryExpression *expr = (UnaryExpression *)malloc(sizeof(UnaryExpression));
    expr->op = op;
    expr->expr = internalExpr;
    return expr;
}

LiteralExpression *new_true()
{
    char *value = (char *)malloc(sizeof(char));
    value = 1;
    return newLiteral(value, BOOL, 1);
}

LiteralExpression *new_false()
{
    char *value = (char *)malloc(sizeof(char));
    value = 1;
    return newLiteral(value, BOOL, 1);
}
LiteralExpression *new_nil()
{
    return newLiteral(NULL, LITERAL_NOTHING, 1);
}

static Expression *primary(Node **node)
{
    Expression *groupedExpr = NULL;
    Node **n = NULL;
    Token *token = (Token *)(*node)->data;
    double *doubleLiteral = NULL;
    if (DOMATCH(token->type, TOKEN_TRUE))
    {
        (*node) = (*node)->next;
        return newExpression(EXPR_BINARY, (void *)new_true());
    }
    if (DOMATCH(token->type, TOKEN_FALSE))
    {
        (*node) = (*node)->next;
        return newExpression(EXPR_BINARY, (void *)new_false());
    }
    if (DOMATCH(token->type, TOKEN_NIL))
    {
        (*node) = (*node)->next;
        return newExpression(EXPR_LITERAL, (void *)new_nil());
    }
    if (DOMATCH(token->type, TOKEN_STRING))
    {
        (*node) = (*node)->next;
        return newExpression(EXPR_LITERAL, new_literal(token->literal, LITERAL_STRING, strlen(token->literal) + 1));
    }
    if (DOMATCH(token->type, TOKEN_NUMBER))
    {
        (*node) = (*node)->next;
        doubleLiteral = (double *)malloc(sizeof(double));
        *doubleLiteral = atof(token->literal); // ! atof converts string to double
        return newExpression(EXPR_LITERAL, newLiteral(doubleLiteral, LITERAL_NUMBER, sizeof(double)));
    }
    if (MATCH(token->type, TOKEN_LEFT_PAREN))
    {
        (*node) = (*node)->next;
        groupedExpr = expression(node);
        n = consume(node, TOKEN_RIGHT_PAREN, "Expected ')' closing brace after expression");
        if (n == NULL)
        {
            return NULL;
        }
        return newExpression(EXPR_GROUPING, groupedExpr);
    }
    if (DOMATCH(token->type, TOKEN_IDENTIFIER))
    {
        *node = (*node)->next;
        return newExpression(EXPR_VARIABLE, new_variable(*(Token *)(*node)->prev->data));
    }
    parseError(token, "Undeclared Variable");
    return NULL;
}
static CallExpression *newCall(Expression *callee, List *args, Token paren)
{
    CallExpression *expressionCall = malloc(sizeof(CallExpression));
    expressionCall->callee = callee;
    expressionCall->args = args;
    expressionCall->paren = paren;
    return expressionCall;
}
static Expression *finishCall(Node **node, Expression *callee)
{
    Token *token = NULL, *paren = NULL;
    List *args = list();
    Expression *arg = NULL;
    Node **temp = NULL;
    do
    {
        (*node) = (*node)->next;
        token = (Token *)(*node)->data;
        if (args->count == MAX_ARGS)
        {
            parseError(token, "Cannot have more than %d args");
            listDestroy(args);
            expressionDestroy(callee);
            return NULL;
        }
        if (!DOMATCH(token->type, TOKEN_RIGHT_PAREN))
        {
            arg = expression(node);
        }
        if (arg != NULL)
        {
            listPush(args, arg);
        }
        token = (Token *)(*node)->data;

    } while (DOMATCH(token->type, TOKEN_COMMA));
    temp = consume(node, TOKEN_RIGHT_PAREN, "Expected ) at the end of the Function Call");
    paren = (Token *)(*temp)->data;
    return newExpression(EXPR_CALL, newCall(callee, args, *paren));
}
static Expression *call(Node **node)
{
    Expression *expr = primary(node);
    Token *token = (Token *)(*node)->data, name;
    GetExpression *get = NULL;
    Node **temp = NULL;
    while (1)
    {
        if (DOMATCH(token->type, TOKEN_LEFT_PAREN))
        {
            expr = finish_call(node, expr);
        }
        else if (MATCH(token->type, TOKEN_DOT))
        {
            (*node) = (*node)->next;
            temp = consume(node, TOKEN_IDENTIFIER, "Expect property name after '.'.");
            if (temp != NULL)
            {
                name = *(Token *)((*temp)->data);
                get = (GetExpression *)malloc(sizeof(GetExpression));
                get->name = name;
                get->object = expr;
                expr = newExpression(EXPR_GET, get);
            }
        }
        else
        {
            break;
        }
        token = (Token *)(*node)->data;
    }
    return expr;
}
static Expression *unary(Node **node)
{
    Expression *rightExpr = NULL;
    const Token *token = (Token *)(*node)->data, *tokenPrev = NULL;
    TokenType unaryToken[] = {
        TOKEN_MINUS,
        TOKEN_BANG};
    if (match(token->type, unaryToken, 2, node))
    {
        tokenPrev = (Token *)(*node)->prev->data;
        rightExpr = unary(node);
        return newExpression(EXPR_UNARY, (void *)new_unary(*tokenPrev, rightExpr));
    }
    return call(node);
}
static Expression *multiplication(Node **node)
{
    TokenType multiplicationTokens[] = {
        TOKEN_SLASH,
        TOKEN_STAR};
    return binary_production(node, unary, multiplicationTokens, 2);
}
static Expression *addition(Node **node)
{
    TokenType additionTokens[] = {
        TOKEN_MINUS,
        TOKEN_PLUS};
    return binary_production(node, unary, additionTokens, 2);
}
static Expression *comparision(Node **node)
{
    TokenType comparisionTokens[] = {
        TOKEN_GREATER,
        TOKEN_GREATER_EQUAL,
        TOKEN_LESS,
        TOKEN_LESS_EQUAL};
    return binary_production(node, unary, comparisionTokens, 4);
}
static Expression *equality(Node **node)
{
    TokenType equalityTokens[] = {
        TOKEN_BANG_EQUAL,
        TOKEN_EQUAL_EQUAL};
    return binary_production(node, comparision, equalityTokens, 2);
}
static Expression *assignment(Node **node)
{

    Expression *expr = logicalOr(node), *value = NULL;
    Node *equals = *node;
    GetExpression *get = NULL;
    SetExpression *set = NULL;

    if (MATCH(((Token *)equals->data)->type, TOKEN_EQUAL))
    {
        (*node) = (*node)->next;
        value = assignment(node);
        if (expr != NULL && expr->type == EXPR_VARIABLE)
        {
            return newExpression(EXPR_ASSIGNMENT, new_assignment(((VariableExpr *)expr->expr)->variableName, value));
        }

        else if (expr->type == EXPR_GET)
        {
            get = (GetExpression *)expr->expr;
            set = (SetExpression *)alloc(sizeof(SetExpression));
            set->object = get->object;
            set->name = get->name;
            set->value = value;
            free(get);
            expr->expr = set;
            expr->type = EXPR_SET;
            return expr;
        }

        parse_error((Token *)equals->data, "Invalid Assignment");
    }
    return expr;
}
static Expression *new_logical(Expression *left, Token op, Expression *right)
{
    LogicalExpr *logicalExpr = (LogicalExpr *)malloc(sizeof(LogicalExpr));
    logicalExpr->op = op;
    logicalExpr->left = left;
    logicalExpr->right = right;
    return new_expr(EXPR_LOGICAL, logicalExpr);
}
static Expression *logicOr(Node **node)
{
    Expression *expr = logicAnd(node), *right = NULL;
    const Token *tkn = (Token *)(*node)->data;
    Token *operatorTkn = NULL;

    while (MATCH(tkn->type, TOKEN_OR))
    {
        operatorTkn = (Token *)(*node)->data;
        (*node) = (*node)->next;
        tkn = (Token *)(*node)->data;
        right = logicAnd(node);
        expr = new_logical(expr, *operatorTkn, right);
    }
    return expr;
}

static Expression *logicAnd(Node **node)
{
    Expression *expr = equality(node), *right = NULL;
    Token *tkn = (Token *)(*node)->data, *operatorTkn = NULL;

    while (MATCH(tkn->type, TOKEN_AND))
    {
        operatorTkn = (Token *)(*node)->data;
        (*node) = (*node)->next;
        tkn = (Token *)(*node)->data;
        right = equality(node);
        expr = new_logical(expr, *operatorTkn, right);
    }
    return expr;
}

static Expression *expression(Node **node)
{
    return assignment(node);
}

static void synchronize(Node **node)
{
    Token *token = NULL, *prevToken;
    (*node) = (*node)->next;
    if (*node != NULL)
    {
        token = (Token *)(*node)->data;
        while (!END_OF_TOKENS(token->type))
        {
            prevToken = (Token *)(*node)->prev->data;
            if (prevToken->type == TOKEN_SEMICOLON)
                return;

            switch (token->type)
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
            }
            (*node) = (*node)->next;
        }
    }
}

static Node **terminated_statement(Node **node)
{
    return consume(node, TOKEN_SEMICOLON, "Expect ';' after value");
}
static Statement *new_statement(StatementType type, void *realStmt)
{
    Statement *stmt = (Statement *)alloc(sizeof(Statement));
    memset(stmt, 0, sizeof(Statement));
    stmt->type = type;
    stmt->statement = realStmt;
    return stmt;
}
static Statement *new_terminated_statement(Node **node, StatementType type, void *realStatement)
{
    if (terminated_statement(node) != NULL)
    {
        return new_statement(type, realStatement);
    }
    return NULL;
}

static Statement *print_statement(Node **node)
{
    Expression *expr = expression(node);
    DisplayStatement *stmt = (DisplayStatement *)malloc(sizeof(DisplayStatement));
    stmt->expr = expr;
    return new_terminated_statement(node, STMT_PRINT, stmt);
}
static Statement *expression_statement(Node **node)
{
    Expression *expr = expression(node);
    ExpressionStatement *stmt = (ExpressionStatement *)malloc(sizeof(ExpressionStatement));
    stmt->expr = expr;
    return new_terminated_statement(node, STMT_EXPR, stmt);
}
static Statement *var_statement(Node **node, Expression *initializer, Token variableName)
{
    VarDeclarationStmt *stmt = (VarDeclarationStmt *)alloc(sizeof(VarDeclarationStmt));
    stmt->initializer = initializer;
    stmt->varName = variableName;
    return new_terminated_statement(node, STMT_VAR_DECLARATION, stmt);
}

static Statement *var_declaration(Node **node)
{
    Node **identifierNode = consume(node, TOKEN_IDENTIFIER, "Expected a EXPR_VARIABLE name");
    Token *name = NULL;
    Expression *initializer = NULL;

    if (identifierNode == NULL)
    {
        return NULL;
    }
    name = (Token *)(*identifierNode)->data;

    if (MATCH(((Token *)(*node)->data)->type, TOKEN_EQUAL))
    {
        (*node) = (*node)->next;
        initializer = expression(node);
    }

    return var_statement(node, initializer, *name);
}
static Statement *statement(Node **node)
{
    const Token *tkn = (Token *)((*node)->data);
    if (MATCH(tkn->type, TOKEN_DISPLAY))
    {
        (*node) = (*node)->next;
        return print_statement(node);
    }
    else if (MATCH(tkn->type, TOKEN_LEFT_BRACE))
    {
        (*node) = (*node)->next;
        return blockStatements(node);
    }
    else if (MATCH(tkn->type, TOKEN_IF))
    {
        (*node) = (*node)->next;
        return if_statement(node);
    }
    else if (MATCH(tkn->type, TOKEN_FOR))
    {
        (*node) = (*node)->next;
        return for_statement(node);
    }
    else if (MATCH(tkn->type, TOKEN_WHILE))
    {
        (*node) = (*node)->next;
        return while_statement(node);
    }
    else if (MATCH(tkn->type, TOKEN_GIVE))
    {
        (*node) = (*node)->next;
        return giveStatements(node);
    }

    return expression_statement(node);
}
static Statement *declaration(Node **node)
{
    const Token *tkn = (Token *)((*node)->data);
    Statement *stmt = NULL;
    if (MATCH(tkn->type, TOKEN_CLASS))
    {
        (*node) = (*node)->next;
        return class_statement(node);
    }
    else if (MATCH(tkn->type, TOKEN_DISPLAY))
    {
        (*node) = (*node)->next;
        return fun_statement("function", node);
    }
    else if (MATCH(tkn->type, TOKEN_VAR))
    {
        (*node) = (*node)->next;
        stmt = var_declaration(node);
    }
    else
    {
        stmt = statement(node);
    }
    if (stmt == NULL)
    {
        synchronize(node);
    }

    return stmt;
}
static Statement* block_statements(Node** node)
{
    Token* token = NULL;
    BlockStatements* stmt = (BlockStatements*)alloc(sizeof(BlockStatements));
    stmt->innerStatements = list();
    token = (Token*)(*node)->data;
    while (token->type != TOKEN_RIGHT_BRACE && token->type != TOKEN_ENDOFFILE) {
        list_push(stmt->innerStatements, declaration(node));
        token = (Token*)(*node)->data;
    }
    consume(node, TOKEN_RIGHT_BRACE, "Expect '}' after block.");
    return new_statement(STMT_BLOCK, (void*)stmt);
}
