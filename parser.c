#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include "visitor.h"
#include "list.h"
#include "memory.h"
#include "lexer.h"
#include "parser.h"
#include "common.h"
#define LINEBUFSIZE 1024* sizeof(char)
// !Definitions
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
void expr_destroy(Expression *expr);
void statementDestroy(Statement *stmt);
// ! Methods
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

    if (MATCH(tkn->type, TOKEN_DISPLAY))
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
static Statement *block_statements(Node **node)
{
    Token *token = NULL;
    BlockStatements *stmt = (BlockStatements *)alloc(sizeof(BlockStatements));
    stmt->innerStatements = list();
    token = (Token *)(*node)->data;
    while (token->type != TOKEN_RIGHT_BRACE && token->type != TOKEN_ENDOFFILE)
    {
        list_push(stmt->innerStatements, declaration(node));
        token = (Token *)(*node)->data;
    }
    consume(node, TOKEN_RIGHT_BRACE, "Expect '}' after block.");
    return new_statement(STMT_BLOCK, (void *)stmt);
}
static Statement *if_statement(Node **node)
{
    Statement *thenStmt = NULL, *elseStmt = NULL;
    IfElseStatement *realStmt = NULL;
    Token *tkn = (Token *)(*node)->data;
    Expression *condition = NULL;
    consume(node, TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    condition = expression(node);
    consume(node, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");
    thenStmt = statement(node);
    tkn = (Token *)(*node)->data;
    if (MATCH(tkn->type, TOKEN_ELSE))
    {
        (*node) = (*node)->next;
        elseStmt = statement(node);
    }
    realStmt = (IfElseStatement *)alloc(sizeof(IfElseStatement));
    realStmt->condition = condition;
    realStmt->elseStmt = elseStmt;
    realStmt->thenStmt = thenStmt;
    return new_statement(STMT_IF_ELSE, realStmt);
}

static Statement *for_statement(Node **node)
{
    Statement *initializer = NULL, *body = NULL;
    Expression *condition = NULL, *step = NULL;
    BlockStatements *wrappedBody = NULL, *wrappedForAndInit = NULL;
    WhileStatement *wrappedFor = NULL;
    ExpressionStatement *wrappedStep = NULL;
    Token *tkn = NULL;
    consume(node, TOKEN_LEFT_PAREN, "Expect '(' after for");
    tkn = (Token *)(*node)->data;
    if (MATCH(tkn->type, TOKEN_SEMICOLON))
    {
        (*node) = (*node)->next;
    }
    else
    {
        if (MATCH(tkn->type, TOKEN_VAR))
        {
            (*node) = (*node)->next;
            initializer = var_declaration(node);
        }
        else
        {
            (*node) = (*node)->next;
            initializer = expression_statement(node);
        }
    }
    tkn = (Token *)(*node)->data;
    if (!MATCH(tkn->type, TOKEN_SEMICOLON))
    {
        condition = expression(node);
    }
    consume(node, TOKEN_SEMICOLON, "Expect ';' after for condition");
    if (!MATCH(tkn->type, TOKEN_RIGHT_PAREN))
    {
        step = expression(node);
    }
    consume(node, TOKEN_RIGHT_PAREN, "Expect ')' for 'for' closing");
    body = statement(node);
    if (step != NULL)
    {
        wrappedBody = malloc(sizeof(BlockStatements));
        wrappedBody->innerStatements = list();
        wrappedStep = alloc(sizeof(ExpressionStatement));
        wrappedStep->expr = step;
        list_push(wrappedBody->innerStatements, body);
        list_push(wrappedBody->innerStatements, new_statement(STMT_EXPR, wrappedStep));
        body = new_statement(STMT_BLOCK, wrappedBody);
    }

    if (condition == NULL)
    {
        condition = new_expr(EXPR_LITERAL, new_true());
    }
    wrappedFor = alloc(sizeof(WhileStatement));
    wrappedFor->condition = condition;
    wrappedFor->body = body;
    body = new_statement(STMT_WHILE, wrappedFor);
    if (initializer != NULL)
    {
        wrappedForAndInit = alloc(sizeof(BlockStatements));
        wrappedForAndInit->innerStatements = list();
        list_push(wrappedForAndInit->innerStatements, initializer);
        list_push(wrappedForAndInit->innerStatements, body);
        body = new_statement(STMT_BLOCK, wrappedForAndInit);
    }
    return body;
}

static Statement *while_statement(Node **node)
{
    WhileStatement *realStmt = NULL;
    Expression *condition = NULL;
    Statement *bodyStmt = NULL;
    consume(node, TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    condition = expression(node);
    consume(node, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");
    bodyStmt = statement(node);
    realStmt = (WhileStatement *)alloc(sizeof(WhileStatement));
    realStmt->condition = condition;
    realStmt->body = bodyStmt;
    return new_statement(STMT_WHILE, realStmt);
}

static Statement *fun_statement(const char *kind, Node **node)
{
    Token *name = NULL, *tkn = NULL;
    Node **temp = NULL;
    List *params = NULL;
    Statement *body = NULL;
    FunStatement *fnStmt = NULL;
    char buf[LINEBUFSIZE];
    memset(buf, 0, LINEBUFSIZE);
    sprintf(buf, "Expect %s name.", kind);
    temp = consume(node, TOKEN_IDENTIFIER, buf);
    if (temp != NULL)
    {

        name = (Token *)(*temp)->data;
        memset(buf, 0, LINEBUFSIZE);
        sprintf(buf, "Expect '(' after %s name.", kind);
        consume(node, TOKEN_LEFT_PAREN, buf);
        params = list();
        tkn = (Token *)(*node)->data;
        if (!MATCH(tkn->type, TOKEN_RIGHT_PAREN))
        {
            do
            {
                if (params->count > MAX_ARGS)
                {
                    parse_error(tkn, "Cannot have more than 8 parameters.");
                }
                temp = consume(node, TOKEN_IDENTIFIER, "Expect parameter name.");
                tkn = (Token *)(*temp)->data;
                list_push(params, tkn);
                tkn = (Token *)(*node)->data;
                if (!MATCH(tkn->type, TOKEN_RIGHT_PAREN))
                {
                    (*node) = (*node)->next;
                }
            } while (MATCH(tkn->type, TOKEN_COMMA));
        }
        consume(node, TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
        memset(buf, 0, LINEBUFSIZE);
        sprintf(buf, "Expect '{' before %s body.", kind);
        consume(node, TOKEN_LEFT_BRACE, buf);
        body = block_statements(node);
        fnStmt = alloc(sizeof(FunStatement));
        fnStmt->name = *name;
        fnStmt->body = body;
        fnStmt->args = params;
        return new_statement(STMT_METHOD, fnStmt);
    }
    return NULL;
}
static void literal_destroy(LiteralExpression *expr)
{
    switch (expr->type)
    {
    case BOOL:
    case LITERAL_NUMBER:
        free(expr->value);
        break;
    case LITERAL_NOTHING:
        break;
    case LITERAL_STRING:
        break;
    }
    free(expr);
}

static void expr_destroy(Expression *expr)
{
    Expression *expr = NULL;
    SetExpression *set = NULL;
    GetExpression *get = NULL;
    LiteralExpression *literal = NULL;
    switch (expr->type)
    {
    case EXPR_LITERAL:
        literal = (LiteralExpression *)expr->expr;
        literal_destroy(literal);
        break;
    case EXPR_UNARY:
        expr = ((UnaryExpression *)expr->expr)->expr;
        exprDestroy(expr);
        break;
    case EXPR_BINARY:
        expr = ((BinaryExpression *)expr->expr)->leftExpr;
        expr_destroy(expr);
        expr = ((BinaryExpression *)expr->expr)->rightExpr;
        expr_destroy(expr);
        break;
    case EXPR_GROUPING:
        expr = ((GroupingExpression *)expr->expr)->expr;
        expr_destroy(expr);
        break;
    case EXPR_ASSIGNMENT:
        expr = ((AssignmentExpr *)expr->expr)->rightExpr;
        expr_destroy(expr);
        break;
    case EXPR_LOGICAL:
        expr = ((LogicalExpr *)expr->expr)->left;
        expr_destroy(expr);
        expr = ((LogicalExpr *)expr->expr)->right;
        expr_destroy(expr);
        break;
    case EXPR_CALL:
        expr = ((CallExpression *)expr->expr)->callee;
        expr_destroy(expr);
        list_destroy(((CallExpression *)expr->expr)->args);
        break;
        break;
        break;
    case EXPR_SET:
        set = (SetExpression *)expr->expr;
        expr_destroy(set->object);
        expr_destroy(set->value);
        break;
    case EXPR_GET:
        get = (GetExpression *)expr->expr;
        expr_destroy(get->object);
        break;
    case EXPR_VARIABLE:
    default:
        break;
    }
    free(expr);
}
static void stmts_foreach_stmt(List *stmts, void *stmtObj)
{
    Statement *stmt = (Statement *)stmtObj;
    statementDestroy(stmt);
}
static void statementDestroy(Statement *stmt)
{
    IfElseStatement *ifStmt = NULL;
    WhileStatement *whileStmt = NULL;
    FunStatement *funStmt = NULL;
    if (stmt != NULL)
    {
        switch (stmt->type)
        {
        case STMT_BLOCK:
            list_foreach(((BlockStatements *)stmt->statement)->innerStatements, stmts_foreach_stmt);
            break;
        case STMT_PRINT:
            expr_destroy(((DisplayStatement *)stmt->statement)->expr);
            break;
        case STMT_EXPR:
            expr_destroy(((ExpressionStatement *)stmt->statement)->expr);
            break;
        case STMT_VAR_DECLARATION:
            expressionDestroy(((VarDeclarationStmt *)stmt->statement)->initializer);
            break;
        case STMT_IF_ELSE:
            ifStmt = (IfElseStatement *)stmt->statement;
            statementDestroy(ifStmt->thenStmt);
            statementDestroy(ifStmt->elseStmt);
            expr_destroy(ifStmt->condition);
            break;
        case STMT_WHILE:
            whileStmt = (WhileStatement *)stmt->statement;
            statementDestroy(whileStmt->body);
            expr_destroy(whileStmt->condition);
            break;
        case STMT_METHOD:
            funStmt = (MethodStatement *)stmt->statement;
            list_destroy(funStmt->args);
            statementDestroy(funStmt->body);
            break;
        case STMT_GIVE:
            expr_destroy(((GiveStatement *)stmt->statement)->value);
            break;

        default:
            break;
        }
        free((void *)stmt);
    }
}
static Statement *return_statement(Node **node)
{
    Token *keyword = (Token *)(*node)->prev->data, *tkn = (Token *)(*node)->data;
    Expression *value = NULL;
    GiveStatement *returnStmt = NULL;
    if (!MATCH(tkn->type, TOKEN_SEMICOLON))
    {
        value = expression(node);
    }
    consume(node, TOKEN_SEMICOLON, "Expect ';' after return");
    returnStmt = (GiveStatement *)malloc(sizeof(GiveStatement));
    returnStmt->keyword = *keyword;
    returnStmt->value = value;
    return new_statement(STMT_GIVE, returnStmt);
}
static void stmts_destroy(List *stmts)
{
    if (stmts->count != 0)
    {
        list_foreach(stmts, stmts_foreach_stmt);
    }
    list_destroy(stmts);
}
void parser_destroy(ParsingContext *ctx)
{
    if (ctx->stmts != NULL)
    {
        stmts_destroy(ctx->stmts);
        ctx->stmts = NULL;
    }

    if (ctx->expr != NULL)
    {
        expr_destroy(ctx->expr);
        ctx->expr = NULL;
    }
}

ParsingContext parse(Tokenization toknz)
{
    ParsingContext ctx = {NULL, NULL};
    List *stmts = NULL;
    List *tokens = toknz.values;
    int nbTokens = 0;
    Node *head = NULL;
    Statement *stmt = NULL;

    if (tokens != NULL)
    {
        stmts = list();
        nbTokens = tokens->count;
        head = tokens->head;

        while (!END_OF_TOKENS(((Token *)head->data)->type))
        {
            stmt = declaration(&head);
            if (stmt != NULL)
            {
                list_push(stmts, stmt);
            }
            else
            {
                stmts_destroy(stmts);
                stmts = NULL;
                break;
            }
        }
    }
    ctx.stmts = stmts;
    return ctx;
}

void parse_error(Token *token, const char *msg, ...)
{
    char buff[LINEBUFSIZE];
    va_list list;
    va_start(list, msg);

    memset(buff, 0, sizeof(buff));

    if (token->type == TOKEN_ENDOFFILE)
    {
        fprintf(stderr, ERROR_AT_EOF, msg);
    }
    else
    {
        sprintf(buff, ERROR_AT_LINE, token->line, msg, token->lexeme);
        vfprintf(stderr, msg, list);
    }
    va_end(list);
}
