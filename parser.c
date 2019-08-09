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
static Expression *comparision(Node **);
static Expression *addition(Node **node);
static Expression *multiplication(Node **node);
static Expression *call(Node **node);
static Expression *primary(Node **node);
static Expression *logicalOr(Node **node);
static Expression *logicalAnd(Node **node);

static Statement *blockStatements(Node **node);
static Statement *forStatements(Node **node);
static Statement *whileStatements(Node **node);
static Statement *methodStatements(const char *type, Node **node);
static Statement *giveStatements(Node **node);

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

static Node ** consume(Node ** node,  TokenType type,const char * msg){
Token *tkn =(Token *)(*node) ->data;
if(DOMATCH(tkn -> type ,type)){
    (*node) = (*node) -> next;
    return &(*node) -> prev;
}
parseError(tkn,msg);
return NULL;
}

static Expression * newExpression(ExpressionType type,const char * realExp){
  Expression * expr=(Expression *) malloc(sizeof(Expression));
  expr ->type =type;
  expr -> order =0;
  expr ->expr =realExp;
  return expr;
}
static LiteralExpression * newLiteral(void * value,LiteralType type,   size_t size){
    LiteralExpression * expr =(LiteralExpression *) malloc(sizeof(LiteralExpression));
    expr -> value = value;
    expr -> valueSize =size;
    expr -> type =type;
    return expr;
} 
static BinaryExpression * newBinary(Token op,Expression *left,Expression * right){
    BinaryExpression *expression =(BinaryExpression *) malloc(sizeof(BinaryExpression));
    expression -> leftExpr = (Expression *) left;
    expression -> rightExpr =(Expression *) right;
    expression -> op =op;
    return expression;

}
static  GroupingExpression *  newGrouping(Expression * internalExpr){
GroupingExpression * expr =(GroupingExpression *) malloc(sizeof(GroupingExpression));
expr -> expr =(Expression *) internalExpr;
return expr;
}
static VariableExpr*  newVariable(Token variableName){
    VariableExpr * expr =(VariableExpr *) malloc(sizeof(VariableExpr));
    expr -> variableName =variableName;
    return expr;
}
static AssignmentExpr * newAssignement(){
    
}