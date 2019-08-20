#define VISITOR_H
#include "parser.h"

typedef void* (*ActionExpr)(Expression*);
typedef void* (*ActionStmt)(Statement*stmt);

typedef struct expression_visitor_t {
    ActionExpr visitBinary;
    ActionExpr visitUnary;
    ActionExpr visitLiteral;
    ActionExpr visitGrouping;
    ActionExpr visitVariable;
    ActionExpr visitAssignment;
    ActionExpr visitLogical;
    ActionExpr visitCallable;
    ActionExpr visitGet;
    ActionExpr visitSet;
} ExpressionVisitor;

typedef struct stmt_visitor_t {
    ActionStmt visitPrint;
    ActionStmt visitVarDeclaration;
    ActionStmt visitExpression;
    ActionStmt visitBlock;
    ActionStmt visitIfElse;
    ActionStmt visitWhile;
    ActionStmt visitFun;
    ActionStmt visitReturn;
    ActionStmt visitClass;
} StatementVisitor;

void* accept(StatementVisitor visitor, Statement* stmt);
void* accept_expr(ExpressionVisitor visitor, Expression* expr);


