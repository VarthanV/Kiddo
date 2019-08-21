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

#define LINEBUFFSIZE 1024 * sizeof(char)
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
static Object *new_bool(int truthy)
{
	char *value = (char *)alloc(sizeof(char));
	*value = (char)truthy;
	return obj_new(OBJECT_BOOL, value, 1);
}

static Object *eval_expr(Expression *expr)
{
	return (Object *)accept_expr(EvaluvateVisitor, expr);
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
	const BinaryExpression *bExpr = (BinaryExpression *)(expr->expr);
	Object *rObject = eval_expr(bExpr->rightExpr);
	Object *lObject = eval_expr(bExpr->leftExpr);
	Object *result = NULL;
	double *lValue = NULL, *rValue = NULL;
	char *sValue = NULL;
	size_t lengthRight = 0, lengthLeft = 0;
	if (rObject == NULL || lObject == NULL)
		return NULL;
	switch (bExpr->op.type)
	{
	case TOKEN_MINUS:
		if (lObject->type == OBJECT_NUMBER && rObject->type == OBJECT_NUMBER)
		{
			result = new_number(*((double *)lObject->value) - *((double *)rObject->value));
		}
		else
		{
			runtime_error(OPERAND_NUMBER, &result, bExpr->op.line);
		}
		break;
	case TOKEN_PLUS:
		if (rObject->type == OBJECT_NUMBER && lObject->type == OBJECT_NUMBER)
		{
			result = new_number(*((double *)lObject->value) + *((double *)rObject->value));
		}
		else if (rObject->type == OBJECT_STRING && lObject->type == OBJECT_STRING)
		{
			lengthLeft = strlen((char *)lObject->value);
			lengthRight = strlen((char *)rObject->value);
			result->type = OBJECT_STRING;
			sValue = (char *)malloc(lengthLeft + lengthRight + 1);
			memcpy(sValue, lObject->value, lengthLeft);
			memcpy(sValue + lengthLeft, rObject->value, lengthRight + 1);
			result->value = sValue;
		}
		else
		{
			runtime_error(OPERAND_SAMETYPE, &result, bExpr->op.line);
		}
		break;
	case TOKEN_SLASH:
		if (rObject->type == OBJECT_NUMBER && lObject->type == OBJECT_NUMBER)
		{
			result = new_number(*(double *)lObject->value / *(double *)rObject->value);
		}
		else
		{
			runtime_error(OPERAND_NUMBER, &result, bExpr->op.line);
		}
	case TOKEN_STAR:
		if (rObject->type == OBJECT_NUMBER && lObject->type == OBJECT_NUMBER)
		{
			result = new_number(*(double *)lObject->value * *(double *)rObject->value);
		}
		else
		{
			runtime_error(OPERAND_NUMBER, &result, bExpr->op.line);
		}
	case TOKEN_GREATER:
		if (rObject->type == OBJECT_NUMBER && lObject->type == OBJECT_NUMBER)
		{
			lValue = (double *)lObject->value;
			rValue = (double *)rObject->value;
			result = new_bool(*lValue > *rValue);
		}
		else
		{
			runtime_error(OPERAND_NUMBER, &result, bExpr->op.line);
		}
		break;
	case TOKEN_GREATER_EQUAL:
		if (rObject->type == OBJECT_NUMBER && lObject->type == OBJECT_NUMBER)
		{
			lValue = (double *)lObject->value;
			rValue = (double *)rObject->value;
			result = new_bool(*lValue >= *rValue);
		}
		else
		{
			runtime_error(OPERAND_NUMBER, &result, bExpr->op.line);
		}
		break;
	case TOKEN_LESS:
		if (rObject->type == OBJECT_NUMBER && lObject->type == OBJECT_NUMBER)
		{
			lValue = (double *)lObject->value;
			rValue = (double *)rObject->value;
			result = new_bool(*lValue < *rValue);
		}
		else
		{
			runtime_error(OPERAND_NUMBER, &result, bExpr->op.line);
		}
		break;
	case TOKEN_LESS_EQUAL:
		if (rObject->type == OBJECT_NUMBER && lObject->type == OBJECT_NUMBER)
		{
			lValue = (double *)lObject->value;
			rValue = (double *)rObject->value;
			result = new_bool(*lValue <= *rValue);
		}
		else
		{
			runtime_error(OPERAND_NUMBER, &result, bExpr->op.line);
		}
		break;
	case TOKEN_EQUAL_EQUAL:
		if (rObject->type == OBJECT_NIL && lObject->type == OBJECT_NIL)
		{
			result->value = new_bool(1);
		}
		else if (rObject->type == OBJECT_NUMBER && lObject->type == OBJECT_NUMBER)
		{
			lValue = (double *)lObject->value;
			rValue = (double *)rObject->value;
			result = new_bool(*lValue == *rValue);
		}
		else if (rObject->type == OBJECT_STRING && lObject->type == OBJECT_STRING)
		{
			result = new_bool(strcmp((char *)rObject->value, (char *)lObject->value) == 0);
		}
		else if (rObject->type == OBJECT_BOOL && lObject->type == OBJECT_BOOL)
		{
			result = new_bool(((char *)rObject->value)[0] == ((char *)lObject->value)[0]);
		}
		else
		{
			result = new_bool(0);
		}
		break;
	case TOKEN_BANG_EQUAL:
		if (rObject->type == OBJECT_NIL && lObject->type == OBJECT_NIL)
		{
			result = new_bool(0);
		}
		else if (rObject->type == OBJECT_NUMBER && lObject->type == OBJECT_NUMBER)
		{
			lValue = (double *)lObject->value;
			rValue = (double *)rObject->value;
			result = new_bool(*lValue != *rValue);
		}
		else if (rObject->type == OBJECT_STRING && lObject->type == OBJECT_STRING)
		{
			result = new_bool(strcmp((char *)rObject->value, (char *)lObject->value) != 0);
		}
		else if (rObject->type == OBJECT_BOOL && lObject->type == OBJECT_BOOL)
		{
			result = new_bool(((char *)rObject->value)[0] != ((char *)lObject->value)[0]);
		}
		else
		{
			result = new_bool(1);
		}
		break;
	case TOKEN_AND:
	case TOKEN_OR:

	default:
		break;
	}
	obj_destroy(lObject);
	obj_destroy(rObject);
	return result;
}

void *visit_unary(Expression *expr)
{
	const UnaryExpression *uexpr = (UnaryExpression *)(expr->expr);
	Object *rObject = eval_expr(uexpr->expr);
	char *bValue = NULL;
	double *value = NULL;
	if (uexpr->op.type == TOKEN_BANG)
	{
		bValue = (char *)alloc(sizeof(char));
		*bValue = obj_unlikely(rObject);
		obj_destroy(rObject);
		rObject = obj_new(OBJECT_BOOL, bValue, sizeof(char));
	}
	else if (uexpr->op.type == TOKEN_MINUS)
	{
		if (rObject->type != OBJECT_NUMBER)
		{
			runtime_error(OPERAND_NUMBER, &rObject, uexpr->op.line);
		}
		else
		{
			value = (double *)clone(rObject->value, sizeof(double));
			*value = -*value;
			obj_destroy(rObject);
			rObject = obj_new(OBJECT_NUMBER, value, sizeof(double));
		}
	}
	return rObject;
}
void *visit_grouping(Expression *expr)
{
	const GroupingExpression *gexpr = (GroupingExpression *)(expr->expr);
	return eval_expr(gexpr->expr);
}

void *visit_literal(Expression *expr)
{
	LiteralExpression *original = (LiteralExpression *)(expr->expr);
	switch (original->type)
	{
	case LITERAL_STRING:
		return obj_new(OBJECT_STRING, original->value, original->valueSize);
	case LITERAL_NUMBER:
		return obj_new(OBJECT_NUMBER, original->value, original->valueSize);
	case LITERAL_NOTHING:
		return obj_new(OBJECT_NIL, NULL, 0);
	case BOOL:
		return obj_new(OBJECT_BOOL, original->value, original->valueSize);
	}
	return NULL;
}
static Object *lookup_var(int order, char *name)
{
	Object *value = NULL;
	if (order == -1)
	{
		value = env_get_variable_value(&GlobalExecutionEnvironment, name);
	}
	else
	{
		value = env_get_variable_value_at(CurrentEnv, order, name);
	}
	return value;
}
void *visit_var_expr(Expression *expr)
{
	VariableExpr *varExpr = (VariableExpr *)(expr->expr);
	Object *value = lookup_var(expr->order, varExpr->variableName.lexeme);
	if (value == NULL)
	{
		runtime_error("Variable name not declared '%s'", &value, varExpr->variableName.line, varExpr->variableName.lexeme);
	}
	return value;
}
void *visit_assign(Expression *expr)
{
	AssignmentExpr *assignExpr = (AssignmentExpr *)(expr->expr);
	Object *value = eval_expr(assignExpr->rightExpr);
	if (value == NULL)
	{
		runtime_error("Cannot assign  value to  variable  which is not declared'%s'", &value, assignExpr->variableName.line, assignExpr->variableName.lexeme);
	}
}
void *visit_logical(Expression *expr)
{
	LogicalExpr *logical = (LogicalExpr *)(expr->expr);
	Object *lvalue = eval_expr(logical->left);
	char lvalueTruth = obj_likely(lvalue);
	if (logical->op.type == TOKEN_OR)
	{
		if (lvalueTruth)
		{
			return lvalue;
		}
	}
	else if (logical->op.type == TOKEN_AND)
	{
		if (!lvalueTruth)
		{
			return lvalue;
		}
	}
	return eval_expr(logical->right);
}
void *visit_callable(Expression *expr)
{
	CallExpression *calleeExpr = (CallExpression *)(expr->expr);
	Object *callee = eval_expr(calleeExpr->callee);
	Callable *callable = NULL;
	List *args = list();
	Node *node = NULL;
	Expression *arg = NULL;
	Object *result = NULL;

	if (callee->type != OBJECT_CALLABLE)
	{
		list_destroy(args);
		return runtime_error("Can only call functions ", &callee, calleeExpr->paren.line);
	}

	if (calleeExpr->args->count != 0)
	{
		for (node = calleeExpr->args->head; node != NULL; node = node->next)
		{
			arg = (Expression *)node->data;
			list_push(args, eval_expr(arg));
		}
	}

	if (args->count != callable->arity)
	{
		list_destroy(args);
		return runtime_error("Expected %d but got %d arguments", &callee, calleeExpr->paren.line, args->count, callable->arity);
	}

	result = callable->call(args, callable->declaration, callable->closure, callable->type);

	obj_destroy(callee);
	list_destroy(args);
	return result;
}
void *visit_print(Statement *stmt)
{
	PrintStatement *printStmt = (PrintStatement *)(stmt->statement);
	Callable *call = NULL;
	Object *obj = eval_expr(printStmt->expr);
	double *value = NULL;
	char *bValue = NULL;
	switch (obj->type)
	{
	case OBJECT_NIL:
		printf("NOTHING\n");
		break;
	case OBJECT_STRING:
	case OBJECT_ERROR:
		printf("%s\n", (char *)obj->value);
		break;
	case OBJECT_BOOL:
		bValue = (char *)obj->value;
		printf("%s\n", *bValue == 1 ? TRUE_KEY : FALSE_KEY);
		break;
	case OBJECT_NUMBER:
		value = (double *)obj->value;
		if (*value != floor(*value))
		{
			printf("%lf\n", *value);
		}
		else
		{
			printf("%0.0lf\n", floor(*value));
		}
		break;
	case OBJECT_CALLABLE:
		call = (Callable *)obj->value;
		printf("<METHOD %s>\n", ((MethodStatement *)call->declaration)->name.lexeme);
		break;

	case OBJECT_VOID:
		break;
	default:
		break;
	}
	return obj;
}
void *visit_expr(Statement *stmt)
{
	ExpressionStatement *exprStmt = (ExpressionStatement *)(stmt->statement);
	return eval_expr(exprStmt->expr);
}

void *visit_var(Statement *stmt)
{
	VarDeclarationStmt *varDeclStmt = (VarDeclarationStmt *)(stmt->statement);
	Object *value = NULL;
	Token key = varDeclStmt->varName;
	if (varDeclStmt->initializer != NULL)
	{
		value = eval_expr(varDeclStmt->initializer);
	}
	if (!env_add_variable(CurrentEnv, key.lexeme, value))
	{
		runtime_error("'%s' is already declared", &value, key.line, key.lexeme);
	}

	return value;
}
static void obj_destroy_in_list(List *objs, void *obj)
{
	Object *o = (Object *)obj;
	obj_destroy(o);
}

static Object *execute_block(BlockStatements *stmt)
{
	Statement *innerStmt = NULL;
	Node *node = NULL;
	List *dump = NULL;
	Object *obj = NULL;
	int propagateReturn = 0;

	for (node = stmt->innerStatements->head; node != NULL; node = node->next)
	{
		innerStmt = (Statement *)node->data;
		if (innerStmt->type == STMT_GIVE)
		{
			obj = accept(EvaluateStmtVisitor, innerStmt);
		}
		else
		{
			if (dump == NULL)
			{
				dump = list();
			}
			obj = accept(EvaluateStmtVisitor, innerStmt);
			if (obj->propagateReturn)
			{
				break;
			}
			else
			{
				list_push(dump, obj);
			}
		}
	}
	propagateReturn = obj->propagateReturn;
	if (!obj->propagateReturn)
	{
		list_foreach(dump, obj_destroy_in_list);
	}
	list_destroy(dump);
	return propagateReturn ? obj : new_void();
}

void *visit_block(Statement *stmt)
{
	BlockStatements *blockStmt = (BlockStatements *)(stmt->statement);
	Object *returnValue = NULL;
	ExecutionEnvironment *prevEnv = CurrentEnv, *env = env_new();
	env->variables = NULL;
	env->enclosing = prevEnv;
	CurrentEnv = env;
	returnValue = execute_block(blockStmt);
	env_destroy(env);
	fr(env);
	CurrentEnv = prevEnv;
	return returnValue;
}

void *visit_ifElse(Statement *stmt)
{
	IfElseStatement *ifElseStmt = (IfElseStatement *)(stmt->statement);
	Object *eval_exprCond = eval_expr(ifElseStmt->condition);
	if (obj_likely(eval_exprCond))
	{
		return accept(EvaluateStmtVisitor, ifElseStmt->thenStmt);
	}
	else if (ifElseStmt->elseStmt != NULL)
	{
		return accept(EvaluateStmtVisitor, ifElseStmt->elseStmt);
	}
	return new_void();
}

void *visit_while(Statement *stmt)
{
	WhileStatement *whileStmt = (WhileStatement *)(stmt->statement);
	while (obj_likely(eval_expr(whileStmt->condition)))
	{
		accept(EvaluateStmtVisitor, whileStmt->body);
	}

	return new_void();
}

static Object *fun_call(List *args, void *declaration, ExecutionEnvironment *closure, FunctionType type)
{
	MethodStatement *funDecl = (MethodStatement *)declaration;
	Token *tkn = NULL;
	Node *node = NULL, *valueWrapper = NULL;
	int i = 0;
	Object *value = NULL;
	ExecutionEnvironment *prevEnv = CurrentEnv, *env = env_new();
	env->enclosing = closure;
	CurrentEnv = env;

	for (node = funDecl->args->head; node != NULL; node = node->next)
	{
		tkn = (Token *)node->data;
		valueWrapper = list_at(args, i);
		if (valueWrapper != NULL)
		{
			value = (Object *)valueWrapper->data;
			env_add_variable(CurrentEnv, tkn->lexeme, value);
		}
		i++;
	}

	value = execute_block((BlockStatements *)funDecl->body->statement);
	if (value->type == OBJECT_VOID)
	{
		if (type == FUNCTION_TYPE_CTOR)
		{
			obj_destroy(value);
			value = env_get_variable_value(CurrentEnv, "this");
		}
	}

	if (type == FUNCTION_TYPE_CTOR)
	{
		obj_destroy(value);
		value = env_get_variable_value(CurrentEnv, "this");
	}
	CurrentEnv = prevEnv;
	return value;
}

static Object *build_function(MethodStatement *funStmt, ExecutionEnvironment *closure, FunctionType type)
{
	Callable *call = (Callable *)alloc(sizeof(Callable));
	memset(call, 0, sizeof(Callable));
	call->call = fun_call;
	call->arity = funStmt->args->count;
	call->declaration = (void *)funStmt;
	call->closure = closure;
	call->type = type;
	return obj_new(OBJECT_CALLABLE, call, sizeof(Callable));
}

void *visit_fun(Statement *stmt)
{
	MethodStatement *funStmt = (MethodStatement *)(stmt->statement);
	Object *obj = NULL;
	obj = build_function(funStmt, CurrentEnv, FUNCTION_TYPE_FUNCTION);
	env_add_variable(CurrentEnv, funStmt->name.lexeme, obj);
	return new_void();
}

void *visit_return(Statement *stmt)
{
	Object *value = new_void();
	GiveStatement *returnStmt = (GiveStatement *)(stmt->statement);

	if (returnStmt->value != NULL)
	{
		obj_destroy(value);
		value = eval_expr(returnStmt->value);
	}
	value->propagateReturn = 1;
	return value;
}

int obj_force_destroy(KeyValuePair *pair)
{
	Object *obj = (Object *)pair->value;
	obj->shallow = 1;
	obj_destroy(obj);
	return 1;
}
static void callable_destroy(Callable *callable)
{
	if (callable->closure != NULL && GlobalExecutionEnvironment.variables != callable->closure->variables)
	{
		env_destroy(callable->closure);
		free(callable->closure);
	}
}
void obj_destroy(Object *obj)
{
	Callable *callable = NULL;
	Class *class = NULL;
	ClassInstance *instance = NULL;

	if (obj != NULL && obj->shallow == 1)
	{
		switch (obj->type)
		{
		case OBJECT_VOID:
		case OBJECT_NIL:
		case OBJECT_BOOL:
		case OBJECT_NUMBER:
		case OBJECT_STRING:
			break;
		case OBJECT_ERROR:
			free(obj->value);
			break;
		case OBJECT_CALLABLE:
			callable = (Callable *)obj->value;
			callable_destroy(callable);
			free(obj->value);
			break;
		default:
			runtime_error("Unknown Object to destroy", &obj, 0);
			break;
		}
		fr(obj);
	}
}
Object *obj_new(ObjectType type, void *value, size_t valueSize)
{
	Object *obj = (Object *)alloc(sizeof(Object));
	obj->type = type;
	obj->value = value;
	obj->valueSize = valueSize;
	obj->shallow = 1;
	obj->propagateReturn = 0;
	return obj;
}

char obj_likely(Object *obj)
{
	char *value = NULL;
	if (obj->type == OBJECT_NIL)
	{
		return (char)0;
	}

	if (obj->type == OBJECT_BOOL)
	{
		value = (char *)obj->value;
		return *value == (char)1;
	}

	return (char)1;
}

char obj_unlikely(Object *obj)
{
	char *value = NULL;
	if (obj->type == OBJECT_NIL)
	{
		return (char)0;
	}

	if (obj->type == OBJECT_BOOL)
	{
		value = (char *)obj->value;
		return *value != (char)1;
	}

	return (char)0;
}

void eval(Statement *stmt)
{
	accept(EvaluateStmtVisitor, stmt);
}
