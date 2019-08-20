#include "parser.h"

int resolve(Statement* stmt);

typedef enum function_type_t {
    FUNCTION_TYPE_NONE,
    FUNCTION_TYPE_FUNCTION,
    FUNCTION_TYPE_METHOD,
    FUNCTION_TYPE_CTOR
} FunctionType;