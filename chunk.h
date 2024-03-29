#define CLOX_CHUNK

#include "common.h"
#include "memory.h"
#include "value.h"

#include <stdlib.h>

typedef enum opcode {
    OP_RETURN,
    OP_CONSTANT,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_NOT,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_NEGATE,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_PRINT,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_LOOP,
    OP_POP,
    OP_DEFINE_GLOBAL,
    OP_GET_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_CALL
} OpCode;

void chunk_init(Chunk* chunk);

void chunk_write(Chunk* chunk, Byte value, int line);

void chunk_free(Chunk* chunk);

int chunk_constants_add(Chunk* chunk, Value value);
