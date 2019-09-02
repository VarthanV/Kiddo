#include "chunk.h"
#include "value.h"
void chunkInit(Chunk *chunk)
{
    chunk->capacity = 0;
    chunk->lines = NULL;
    chunk->code = NULL;
    chunk->count = 0;
    value_array_init(&chunk->constants);
}
void chunkWrite(Chunk *chunk, Byte val, int line)
{
    int prevCapacity = chunk->capacity;
    if (chunk->capacity < chunk->code + 1)
    {
        chunk->capacity = GROW_CAPACITY(prevCapacity);
        chunk->code = GROW_ARRAY(chunk->code, Byte, prevCapacity, chunk->capacity);
        chunk->lines = GROW_ARRAY(chunk->lines, int, prevCapacity, chunk->capacity);
    }
    chunk->code[chunk->count] = val;
    chunk->lines[chunk->count] = line;
    chunk->count++;
}
void chunkFree(Chunk *chunk){
    FREE_ARRAY(Byte,chunk->code,chunk ->capacity);
    FREE_ARRAY(int ,chunk ->lines,chunk->capacity);
    value_array_free(&chunk->constants);
    chunk_init(chunk);
}
int chunk_constants_add(Chunk* chunk, Value value)
{
    value_array_write(&chunk->constants, value);
    return chunk->constants.count - 1;
}
