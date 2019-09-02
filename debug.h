#ifndef CLOX_DEBUG
#define CLOX_DEBUG

#include "chunk.h"

void chunk_disassemble(Chunk* chunk, const char* name);
int chunk_disassemble_instruction(Chunk* chunk, int offset);

#endif