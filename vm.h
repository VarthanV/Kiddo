
#define CLOX_VM

#include "chunk.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * BYTE_COUNT)

typedef struct call_frame {
    VmFunction* function;
    Byte* ip;
    Value* slots;
} CallFrame;

typedef struct vm {
    CallFrame frames[FRAMES_MAX];
    int frameCount;
    Value stack[STACK_MAX];
    Value* stackTop;
    Table strings;
    Table globals;
    VmObject* objects;
} VM;

extern VM vm;

typedef enum vm_interpret_result {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} VmInterpretResult;

void vm_init();
void vm_free();
VmInterpretResult vm_interpret(const char* code);

