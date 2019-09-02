#include "vm.h"
#include "debug.h"
#include "compiler.h"
#include "table.h"
#include "value.h"
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <stdio.h>
static void runtime_error(const char *format, ...);
static VmBoolean is_falsey(Value value);
VM vm;
static void vm_stack_reset()
{
    vm.stackTop = vm.stack;
    vm.frameCount = 0;
}
static void vm_stack_push(Value value)
{
    *vm.stackTop = value;
    vm.stackTop++;
}
static Value vm_stack_pop()
{
    return *--vm.stackTop;
}
static Value vm_stack_peek(int distance)
{
    return vm.stackTop[-1 - distance];
}

static void runtime_error(const char *format, ...)
{

    va_list args;
    VmFunction *function = NULL;
    CallFrame *frame = NULL;
    size_t instruction;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);
    for (int i = vm.frameCount - 1; i >= 0; i--)
    {
        frame = &vm.frames[i];
        function = frame->function;
        // -1 because the IP is sitting on the next instruction to be
        // executed.
        instruction = frame->ip - function->chunk.code - 1;
        fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);
        if (function->name == NULL)
        {
            fprintf(stderr, "script\n");
        }
        else
        {
            fprintf(stderr, "%s()\n", function->name->chars);
        }
    }
    vm_stack_reset();
}

static VmBoolean is_falsey(Value value)
{
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void vmstring_concatenate()
{
    VmString *b = AS_STRING(vm_stack_pop());
    VmString *a = AS_STRING(vm_stack_pop());
    VmString *result = NULL;
    size_t length = a->length + b->length;
    char *chars = (char *)alloc(length + 1);

    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = 0;

    result = vmstring_take(chars, length);
    vm_stack_push(object_val((VmObject *)result));
}
static int call(VmFunction *function, int argCount)
{
    if (argCount != function->arity)
    {
        runtime_error("EXPECTED %d ARGUMENTS BUT GOT %d.", function->arity, argCount);
        return 0;
    }

    if (vm.frameCount == FRAMES_MAX)
    {
        runtime_error("Stack overflow.");
        return 0;
    }

    CallFrame *frame = &vm.frames[vm.frameCount++];
    frame->function = function;
    frame->ip = function->chunk.code;

    frame->slots = vm.stackTop - argCount - 1;
    return 1;
}