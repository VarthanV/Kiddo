#include "value.h"
#include "memory.h"
#include "vm.h"
#include <stdio.h>
#include <string.h>

Value bool_val(VmBoolean boolean)
{
    Value v;
    v.type = VAL_BOOL;
    v.as.boolean = boolean;
    return v;
}
Value nil_val()
{
    Value v;
    v.type = VAL_NIL;
    v.as.number = 0;
    return v;
}
Value number_val(VmNumber number)
{
    Value v;
    v.type = VAL_NUMBER;
    v.as.number = number;
    return v;
}
void value_array_init(ValueArray *array)
{
    array->count = 0;
    array->capacity = 0;
    array->values = NULL;
}
void value_array_free(ValueArray *array)
{
    FREE_ARRAY(Value, array->values, array->capacity);
    value_array_init(array);
}

int object_equal(Value a, Value b)
{
    VmString *s1 = NULL, *s2 = NULL;
    if (AS_OBJECT(a)->type != AS_OBJECT(b)->type)
    {
        return 0;
    }

    switch (AS_OBJECT(a)->type)
    {
    case OBJECT_STRING:
        s1 = AS_STRING(a);
        s2 = AS_STRING(b);
        return s1 == s2;
    }

    return 0;
}

int values_equal(Value a, Value b)
{
    if (a.type != b.type)
    {
        return 0;
    }

    switch (a.type)
    {
    case VAL_BOOL:
        return AS_BOOL(a) == AS_BOOL(b);
    case VAL_NUMBER:
        return AS_NUMBER(a) == AS_NUMBER(b);
    case VAL_NIL:
        return 1;
    case VAL_OBJECT:
        return object_equal(a, b);
    }

    return 0;
}

static void object_print(Value value)
{
    VmObject *object = AS_OBJECT(value);

    switch (object->type)
    {
    case OBJECT_STRING:
        printf("%s", AS_CSTRING(value));
        break;
    case OBJECT_FUNCTION:
        if (AS_FUNCTION(value)->name != NULL)
        {
            printf("<fn %s>", AS_FUNCTION(value)->name->chars);
        }
        else
        {
            printf("<script>");
        }
        break;
    case OBJECT_NATIVE:
        printf("<native fn>");
        break;
    }
}

void value_print(Value value)
{
    switch (value.type)
    {
    case VAL_NUMBER:
        printf("%g", AS_NUMBER(value));
        break;
    case VAL_BOOL:
        printf(AS_BOOL(value) ? "true" : "false");
        break;
    case VAL_NIL:
        printf("nil");
        break;
    case VAL_OBJECT:
        object_print(value);
        break;
    }
}

static void vmstring_free(VmString *string)
{
    FREE_ARRAY(char, string->chars, string->length + 1);
    FREE(VmString, string);
}

static void object_free(VmObject *object)
{
    VmString *string = NULL;
    VmFunction *function = NULL;
    switch (object->type)
    {
    case OBJECT_STRING:
        string = (VmString *)object;
        vmstring_free(string);
        break;
    case OBJECT_FUNCTION:
        function = (VmFunction *)object;
        chunk_free(&function->chunk);
        //vmstring_free(function->name);
        FREE(VmFunction, function);
        break;
    case OBJECT_NATIVE:
        FREE(VmNative, object);
        break;
    }
}

void objects_free()
{
    VmObject *object = vm.objects, *next = NULL;
    while (object != NULL)
    {
        next = object->next;
        object_free(object);
        object = next;
    }
}