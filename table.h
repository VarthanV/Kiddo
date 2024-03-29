
#define CLOX_TABLE

#include "vmcommon.h"
#include "value.h"
#include <stdio.h>

#define TABLE_MAX_LOAD 0.75

typedef struct table_entry {
    VmString* key;
    Value value;
} Entry;

typedef struct table {
    int count;
    int capacity;
    Entry* entries;
} Table;

void table_init(Table* table);
void table_free(Table* table);
int table_set(Table* table, VmString* key, Value value);
void table_add_all(Table* from, Table* to);
int table_get(Table* table, VmString* key, Value* value);
int table_delete(Table* table, VmString* key);
VmString* table_find_string(Table* table, const char* chars, size_t length, Hash hash);

