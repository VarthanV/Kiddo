
// ! Hash Table Implementation 

#define CAPACITY 16
#include <stdio.h>
typedef struct key_value_pair_t {
    char* key;
    void* value;
    struct key_value_pair_t* next;
} KeyValuePair;

typedef int (*DictAction)(KeyValuePair* pair);

typedef struct dict_t {
    KeyValuePair* buckets[ CAPACITY];
    int capacity;
    int count;
    DictAction DeleteValue;
} Dictionary;

Dictionary* dict(DictAction deleteValue);

int dict_add(Dictionary* dict, const char* key, void* value);
int dict_remove(Dictionary* dict, const char* key);
void* dict_get(Dictionary* dict, const char* key);
void dict_destroy(Dictionary* dict);
int dict_contains(Dictionary* dict, const char* key);
int dict_set(Dictionary* dict, const char* key, void* value);
