
#define LIST_H

typedef struct node {
    void* data;
    struct node* next;
    struct node* prev;
} Node;

typedef struct list {
    Node* head;
    Node* last;
    unsigned int count;
} List;

typedef void (*Iterator)(List* list, void* data);

typedef int (*Predicate)(Node* n);

List* list();
Node* listPush(List* list, void* data);
Node* listInsert(List* list, void* data, unsigned int index);
int listRemove(List* list, Node* n);
int list_remove_at(List* list, unsigned int index);
void listClear(List* list);
Node* listLast(List* list);
Node* listAt(List* list, unsigned int index);
void listForeach(List* list, Iterator iter);
void listDestroy(List* list);
int listAny(List* list, Predicate pred);
void* listPop(List* list);
