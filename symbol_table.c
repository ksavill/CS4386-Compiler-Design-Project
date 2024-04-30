#include "tl13_set.h"

SymbolTable* symbolTable = NULL;

void pushSymbolTable() {
  SymbolTable* newTable = createSymbolTable(101);
  newTable->prev = symbolTable;
  symbolTable = newTable;
}

void popSymbolTable() {
  SymbolTable* oldTable = symbolTable;
  symbolTable = symbolTable->prev;
  destroySymbolTable(oldTable);
}

SymbolTableEntry* findSymbol(const char* id) {
  SymbolTable* current;
  for (current = symbolTable; current != NULL; current = current->prev) {
    SymbolTableEntry* entry = symbolTableLookup(current, id);
    if (entry != NULL) {
      return entry;
    }
  }
  return NULL;
}

SymbolTable* createSymbolTable(int size) {
    SymbolTable* table = (SymbolTable*) malloc(sizeof(SymbolTable));
    if (!table) return NULL;
    table->size = size;
    table->buckets = (SymbolTableEntry**) calloc(size, sizeof(SymbolTableEntry*));
    table->prev = NULL;
    return table;
}

void destroySymbolTable(SymbolTable* table) {
    if (!table) return;
    int i;
    for (i = 0; i < table->size; i++) {
        SymbolTableEntry* entry = table->buckets[i];
        while (entry) {
            SymbolTableEntry* temp = entry;
            entry = entry->next;
            free(temp->identifier);
            free(temp);
        }
    }
    free(table->buckets);
    free(table);
}

SymbolTableEntry* symbolTableLookup(SymbolTable* table, const char* identifier) {
    int index = hash(identifier, table->size);
    SymbolTableEntry* entry;
    for (entry = table->buckets[index]; entry != NULL; entry = entry->next) {
        if (strcmp(entry->identifier, identifier) == 0) {
            return entry;
        }
    }
    return NULL;
}

SymbolTableEntry* symbolTableInsert(SymbolTable* table, const char* identifier, TypeEnum type) {
    int index = hash(identifier, table->size);
    SymbolTableEntry* entry = (SymbolTableEntry*) malloc(sizeof(SymbolTableEntry));
    if (!entry) return NULL;
    entry->identifier = strdup(identifier);
    entry->type = type;
    entry->next = table->buckets[index];
    table->buckets[index] = entry;
    return entry;
}

int hash(const char* str, int size) {
    unsigned long hash = 5381;
    int c;
    while ((c = *str++))
        hash = ((hash << 5) + hash) + c;
    return hash % size;
}