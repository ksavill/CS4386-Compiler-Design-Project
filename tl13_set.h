#ifndef TL13_SET_H
#define TL13_SET_H

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

typedef enum {
    TYPE_INT,
    TYPE_BOOL
} TypeEnum;

typedef struct Program {
    struct DeclarationList* declarations;
    struct StatementList* statements;
} Program;

typedef struct DeclarationList {
    struct Declaration* declaration;
    struct DeclarationList* next;
} DeclarationList;

typedef struct Declaration {
    char* identifier;
    struct Type* type;
} Declaration;

typedef struct Type {
    TypeEnum typeEnum;
} Type;

typedef struct StatementList {
    struct Statement* statement;
    struct StatementList* next;
} StatementList;

typedef enum {
    STMT_ASSIGNMENT,
    STMT_IF,
    STMT_WHILE,
    STMT_WRITEINT,
    STMT_READINT
} StatementType;

typedef struct Statement {
    StatementType type;
    union {
        struct { char* identifier; struct Expression* expression; } assignment;
        struct { struct Expression* condition; struct StatementList* thenBranch; struct StatementList* elseBranch; } ifStmt;
        struct { struct Expression* condition; struct StatementList* body; } whileStmt;
        struct { struct Expression* expression; } writeInt;
    };
} Statement;

typedef enum {
    EXPR_IDENT,
    EXPR_NUM,
    EXPR_BOOLLIT,
    EXPR_OP,
    EXPR_PAREN,
    EXPR_READINT
} ExpressionType;

typedef enum {
    OP_ADD, OP_SUBTRACT, OP_MULTIPLY, OP_DIVIDE, OP_MOD, OP_EQUAL,
    OP_NOTEQUAL, OP_LESS, OP_LESSEQ, OP_GREATER, OP_GREATEREQ
} Operator;

typedef struct Expression {
    ExpressionType type;
    union {
        char* ident;
        int num;
        bool boollit;
        struct { Operator op; struct Expression* left; struct Expression* right; } operation;
        struct { struct Expression* expression; } parenExpr;
    };
} Expression;

typedef struct SymbolTableEntry {
    char* identifier;
    TypeEnum type;
    struct SymbolTableEntry* next;
} SymbolTableEntry;

typedef struct SymbolTable {
    struct SymbolTableEntry** buckets;
    int size;
    struct SymbolTable* prev;
} SymbolTable;

struct SymbolTable* createSymbolTable(int size);
void destroySymbolTable(struct SymbolTable* table);
struct SymbolTableEntry* symbolTableInsert(struct SymbolTable* table, const char* identifier, TypeEnum type);
struct SymbolTableEntry* symbolTableLookup(struct SymbolTable* table, const char* identifier);
void symbolTableRemove(struct SymbolTable* table, const char* identifier);
void pushScope(struct SymbolTable** currentScope);
void popScope(struct SymbolTable** currentScope);

void pushSymbolTable();
void popSymbolTable();
struct SymbolTableEntry* findSymbol(const char* id);

#endif // TL13_SET_H