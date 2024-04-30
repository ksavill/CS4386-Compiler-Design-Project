%error-verbose

%{
#include "tl13_set.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#define _XOPEN_SOURCE 700 // for strdup

Operator identifyOperator(char* op);
Type* make_type(TypeEnum te);
Expression* make_number_expression(int num);
Expression* make_operation_expression(Expression* left, Operator op, Expression* right);
Statement* make_if_statement(Expression* condition, StatementList* thenBranch, StatementList* elseBranch);
Statement* make_while_statement(Expression* condition, StatementList* body);
Statement* make_writeInt_statement(Expression* expr);

StatementList* make_statement_list(Statement* stmt);
StatementList* append_statement_list(StatementList* list, StatementList* newItem);
Expression* make_read_int_expression(void);
Expression* make_identifier_expression(char* id);
Statement* make_assignment_read_int(char* id, Expression* expr);
Statement* make_assignment_statement(char* id, Expression* expr);
Statement* make_read_int_statement(void);

char* expression_to_c_code(Expression* expr);
void generate_c_code_for_if(const char* condition, const char* then_code, const char* else_code);
void generate_c_code_for_while(const char* condition, const char* body_code);
void generate_c_code_for_writeInt(const char* expr_code);

char* statement_sequence_to_c_code(StatementList* stmtList);

extern int yylex(void);
void yyerror(const char* s);
extern char* yytext; // defined in analyzer.l

FILE *outputFile; // global file pointer for generated C code to go to.
extern SymbolTable* symbolTable; // symbol_table.c

char declarations[10240] = "";  // Buffer for variable declarations
char statements[10240] = "";    // Buffer for statements

void add_declaration(const char* text) {
    strcat(declarations, text);
    strcat(declarations, "\n");
}

void add_statement(const char* text) {
  printf("Debug add_statement: %s\n", text);
    strcat(statements, text);
    strcat(statements, "\n");
}
%}

%union {
  int ival;
  char *sval;
  struct Program* program;
  struct Declaration* declaration;
  struct DeclarationList* declList;
  struct Statement* stmt;
  struct StatementList* stmtList;
  struct Expression* expr;
  struct Type* type;
}

/* Token specifications with types */
%token <sval> IDENTIFIER
%token <ival> NUM BOOLLIT
%token <sval> OP2 OP3 OP4 // Changed to <sval> to pass strings
%token IF THEN ELSE PROGRAM_BEGIN END WHILE DO PROGRAM VAR AS INT BOOL WRITEINT READINT ASGN LP RP SC

/* Type specifications for nonterminals */
%type <program> program
%type <declList> declarations
%type <stmtList> statementSequence statementList
%type <stmt> statement assignment ifStatement whileStatement writeInt
%type <expr> expression readInt
%type <stmtList> elseClause
%type <type> type
%type <sval> ident

/* Operator precedence */
%right OP4
%left OP3  // Additive operators
%left OP2  // Multiplicative operators
%nonassoc LOWER_THAN_ELSE ELSE // Special handling for the 'else' keyword

%%

program:
    PROGRAM declarations PROGRAM_BEGIN statementSequence END {
        fprintf(outputFile, "// Generated C code\n");
        fprintf(outputFile, "#include <stdio.h>\n");
        fprintf(outputFile, "#include <stdlib.h>\n");
        fprintf(outputFile, "int main() {\n");
        fprintf(outputFile, "    // Declarations\n");
        fprintf(outputFile, "%s", declarations);  // Output all variable declarations
        fprintf(outputFile, "    // Statements\n");
        fprintf(outputFile, "%s", statements);    // Output all statements
        fprintf(outputFile, "    return 0;\n}\n");
        fprintf(stdout, "Parsing completed.\n");
    }
;

declarations:
    { $$ = NULL; }
    |
    VAR ident AS type SC declarations {
        if (findSymbol($2)) {
            yyerror("Variable redeclared");
            exit(1);
        } else {
            DeclarationList* newList = malloc(sizeof(DeclarationList));
            if (!newList) yyerror("Out of memory");
            newList->declaration = malloc(sizeof(Declaration));
            if (!newList->declaration) yyerror("Out of memory");
            newList->declaration->identifier = strdup($2);
            newList->declaration->type = $4;
            newList->next = $6;
            $$ = newList;
            symbolTableInsert(symbolTable, $2, $4->typeEnum); // Insert into the current scope's symbol table
            char decl[256];  // Temporary buffer for one declaration
            sprintf(decl, "    %s %s;\n", $4->typeEnum == TYPE_INT ? "int" : "bool", $2);
            add_declaration(decl);  // Append this declaration to the global declarations buffer
        }
    }
;

type:
    INT { Type* type = make_type(TYPE_INT); $$ = type; }
    |
    BOOL { Type* type = make_type(TYPE_BOOL); $$ = type; }
;

statementSequence:
    { $$ = NULL; }
    |
    statementList { $$ = $1; }
;

statementList:
    statementList statement SC { $$ = append_statement_list($1, make_statement_list($2)); }
    |
    statementList statement { $$ = append_statement_list($1, make_statement_list($2)); }
    |
    statement SC { $$ = make_statement_list($1); }
    |
    statement { $$ = make_statement_list($1); }
;

statement:
    assignment { $$ = $1; } // Commented out add_statement(expression_to_c_code($1->assignment.expression));
    |
    ifStatement { $$ = $1; }
    |
    whileStatement { $$ = $1; }
    |
    writeInt { $$ = $1; }
    |
    readInt { $$ = $1; }
;

readInt:
    READINT { $$ = make_read_int_expression(); add_statement("readInt()"); }
;

assignment:
    ident ASGN expression {
        $$ = make_assignment_statement($1, $3);
        char* expr_code = expression_to_c_code($3);
        char stmt[1024];
        sprintf(stmt, "    %s = %s;\n", $1, expr_code);
        printf("Debug Assignment: %s", stmt);  // Confirm assignment output
        add_statement(stmt);
        free(expr_code); // Clean up after use
    }
    |
    ident ASGN readInt {
        $$ = make_assignment_read_int($1, $3);
        char* read_expr_code = expression_to_c_code($3);
        char stmt[256];
        sprintf(stmt, "%s = %s;", $1, read_expr_code);
        add_statement(stmt);
        free(read_expr_code); // Clean up dynamically allocated memory
    }
;

ifStatement:
    IF expression THEN statementSequence elseClause END {
        Statement* stmt = make_if_statement($2, $4, $5);
        $$ = stmt;
        char* cond_code = expression_to_c_code($2);
        char* then_code = statement_sequence_to_c_code($4);
        char* else_code = ($5 != NULL) ? statement_sequence_to_c_code($5) : " "; // Use an empty string or handle appropriately
        generate_c_code_for_if(cond_code, then_code, else_code);
        free(cond_code);
        free(then_code);
        if (else_code != " ") free(else_code); // Only free if not the placeholder empty string
    }
;

elseClause:
    { $$ = NULL; }
    |
    ELSE statementSequence { $$ = $2; }
;

whileStatement:
    WHILE expression DO statementSequence END {
        Statement* stmt = make_while_statement($2, $4);
        $$ = stmt;
        char* cond_code = expression_to_c_code($2);
        char* body_code = statement_sequence_to_c_code($4);
        generate_c_code_for_while(cond_code, body_code);
        free(cond_code);
        free(body_code);
    }
;

writeInt:
    WRITEINT expression {
        Statement* stmt = make_writeInt_statement($2);
        $$ = stmt;
        char* expr_code = expression_to_c_code($2);
        generate_c_code_for_writeInt(expr_code);
        free(expr_code);
    }
;

expression:
    NUM {
        Expression* expr = make_number_expression($1);
        $$ = expr;
    }
    |
    IDENTIFIER {
        Expression* expr = make_identifier_expression($1);
        $$ = expr;
    }
    |
    expression OP2 expression {
        Operator op = identifyOperator($2); 
        Expression* expr = make_operation_expression($1, op, $3);
        $$ = expr;
    }
    |
    expression OP3 expression {
        Operator op = identifyOperator($2);
        Expression* expr = make_operation_expression($1, op, $3);
        $$ = expr;
    }
    |
    expression OP4 expression {
        Operator op = identifyOperator($2);
        Expression* expr = make_operation_expression($1, op, $3);
        $$ = expr;
    }
    |
    LP expression RP { $$ = $2; }
;

ident:
    IDENTIFIER { $$ = strdup($1); } // Duplicate the identifier string
;

%%

extern FILE* yyin; // Declare the external file pointer used by Flex

extern int yylineno;

int yydebug = 1; // Enable Bison debugging globally

StatementList* make_statement_list(Statement* stmt) {
    StatementList* list = (StatementList*)malloc(sizeof(StatementList));
    if (!list) yyerror("Out of memory");
    list->statement = stmt;
    list->next = NULL;
    return list;
}

StatementList* append_statement_list(StatementList* list, StatementList* newItem) {
    if (!list) return newItem;
    StatementList* tail = list;
    while (tail->next != NULL) tail = tail->next;
    tail->next = newItem;
    return list;
}

Expression* make_read_int_expression() {
    Expression* expr = (Expression*)malloc(sizeof(Expression));
    if (!expr) yyerror("Out of memory");
    expr->type = EXPR_READINT;
    return expr;
}

Expression* make_identifier_expression(char* id) {
    Expression* expr = malloc(sizeof(Expression));
    if (!expr) {
        yyerror("Out of memory");
        return NULL;
    }
    expr->type = EXPR_IDENT;
    expr->ident = strdup(id); // Duplicate the identifier for use in the expression
    return expr;
}

Statement* make_assignment_read_int(char* id, Expression* expr) {
    Statement* stmt = (Statement*)malloc(sizeof(Statement));
    if (!stmt) yyerror("Out of memory");
    stmt->type = STMT_ASSIGNMENT;
    stmt->assignment.identifier = strdup(id);
    stmt->assignment.expression = expr;
    return stmt;
}

Statement* make_read_int_statement() {
    Statement* stmt = (Statement*)malloc(sizeof(Statement));
    if (!stmt) yyerror("Out of memory");
    stmt->type = STMT_READINT;
    return stmt;
}

Statement* make_assignment_statement(char* id, Expression* expr) {
    Statement* stmt = (Statement*)malloc(sizeof(Statement));
    if (!stmt) {
        yyerror("Out of memory");
        return NULL;
    }
    stmt->type = STMT_ASSIGNMENT;
    stmt->assignment.identifier = strdup(id); // Safely duplicate the identifier
    stmt->assignment.expression = expr;
    return stmt;
}

// Function to create an if statement structure
Statement* make_if_statement(Expression* condition, StatementList* thenBranch, StatementList* elseBranch) {
    Statement* stmt = malloc(sizeof(Statement));
    if (!stmt) {
        yyerror("Out of memory");
        return NULL;
    }
    stmt->type = STMT_IF;
    stmt->ifStmt.condition = condition;
    stmt->ifStmt.thenBranch = thenBranch;
    stmt->ifStmt.elseBranch = elseBranch;
    return stmt;
}

// Function to create a while statement structure
Statement* make_while_statement(Expression* condition, StatementList* body) {
    Statement* stmt = malloc(sizeof(Statement));
    if (!stmt) {
        yyerror("Out of memory");
        return NULL;
    }
    stmt->type = STMT_WHILE;
    stmt->whileStmt.condition = condition;
    stmt->whileStmt.body = body;
    return stmt;
}

// Function to create a writeInt statement structure
Statement* make_writeInt_statement(Expression* expr) {
    Statement* stmt = malloc(sizeof(Statement));
    if (!stmt) {
        yyerror("Out of memory");
        return NULL;
    }
    stmt->type = STMT_WRITEINT;
    stmt->writeInt.expression = expr;
    return stmt;
}

// Function to create an operation expression
Expression* make_operation_expression(Expression* left, Operator op, Expression* right) {
    Expression* expr = malloc(sizeof(Expression));
    if (!expr) {
        yyerror("Out of memory");
        return NULL;
    }
    expr->type = EXPR_OP;
    expr->operation.left = left;
    expr->operation.op = op;
    expr->operation.right = right;
    return expr;
}

// Function to generate C code from a sequence of statements
char* statement_sequence_to_c_code(StatementList* stmtList) {
    // This needs to dynamically generate code based on your statement list
    // Placeholder for example
    char* code = malloc(10000); // Ensure this is appropriately sized
    strcpy(code, "// Implement dynamic statement generation here\n");
    return code;
}

// Functions to generate C code for control structures
void generate_c_code_for_if(const char* condition, const char* then_code, const char* else_code) {
    fprintf(outputFile, "if (%s) {\n%s}\n", condition, then_code);
    if (else_code) {
        fprintf(outputFile, "else {\n%s}\n", else_code);
    }
}

void generate_c_code_for_while(const char* condition, const char* body_code) {
    fprintf(outputFile, "while (%s) {\n%s}\n", condition, body_code);
}

void generate_c_code_for_writeInt(const char* expr_code) {
    char temp[1024];  // Temporary buffer to hold the generated code.
    sprintf(temp, "    printf(\"%%d\\n\", %s);\n", expr_code);
    add_statement(temp);  // Add the formatted statement to the global statements buffer.
}

Operator identifyOperator(char* op) {
    if (strcmp(op, "+") == 0) return OP_ADD;
    else if (strcmp(op, "-") == 0) return OP_SUBTRACT;
    else if (strcmp(op, "*") == 0) return OP_MULTIPLY;
    else if (strcmp(op, "div") == 0) return OP_DIVIDE;
    else if (strcmp(op, "mod") == 0) return OP_MOD;
    else if (strcmp(op, "==") == 0) return OP_EQUAL;
    else if (strcmp(op, "!=") == 0) return OP_NOTEQUAL;
    else if (strcmp(op, "<") == 0) return OP_LESS;
    else if (strcmp(op, "<=") == 0) return OP_LESSEQ;
    else if (strcmp(op, ">") == 0) return OP_GREATER;
    else if (strcmp(op, ">=") == 0) return OP_GREATEREQ;
    else {
        fprintf(stderr, "Unknown operator: %s\n", op);
        exit(EXIT_FAILURE);
    }
}

const char* op_to_string(Operator op) {
    switch (op) {
        case OP_ADD: return "+";
        case OP_SUBTRACT: return "-";
        case OP_MULTIPLY: return "*";
        case OP_DIVIDE: return "/";
        case OP_MOD: return "%";
        case OP_EQUAL: return "==";
        case OP_NOTEQUAL: return "!=";
        case OP_LESS: return "<";
        case OP_LESSEQ: return "<=";
        case OP_GREATER: return ">";
        case OP_GREATEREQ: return ">=";
        default: return "??";
    }
}


/* Helper functions for creating various structures */
Type* make_type(TypeEnum te) {
    Type* type = (Type*)malloc(sizeof(Type));
    if (type == NULL) {
        yyerror("Out of memory");
        exit(EXIT_FAILURE);
    }
    type->typeEnum = te;
    return type;
}

Expression* make_number_expression(int num) {
  Expression* expr = malloc(sizeof(Expression));
  if (!expr) {
    yyerror("Out of memory");
    return NULL;
  }
  expr->type = EXPR_NUM;
  expr->num = num;
  return expr;
}

char* expression_to_c_code(Expression* expr) {
    char* code = malloc(1000); // Allocate enough space for the expression code
    if (!code) {
        yyerror("Out of memory");
        return NULL;
    }

    switch (expr->type) {
        case EXPR_NUM:
            sprintf(code, "%d", expr->num);
            break;
        case EXPR_IDENT:
            sprintf(code, "%s", expr->ident);
            break;
        case EXPR_OP: {
            char* left = expression_to_c_code(expr->operation.left);
            char* right = expression_to_c_code(expr->operation.right);
            const char* op_str = op_to_string(expr->operation.op);
            sprintf(code, "(%s %s %s)", left, op_str, right);
            free(left);
            free(right);
            break;
        }
        default:
            sprintf(code, "unsupported_expression_type");
    }
    return code;
}

int main(int argc, char **argv) {
    printf("Starting the compiler.\n");

    if (argc != 3) {
        fprintf(stderr, "Usage: %s <input_filename> <output_filename>\n", argv[0]);
        return 1;
    }

    printf("Input file: %s\n", argv[1]);
    printf("Output file: %s\n", argv[2]);

    yyin = fopen(argv[1], "r");
    if (!yyin) {
        perror("Error opening input file");
        return 1;
    }
    printf("Input file successfully opened.\n");

    outputFile = fopen(argv[2], "w");
    if (!outputFile) {
        perror("Error opening output file");
        fclose(yyin);  // Ensure the input file is closed if output file open fails
        return 1;
    }
    printf("Output file successfully opened.\n");

    pushSymbolTable();  // Initialize the symbol table for the global scope
    printf("Symbol table initialized.\n");

    printf("Starting parsing process...\n");
    if (yyparse() == 0) {  // Parse successfully completed
        fprintf(stdout, "Successfully parsed the file.\n");
    } else {
        fprintf(stdout, "Parsing failed.\n");
    }

    popSymbolTable();  // Clean up the global symbol table
    printf("Symbol table cleaned up.\n");

    fclose(yyin);
    fclose(outputFile);
    printf("Files closed. Exiting program.\n");

    return 0;
}

void yyerror(const char *s) {
    fprintf(stderr, "Error: %s at line %d\n", s, yylineno);
    // print more about the error context
    fprintf(stderr, "Last token read: %s\n", yytext);
}

void output_statement(const char* code) {
    fprintf(outputFile, "%s\n", code);  // Write to outputFile
}

int yywrap() {
  printf("yywrap() called.");
  return 1;
}
