%{
#include <stdio.h>
#include "tl13.tab.h"
%}

%option noyywrap
%option yylineno

%%

[ \t\n]+                  { /* ignore whitespace */ }
"%"[^\n]*                 { /* ignore comments */ }

"if"                      { printf("Token: IF\n"); return IF; }
"then"                    { printf("Token: THEN\n"); return THEN; }
"else"                    { printf("Token: ELSE\n"); return ELSE; }
"begin"                   { printf("Token: PROGRAM_BEGIN\n"); return PROGRAM_BEGIN; }
"end"                     { printf("Token: END\n"); return END; }
"while"                   { printf("Token: WHILE\n"); return WHILE; }
"do"                      { printf("Token: DO\n"); return DO; }
"program"                 { printf("Token: PROGRAM\n"); return PROGRAM; }
"var"                     { printf("Token: VAR\n"); return VAR; }
"as"                      { printf("Token: AS\n"); return AS; }
"int"                     { printf("Token: INT\n"); return INT; }
"bool"                    { printf("Token: BOOL\n"); return BOOL; }
"writeInt"                { printf("Token: WRITEINT\n"); return WRITEINT; }
"readInt"                 { printf("Token: READINT\n"); return READINT; }

[1-9][0-9]*|0             { printf("Token: NUM\n"); yylval.ival = atoi(yytext); return NUM; }
"false"                   { printf("Token: BOOLLIT\n"); yylval.ival = 0; return BOOLLIT; }
"true"                    { printf("Token: BOOLLIT\n"); yylval.ival = 1; return BOOLLIT; }

"("                       { printf("Token: LP\n"); return LP; }
")"                       { printf("Token: RP\n"); return RP; }
":="                      { printf("Token: ASGN\n"); return ASGN; }
";"                       { printf("Token: SC (;)\n"); return SC; }

[A-Z][A-Z0-9]*            { printf("Token: IDENTIFIER (%s)\n", yytext); yylval.sval = strdup(yytext); return IDENTIFIER; }
"*"|"div"|"mod"           { printf("Token: OP2 (%s)\n", yytext); yylval.sval = strdup(yytext); return OP2; }
"+"|"-"                   { printf("Token: OP3 (%s)\n", yytext); yylval.sval = strdup(yytext); return OP3; }
"="|"!="|"<"|">"|"<="|">=" { printf("Token: OP4 (%s)\n", yytext); yylval.sval = strdup(yytext); return OP4; }

.                         { fprintf(stderr, "Unknown character encountered: '%s' at line %d\n", yytext, yylineno); yyerror("Unknown character encountered"); return 0; }


%%