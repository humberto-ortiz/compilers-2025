/* parser.mly - menhir source for parser for increment programs 
   Copyright (2023) Humberto Ortiz-Zuazaga <humberto.ortiz@upr.edu>
   See LICENSE for details
*/
%token <int64> INT
%token PLUS
%token MINUS
%token TIMES
%token LPAREN
%token RPAREN
%token LET
%token <string> ID 
%token EOF

%start <Ast.expr> prog
%%

prog:
  | e = expr EOF { e }

expr:
  | i = INT { EConstant i }
  | id = ID { EId id }
  | LPAREN PLUS l = expr r = expr RPAREN 
    { EPrim2 (Plus, l, r) }
  | LPAREN LET LPAREN id = ID e1 = expr RPAREN e2 = expr RPAREN 
    { ELet (id, e1, e2) }
