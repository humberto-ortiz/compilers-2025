/* parser.mly - menhir source for parser for increment programs 
   Copyright (2023) Humberto Ortiz-Zuazaga <humberto.ortiz@upr.edu>
   See LICENSE for details
*/
%token <int64> INT
%token LPAREN
%token RPAREN
%token LET
%token <string> ID 
%token EOF
%token PLUS
%token MINUS
%token TIMES
%token IF0

%start <'a Ast.expr> prog
%%

prog:
  | e = expr EOF { e }

expr:
  | i = INT { ENumber (i, $startpos) }
  | id = ID { EId (id, $startpos) }
  | LPAREN LET LPAREN id = ID e1 = expr RPAREN e2 = expr RPAREN 
    { ELet (id, e1, e2, $startpos) }
  | LPAREN PLUS e1 = expr e2 = expr RPAREN 
    { EPrim2 (Plus, e1, e2, $startpos) }
  | LPAREN IF0 cnd = expr thn = expr els = expr RPAREN
    { EIf0 (cnd, thn, els, $startpos) }
