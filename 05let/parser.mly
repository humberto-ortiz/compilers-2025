/* parser.mly - menhir source for parser for increment programs 
   Copyright (2023) Humberto Ortiz-Zuazaga <humberto.ortiz@upr.edu>
   See LICENSE for details
*/
%token <int64> INT
%token INCREMENT
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
  | i = INT { Constant i }
  | id = ID { Id id }
  | LPAREN INCREMENT e = expr RPAREN { Increment e }
  | LPAREN LET LPAREN id = ID e1 = expr RPAREN e2 = expr RPAREN { Let (id, e1, e2) }
