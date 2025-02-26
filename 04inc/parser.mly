/* parser.mly - menhir source for parser for increment programs 
   Copyright (2023) Humberto Ortiz-Zuazaga <humberto.ortiz@upr.edu>
   See LICENSE for details
*/
%token <int64> INT
%token INCREMENT
%token EOF

%start <Ast.expr> prog
%%

prog:
  | e = expr EOF { e }

expr:
  | i = INT { Constant i }
  | e = expr INCREMENT { Increment e }
