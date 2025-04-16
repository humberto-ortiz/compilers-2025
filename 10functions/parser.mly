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
%token IFDVD
%token TRUE
%token FALSE
%token PRINT
%token DEF

%start <'a Ast.program> prog
%%

prog:
  | e = expr EOF { Prog ([], e) }
  | ds = defs e = expr EOF { Ast.Prog (ds, e) }

defs:
  | d = def { [d] }
  | d = def ds = defs { d :: ds }

def:
 | LPAREN DEF LPAREN f = ID x = ID RPAREN body = expr RPAREN
    { DFun (f, x, body, $startpos) }

expr:
  | i = INT { ENumber (i, $startpos) }
  | id = ID { EId (id, $startpos) }
  | LPAREN LET LPAREN id = ID e1 = expr RPAREN e2 = expr RPAREN 
    { ELet (id, e1, e2, $startpos) }
  | LPAREN PLUS e1 = expr e2 = expr RPAREN 
    { EPrim2 (Plus, e1, e2, $startpos) }
  | LPAREN IFDVD cnd = expr thn = expr els = expr RPAREN
    { EIfdvd (cnd, thn, els, $startpos) }
  | FALSE { EBool (false, $startpos) }
  | TRUE { EBool (true, $startpos) }
  | LPAREN PRINT e1 = expr RPAREN
    { EPrim1 (Print, e1, $startpos) }
  | LPAREN f = ID e1 = expr RPAREN
    { EApp (f, e1, $startpos) }
