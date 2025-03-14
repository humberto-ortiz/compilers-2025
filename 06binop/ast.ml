(* ast.ml - abstract syntax for compiler *)

type binop = Plus | Minus | Times

type expr =
  | EConstant of int64
  | ELet of string * expr * expr
  | EId of string
  | EPrim2 of binop * expr * expr

type immexp =
  | ImmNum of int64
  | ImmId of string

type aexpr =
  | AImm of immexp
  | APrim2 of binop * immexp * immexp 
  | ALet of string * aexpr * aexpr 
