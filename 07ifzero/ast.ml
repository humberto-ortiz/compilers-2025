(* ast.ml - abstract syntax for compiler *)

type prim2 = 
  | Plus
  | Minus
  | Times

type 'a expr =
  | ENumber of int64 * 'a
  | EId of string * 'a
  | EPrim2 of prim2 * 'a expr * 'a expr * 'a
  | ELet of string * 'a expr * 'a expr * 'a
  | EIf0 of 'a expr * 'a expr * 'a expr * 'a

type immexpr =
  | ImmNum of int64
  | ImmId of string

type aexpr =
  | AImm of immexpr
  | APrim2 of prim2 * immexpr * immexpr
  | ALet of string * aexpr * aexpr
  | AIf0 of immexpr * aexpr * aexpr
