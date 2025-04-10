(* ast.ml - abstract syntax for compiler *)

type prim2 = 
  | Plus
  | Minus
  | Times

type 'a expr =
  | ENumber of int64 * 'a
  | EBool of bool * 'a
  | EId of string * 'a
  | EPrim2 of prim2 * 'a expr * 'a expr * 'a
  | ELet of string * 'a expr * 'a expr * 'a
  | EIfdvd of 'a expr * 'a expr * 'a expr * 'a

type immexpr =
  | ImmNum of int64
  | ImmId of string
  | ImmBool of bool

type aexpr =
  | AImm of immexpr
  | APrim2 of prim2 * immexpr * immexpr
  | ALet of string * aexpr * aexpr
  | AIfdvd of immexpr * aexpr * aexpr
