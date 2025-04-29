(* ast.ml - abstract syntax for compiler *)

type prim2 = 
  | Plus
  | Minus
  | Times
  | Less

type prim1 =
  | Print

type 'a expr =
  | ENumber of int64 * 'a
  | EBool of bool * 'a
  | EId of string * 'a
  | EPrim1 of prim1 * 'a expr * 'a
  | EPrim2 of prim2 * 'a expr * 'a expr * 'a
  | ELet of string * 'a expr * 'a expr * 'a
  | EIfdvd of 'a expr * 'a expr * 'a expr * 'a
  | EApp of string * 'a expr * 'a

type 'a decl =
  | DFun of string * string * 'a expr * 'a

type 'a program =
   | Prog of 'a decl list * 'a expr 

type immexpr =
  | ImmNum of int64
  | ImmId of string
  | ImmBool of bool

type aexpr =
  | AImm of immexpr
  | APrim1 of prim1 * immexpr
  | APrim2 of prim2 * immexpr * immexpr
  | ALet of string * aexpr * aexpr
  | AIfdvd of immexpr * aexpr * aexpr
  | AApp of string * immexpr

type adecl =
  | ADFun of string * string * aexpr

type aprogram =
  | AProg of adecl list * aexpr
