(* ast.ml - abstract syntax for compiler *)

type expr =
  | Constant of int64
  | Increment of expr
