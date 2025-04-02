(* esta_suma.ml - un ejemplo de una suma en int aexpr *)
open Ast

let esta_suma = ALet ("a",
                      APrim2 (Plus, ImmNum 2L, ImmNum 3L, 1),
                      APrim2 (Minus, ImmNum 1L, ImmId "a", 2), 3);;
