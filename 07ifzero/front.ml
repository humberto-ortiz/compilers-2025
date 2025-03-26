open Lexing
open Lexer
open Printf

(* Prints the line number and character number where the error occurred.*)
let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_program lexbuf =
  try (Parser.prog Lexer.read lexbuf) with
  (* catch exception and turn into Error *)
  | SyntaxError msg ->
      let error_msg = sprintf "%s: %s." (print_error_position lexbuf) msg in
      failwith error_msg
  | Parser.Error ->
      let error_msg = sprintf "%s: syntax error." (print_error_position lexbuf) in
      failwith error_msg

let parse_file (in_file) =
  let lexbuf = Lexing.from_channel (open_in in_file) in
  let ast = parse_program lexbuf in
  ast
