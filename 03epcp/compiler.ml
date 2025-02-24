open Printf

(* A very sophisticated compiler - insert the given integer into the mov
instruction at the correct place *)
let compile (program : int64) : string =
  sprintf "
section .text
global our_code_starts_here
our_code_starts_here:
  mov RAX, %Ld
  ret\n" program;;

(* Some OCaml boilerplate for reading files and command-line arguments *)
let () =
  let input_file = (open_in (Sys.argv.(1))) in
  let lexbuf = Lexing.from_channel input_file in
  let input_program = Parser.prog Lexer.read lexbuf in
  close_in input_file;
  let program = (compile input_program) in
  printf "%s\n" program
