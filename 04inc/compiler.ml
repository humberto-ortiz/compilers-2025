open Printf
open Ast

type reg =
  | RAX (* the register where we place answers *)

type arg =
  | Const of int64 (* explicit numeric constants *)
  | Reg of reg (* any named register *)

type instruction =
  | IMov of arg * arg (* Move the value of the right-side arg into the left-arg *)
  | IAdd of arg * arg

let arg_to_string arg =
  match arg with
  | Reg RAX -> "RAX"
  | Const num -> Int64.to_string num

let instr_to_string instr =
  match instr with
  | IMov (dst, src) -> "mov" ^ arg_to_string dst ^ ", " ^ arg_to_string src ^ "\n"
  | IAdd (dst, src) -> "add" ^ arg_to_string dst ^ ", " ^ arg_to_string src ^ "\n"

let rec asm_to_string (asm : instruction list) : string =
  (* do something to get a string of assembly *)
  match asm with
  | [] -> ""
  | instr::instrs -> instr_to_string instr ^ asm_to_string instrs

(* REFACTORING STARTS HERE *)
(* compile_expr is responsible for compiling just a single expression,
   and does not care about the surrounding scaffolding *)
let compile_expr (e : expr) : instruction list =
  match e with
  | Constant num -> [ IMov (Reg RAX, Const num) ]
  | _ -> failwith "to be continued"


(* compile_prog surrounds a compiled program by whatever scaffolding is needed *)
let compile_prog (e : expr) : string =
  (* compile the program *)
  let instrs = compile_expr e in
  (* convert it to a textual form *)
  let asm_string = asm_to_string instrs in
  (* surround it with the necessary scaffolding *)
  let prelude = "
section .text
global our_code_starts_here
our_code_starts_here:" in
  let suffix = "ret" in
  prelude ^ "\n" ^ asm_string ^ "\n" ^ suffix

(* Some OCaml boilerplate for reading files and command-line arguments *)
let () =
  let input_file = (open_in (Sys.argv.(1))) in
  let lexbuf = Lexing.from_channel input_file in
  let input_program = Parser.prog Lexer.read lexbuf in
  close_in input_file;
  let program = (compile_prog input_program) in
  printf "%s\n" program
