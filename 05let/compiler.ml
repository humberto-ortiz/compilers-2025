open Printf
open Ast

type reg =
  | RAX (* the register where we place answers *)
  | RSP (* stack pointer *)

type arg =
  | Const of int64 (* explicit numeric constants *)
  | Reg of reg (* any named register *)
  | RegOffset of reg * int 

type instruction =
  | IMov of arg * arg (* Move the value of the right-side arg into the left-arg *)
  | IAdd of arg * arg

let reg_to_string r =
  match r with 
  | RAX -> "RAX"
  | RSP -> "RSP"

let arg_to_string arg =
  match arg with
  | Reg r -> reg_to_string r
  | Const num -> Int64.to_string num
  | RegOffset (r, o) -> "[" ^ reg_to_string r ^ "+" ^ string_of_int o ^ "]"

let instr_to_string instr =
  match instr with
  | IMov (dst, src) -> "mov " ^ arg_to_string dst ^ ", " ^ arg_to_string src ^ "\n"
  | IAdd (dst, src) -> "add " ^ arg_to_string dst ^ ", " ^ arg_to_string src ^ "\n"

let rec asm_to_string (asm : instruction list) : string =
  (* do something to get a string of assembly *)
  match asm with
  | [] -> ""
  | instr::instrs -> instr_to_string instr ^ asm_to_string instrs

type env = (string * int) list

let rec  lookup name env =
  match env with
  | [] -> failwith (sprintf "Identifier %s not found in environment" name)
  | (n, i)::rest ->
    if n = name then i else (lookup name rest)

let add name env : (env * int) =
  let slot = 1 + (List.length env) in
  ((name, slot)::env, slot)

(* REFACTORING STARTS HERE *)
(* compile_expr is responsible for compiling just a single expression,
   and does not care about the surrounding scaffolding *)
let rec compile_expr (e : expr) (env : env) : instruction list =
  match e with
  | Constant num -> [ IMov (Reg RAX, Const num) ]
  | Increment expr -> compile_expr expr env @ [ IAdd (Reg RAX, Const 1L) ]
  | Id iD -> let slot = lookup iD env in
             [ IMov (Reg RAX, RegOffset (RSP, ~-8*slot)) ]
| Let (v, init, body) ->  let (env', slot) = add v env in
                          compile_expr init env @
                            [ IMov (RegOffset (RSP, ~-8*slot), Reg RAX) ] @
                              compile_expr body env'

(* compile_prog surrounds a compiled program by whatever scaffolding is needed *)
let compile_prog (e : expr) : string =
  (* compile the program *)
  let instrs = compile_expr e [] in
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
