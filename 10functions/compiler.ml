open Printf
open Ast

type reg =
  | RAX (* the register where we place answers *)
  | RSP (* stack pointer *)
  | RDI (* first arg *)
  | RBP (* base pointer *)
(*
  | RSI (* second arg *)
*)

type arg =
  | Const of int64 (* explicit numeric constants *)
  | Reg of reg (* any named register *)
  | RegOffset of reg * int 

type instruction =
  | IMov of arg * arg (* Move the value of the right-side arg into the left-arg *)
  | IAdd of arg * arg
  | ISub of arg * arg
  | IImul of arg * arg
  | ISar of arg * arg
  | ICmp of arg * arg
  | ITst of arg * arg
  | IPush of arg
  | IPop of arg
  | IJmp of string
  | IJnz of string
  | IJl of string
  | ILabel of string
  | ICall of string
  | IRet

let reg_to_string r =
  match r with 
  | RAX -> "RAX"
  | RSP -> "RSP"
  | RDI -> "RDI"
  | RBP -> "RBP"
(*
  | RSI -> "RSI"
*)

let arg_to_string arg =
  match arg with
  | Reg r -> reg_to_string r
  | Const num -> Int64.to_string num
  | RegOffset (r, o) -> "[" ^ reg_to_string r ^ "+" ^ string_of_int o ^ "]"

let instr_to_string (instr : instruction) : string =
  match instr with
  | IMov (dst, src) -> "mov " ^ arg_to_string dst ^ ", " ^ arg_to_string src ^ "\n"
  | IAdd (dst, src) -> "add " ^ arg_to_string dst ^ ", " ^ arg_to_string src ^ "\n"
  | ISub (dst, src) -> "sub " ^ arg_to_string dst ^ ", " ^ arg_to_string src ^ "\n"
  | IImul (dst, src) -> "imul " ^ arg_to_string dst ^ ", " ^ arg_to_string src ^ "\n"
  | ISar (dst, src) -> "sar " ^ arg_to_string dst ^ ", " ^ arg_to_string src ^ "\n"
  | ICmp (dst, src) -> "cmp " ^ arg_to_string dst ^ ", " ^ arg_to_string src ^ "\n"
  | ITst (dst, src) -> "test " ^ arg_to_string dst ^ ", " ^ arg_to_string src ^ "\n"
  | IPush (dst) -> "push " ^ arg_to_string dst ^ "\n"
  | IPop (dst) -> "pop " ^ arg_to_string dst ^ "\n"
  | IJmp label -> "jmp " ^ label ^ "\n"
  | IJnz label -> "jnz " ^ label ^ "\n"
  | IJl label -> "jl " ^ label ^ "\n"
  | ILabel label -> label ^ ":\n"
  | ICall label -> "call " ^ label ^ "\n"
  | IRet -> "ret\n"

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

(* tag los expr con numeros unicos *)
type tag = int

let tag_program (p : 'a program) : (tag program) =
  let rec tag_expr e cur =
    match e with
    | ENumber (n, _) -> (ENumber (n, cur), (cur + 1))
    | EBool (b, _) -> (EBool (b, cur), (cur + 1))
    | EId (v, _) -> (EId (v, cur), (cur + 1))
    | EPrim1 (op, e, _) -> 
       let (tag_e, next_tag) = tag_expr e cur in
       (EPrim1 (op, tag_e, next_tag), (next_tag + 1))
    | EPrim2 (op, left, right, _) ->
       let (tag_l, next_tag) = tag_expr left cur in
       let (tag_r, next_tag) = tag_expr right next_tag in
       (EPrim2 (op, tag_l, tag_r, next_tag), (next_tag + 1))
    | ELet (v, init, body, _) ->
       let (tag_i, next_tag) = tag_expr init cur in
       let (tag_b, next_tag) = tag_expr body next_tag in
       (ELet (v, tag_i, tag_b, next_tag), (next_tag + 1))
    | EIfdvd (cnd, thn, els, _) ->
       let (tag_cnd, next_tag) = tag_expr cnd cur in
       let (tag_thn, next_tag) = tag_expr thn next_tag in
       let (tag_els, next_tag) = tag_expr els next_tag in
       (EIfdvd (tag_cnd, tag_thn, tag_els, next_tag), (next_tag + 1))
    | EApp (f, x, _) -> 
       let (tag_x, next_tag) = tag_expr x cur in
       (EApp (f, tag_x, next_tag), (next_tag + 1))
    and tag_def def cur = 
      match def with
      | DFun (f, a, body, _) ->
         let (tagged_body, next_tag) = tag_expr body cur in
         (DFun (f, a, tagged_body, next_tag), (next_tag + 1))
  in
  match p with
  | Prog (ds, e) -> 
     let rec help (ds, cur) =
       match ds with
       | [] -> ([], cur)
       | d :: ds -> 
          let next_tag = cur + 1 in
          let (tagged_d, next_tag) = tag_def d next_tag in
          let (tagged_ds, next_tag) = help (ds, next_tag) in
          (tagged_d :: tagged_ds, next_tag)
     in
     let (tagged_ds, next_tag) = help (ds, 1) in
     let (tagged_e, _) = tag_expr e next_tag in
     Prog (tagged_ds, tagged_e)

(* anf - transform 'a expr to aexpr *)
let rec anf (e : 'a expr) (expr_with_holes : (immexpr -> aexpr)) : aexpr =
  match e with
  | ENumber (n, _) -> (expr_with_holes (ImmNum n))
  | EBool (b, _) -> (expr_with_holes (ImmBool b))
  | EId (b, _) -> (expr_with_holes (ImmId b))
  | EPrim1 (op, e, tag) ->
     anf e (fun eimm ->
         let varname = "bar" ^ (string_of_int tag) in
         ALet (varname,
               APrim1 (op, eimm),
               (expr_with_holes (ImmId varname))))
  | EPrim2 (op, l, r, tag) ->
     anf l (fun limm ->
       anf r (fun rimm ->
           let varname = "foo" ^ (string_of_int tag) in
         ALet (varname,
           APrim2 (op, limm, rimm),
           (expr_with_holes (ImmId varname)))))
  | ELet (v, init, body, _) ->
     (anf init (fun immval ->
          ALet (v, AImm immval, 
                (anf body (fun immbody ->
                     (expr_with_holes immbody))))))
  | EIfdvd (cnd, thn, els, _) ->
     (* aun no se hacer esto *)
     anf cnd (fun immcnd ->
         AIfdvd (immcnd, 
               anf thn expr_with_holes,
               anf els expr_with_holes))
  | EApp (f, e, tag) ->
     (anf e (fun immval ->
          let varname = "_app" ^ (string_of_int tag) in
          ALet (varname,
                AApp (f, immval),
                (expr_with_holes (ImmId varname)))))

let anf_prog p expr_with_holes =
  let help dec =
    match dec with
    | DFun (f, x, body, _) ->
       ADFun (f, x, anf body expr_with_holes)
  in
  match p with
  | Prog (ds, e) ->
     AProg (List.map help ds, anf e expr_with_holes)

let rename (e : tag expr) : tag expr =
  let rec help (e : tag expr) (env : (string * string) list) : tag expr =
    match e with
    | ENumber _ -> e
    | EBool _ -> e
    | EId (x, tag) -> EId (lookup x env, tag)
    | EPrim1 (op, left, tag) ->
       let renamed_left = help left env in
       EPrim1 (op, renamed_left, tag)
    | EPrim2 (op, left, right, tag) ->
       let renamed_left = help left env in
       let renamed_right = help right env in
       EPrim2 (op, renamed_left, renamed_right, tag)
    | ELet (v, init, body, tag) ->
       let renamed = sprintf "%s#%d" v tag in
       ELet (renamed, help init env, help body ((v, renamed)::env), tag)
    | EIfdvd (cnd, thn, els, tag) ->
       let r_cnd = help cnd env in
       let r_thn = help thn env in
       let r_els = help els env in
       EIfdvd (r_cnd, r_thn, r_els, tag)
    | EApp (f, e, tag) ->
       let renamed = help e env in
       EApp (f, renamed, tag)
  in
  help e []

let rename_prog p = 
  let help dec =
    match dec with
    | DFun (f, x, body, tag) ->
       DFun(f, x, body, tag)
  in
  match p with
  | Prog (ds, e) -> Prog (List.map help ds, rename e)

(* marronear para labels unicos *)
let count = ref 0

let gen_temp base =
  count := !count + 1;
  sprintf "temp_%s_%d" base !count

let const_true =  0xFFFFFFFFFFFFFFFFL
let const_false = 0x7FFFFFFFFFFFFFFFL
let const_tag =   0x0000000000000001L

(* REFACTORING STARTS HERE *)
(* compile_expr is responsible for compiling just a single expression,
   and does not care about the surrounding scaffolding *)
let rec compile_aexpr (e : aexpr) (env : env) : instruction list =
  let imm_to_arg imm =
    match imm with
    | ImmNum n -> Const (Int64.mul n 2L)   (* espacio para el tag bit *)
    | ImmBool true -> Const const_true
    | ImmBool false -> Const const_false
    | ImmId v -> let slot = lookup v env
                 in RegOffset (RBP, ~-8*slot)
  in
  match e with
  | AImm imm -> 
     [ IMov (Reg RAX, imm_to_arg imm ) ]
  | ALet (v, init, body) ->  
     let 
       (env', slot) = add v env 
     in
     compile_aexpr init env @
       [ IMov (RegOffset (RBP, ~-8*slot), Reg RAX) ] @
         compile_aexpr body env'
  | APrim1 (Print, imm) ->
     [ IMov (Reg RDI, imm_to_arg imm) ;
       ICall "print" ]
  | APrim2 (Plus, left, right) ->
     [ IMov (Reg RAX, imm_to_arg left) ;
       ITst (Reg RAX, Const const_tag) ;
       IJnz "err_not_number" ;
       IMov (Reg RAX, imm_to_arg right) ;
       ITst (Reg RAX, Const const_tag);
       IJnz "err_not_number" ;
       IMov (Reg RAX, imm_to_arg left) ;
       IAdd (Reg RAX, imm_to_arg right) ]
  | APrim2 (Minus, left, right) ->
     [ IMov (Reg RAX, imm_to_arg left) ;
       ITst (Reg RAX, Const const_tag) ;
       IJnz "err_not_number" ;
       IMov (Reg RAX, imm_to_arg right) ;
       ITst (Reg RAX, Const const_tag);
       IJnz "err_not_number" ;
       IMov (Reg RAX, imm_to_arg left) ;
       ISub (Reg RAX, imm_to_arg right) ]
  | APrim2 (Times, left, right) ->
     [ IMov (Reg RAX, imm_to_arg left) ;
       ITst (Reg RAX, Const const_tag) ;
       IJnz "err_not_number" ;
       IMov (Reg RAX, imm_to_arg right) ;
       ITst (Reg RAX, Const const_tag);
       IJnz "err_not_number" ;
       IMov (Reg RAX, imm_to_arg left) ;
       IImul (Reg RAX, imm_to_arg right) ;
       ISar (Reg RAX, Const 1L)
     ]
  | APrim2 (Less, left, right) ->
     [ IMov (Reg RAX, imm_to_arg left) ;
       ITst (Reg RAX, Const const_tag) ;
       IJnz "err_not_number" ;
       IMov (Reg RAX, imm_to_arg right) ;
       ITst (Reg RAX, Const const_tag);
       IJnz "err_not_number" ;
       IMov (Reg RAX, imm_to_arg left) ;
       ICmp (Reg RAX, imm_to_arg right) ;
       IJl "less" ;
       IMov (Reg RAX, Const const_false) ;
       IJmp "done" ;
       ILabel "less" ;
       IMov (Reg RAX, Const const_true) ;
       ILabel "done"
     ]

  | AIfdvd (cnd, thn, els) ->
     let elselabel = gen_temp "else" in
     let donelabel = gen_temp "done" in
     [ IMov (Reg RAX, imm_to_arg cnd) ;
       ICmp (Reg RAX, Const const_true) ;
       IJnz elselabel ] @
       compile_aexpr thn env @
    [ IJmp donelabel ;
      ILabel elselabel ] @
      compile_aexpr els env @
    [ ILabel donelabel ]

  | AApp (f, x) ->
     [ IMov (Reg RDI, imm_to_arg x) ;
       ICall f ]
(*  | _ -> failwith "No se compilar eso." *)

let rec depth (e : aexpr) : int =
  match e with
  | ALet (_, init, body) -> 1 + Int.max (depth init) (depth body)
  | AIfdvd (_, t, e) -> Int.max (depth t) (depth e)
  | _ -> 0

(* compile_prog surrounds a compiled program by whatever scaffolding is needed *)
let compile_prog (p : 'a program) : string =
  (* tag expr *)
  let tagged = tag_program p in
  (* rename the variables *)
  let renamed = rename_prog tagged in
  (* convert to anf *)
  let anfed = anf_prog renamed (fun imm -> AImm imm) in
  (* compile the program *)
  let compile_dec dec env =
    match dec with
    | ADFun (f, x, body) ->
       let (env', slot) = add x env in
 
       [ ILabel f ;
         IPush (Reg RBP) ;
         IMov (Reg RBP, Reg RSP) ;
         ISub (Reg RSP, Const 8L) ;
         IMov (RegOffset (RBP, ~-8*slot), Reg RDI) ] @
         compile_aexpr body env' @
           [ IMov (Reg RSP, Reg RBP) ;
             IPop (Reg RBP);
             IRet ]
in
  match anfed with
  | AProg (ds, a) ->
    let def_instr = List.flatten (List.map (fun d -> compile_dec d []) ds ) in
    let def_asm = asm_to_string def_instr in
  let instrs = compile_aexpr a [] in
  let stack_depth = depth a in
  (* convert it to a textual form *)
  let asm_string = asm_to_string instrs in
  (* surround it with the necessary scaffolding *)
  sprintf "
section .text
extern our_error
extern print

global our_code_starts_here
err_not_number:
	mov rdi, 1
	mov rsi, rax
	call our_error

%s

our_code_starts_here:
	push RBP
	mov RBP, RSP
        add RSP, -8*%d
%s
	mov RSP, RBP
	pop RBP
	ret" def_asm stack_depth asm_string

(* Some OCaml boilerplate for reading files and command-line arguments *)
let () =
  if 2 = Array.length(Sys.argv) then
    let input_file = Sys.argv.(1) in
    let  input_program = Front.parse_file input_file in
    let program = (compile_prog input_program) in
    printf "%s\n" program

