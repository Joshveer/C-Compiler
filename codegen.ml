(* codegen.ml *)
open Ast
open Asm

let gen_exp = function
  | Ast.Constant n -> Asm.Imm n

let gen_statement = function
  | Ast.Return e ->
      let op = gen_exp e in
      [ Asm.Mov (op, Asm.Reg Asm.EAX); Asm.Ret ]

let gen_function (Ast.Function (name, body)) =
  (* Create the Asm record, not the Ast constructor *)
  { Asm.name = name; Asm.instructions = gen_statement body }

(* Explicitly specify we are matching Ast.Program and creating Asm.Program *)
let gen_program (Ast.Program f) =
  Asm.Program (gen_function f)
