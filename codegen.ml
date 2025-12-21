open Ast
open Asm

let rec gen_exp = function
  | Ast.Constant n ->
      [ Mov (Imm n, Reg EAX) ]
  | Ast.Unary (op, e) ->
      let instrs = gen_exp e in
      let asm_op = match op with
        | Ast.Complement -> Asm.Not
        | Ast.Negate -> Asm.Neg
      in
      instrs @ [ Unary (asm_op, Reg EAX) ]

let gen_statement = function
  | Ast.Return e ->
      gen_exp e @ [ Ret ]

let gen_function (Ast.Function (name, body)) =
  { Asm.name = name; Asm.instructions = gen_statement body }

let gen_program (Ast.Program f) =
  Asm.Program (gen_function f)
