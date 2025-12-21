open Ast
open Tacky

let tmp_counter = ref 0

let make_temporary () =
  let name = Printf.sprintf "tmp.%d" !tmp_counter in
  incr tmp_counter;
  name

let convert_unop = function
  | Ast.Complement -> Tacky.Complement
  | Ast.Negate -> Tacky.Negate

let rec emit_tacky e instrs =
  match e with
  | Ast.Constant c ->
      Tacky.Constant c
  | Ast.Unary (op, inner) ->
      let src = emit_tacky inner instrs in
      let dst_name = make_temporary () in
      let dst = Tacky.Var dst_name in
      let tacky_op = convert_unop op in
      instrs := !instrs @ [Tacky.Unary (tacky_op, src, dst)];
      dst

let convert_statement (Ast.Return e) =
  let instrs = ref [] in
  let v = emit_tacky e instrs in
  !instrs @ [Tacky.Return v]

let convert_function (Ast.Function (name, body)) =
  tmp_counter := 0;
  let body_instrs = convert_statement body in
  { Tacky.name = name; Tacky.body = body_instrs }

let gen_program (Ast.Program f) =
  Tacky.Program (convert_function f)
