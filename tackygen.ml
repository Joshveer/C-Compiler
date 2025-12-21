open Ast
open Tacky

let tmp_counter = ref 0

let make_temporary () =
  let name = Printf.sprintf "tmp.%d" !tmp_counter in
  incr tmp_counter;
  name

let make_label () = 
  let name = Printf.sprintf "L.%d" !tmp_counter in 
  incr tmp_counter;
  name

let convert_unop = function
  | Ast.Complement -> Tacky.Complement
  | Ast.Negate -> Tacky.Negate
  | Ast.Not -> Tacky.Not

let convert_binop = function
  | Ast.Add -> Tacky.Add
  | Ast.Subtract -> Tacky.Subtract
  | Ast.Multiply -> Tacky.Multiply
  | Ast.Divide -> Tacky.Divide
  | Ast.Remainder -> Tacky.Remainder
  | Ast.BitAnd -> Tacky.BitAnd
  | Ast.BitOr -> Tacky.BitOr
  | Ast.Xor -> Tacky.Xor
  | Ast.ShiftLeft -> Tacky.ShiftLeft
  | Ast.ShiftRight -> Tacky.ShiftRight
  | Ast.Equal -> Tacky.Equal
  | Ast.NotEqual -> Tacky.NotEqual
  | Ast.LessThan -> Tacky.LessThan
  | Ast.LessOrEqual -> Tacky.LessOrEqual
  | Ast.GreaterThan -> Tacky.GreaterThan
  | Ast.GreaterOrEqual -> Tacky.GreaterOrEqual
  | _ -> failwith "Internal Error: And/Or should be handled specially"

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
  
  | Ast.Binary (Ast.And, e1, e2) ->
      let false_lbl = make_label () in
      let end_lbl = make_label () in
      let dst_name = make_temporary () in
      let dst = Tacky.Var dst_name in

      let v1 = emit_tacky e1 instrs in
      instrs := !instrs @ [Tacky.JumpIfZero (v1, false_lbl)];

      let v2 = emit_tacky e2 instrs in
      instrs := !instrs @ [Tacky.JumpIfZero (v2, false_lbl)];

      instrs := !instrs @ [Tacky.Copy (Tacky.Constant 1, dst); Tacky.Jump end_lbl];
      
      instrs := !instrs @ [Tacky.Label false_lbl; Tacky.Copy (Tacky.Constant 0, dst)];
      instrs := !instrs @ [Tacky.Label end_lbl];
      dst

  | Ast.Binary (Ast.Or, e1, e2) ->
      let true_lbl = make_label () in
      let end_lbl = make_label () in
      let dst_name = make_temporary () in
      let dst = Tacky.Var dst_name in

      let v1 = emit_tacky e1 instrs in
      instrs := !instrs @ [Tacky.JumpIfNotZero (v1, true_lbl)];

      let v2 = emit_tacky e2 instrs in
      instrs := !instrs @ [Tacky.JumpIfNotZero (v2, true_lbl)];

      instrs := !instrs @ [Tacky.Copy (Tacky.Constant 0, dst); Tacky.Jump end_lbl];

      instrs := !instrs @ [Tacky.Label true_lbl; Tacky.Copy (Tacky.Constant 1, dst)];
      instrs := !instrs @ [Tacky.Label end_lbl];
      dst

  | Ast.Binary (op, e1, e2) ->
      let v1 = emit_tacky e1 instrs in
      let v2 = emit_tacky e2 instrs in
      let dst_name = make_temporary () in
      let dst = Tacky.Var dst_name in
      let tacky_op = convert_binop op in
      instrs := !instrs @ [Tacky.Binary (tacky_op, v1, v2, dst)];
      dst

let gen_function_def (Ast.Function (name, body)) =
  let instrs = ref [] in
  match body with
  | Ast.Return e ->
      let v = emit_tacky e instrs in
      instrs := !instrs @ [Tacky.Return v];
      { Tacky.name = name; body = !instrs }

let gen_program (Ast.Program f) =
  Tacky.Program (gen_function_def f)
