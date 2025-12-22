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
  | Ast.Var v ->
      Tacky.Var v
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
  | Ast.Assignment (e1, e2) ->
      begin
        match e1 with
        | Ast.Var name ->
            let v_r = emit_tacky e2 instrs in
            let dst = Tacky.Var name in
            instrs := !instrs @ [Tacky.Copy (v_r, dst)];
            dst
        | _ -> failwith "Internal Error: Lvalue must be a variable"
      end
  | Ast.CompoundAssignment (op, e1, e2) ->
      begin
        match e1 with
        | Ast.Var name ->
            let v_lhs = Tacky.Var name in
            let v_rhs = emit_tacky e2 instrs in
            let tacky_op = convert_binop op in
            let dst_name = make_temporary () in
            let dst = Tacky.Var dst_name in
            instrs := !instrs @ [Tacky.Binary (tacky_op, v_lhs, v_rhs, dst)];
            instrs := !instrs @ [Tacky.Copy (dst, v_lhs)];
            v_lhs
        | _ -> failwith "Internal Error: Lvalue must be a variable"
      end
  | Ast.PrefixIncrement e ->
      begin
        match e with
        | Ast.Var name ->
            let v_var = Tacky.Var name in
            let one = Tacky.Constant 1 in
            let dst_name = make_temporary () in
            let dst = Tacky.Var dst_name in
            instrs := !instrs @ [Tacky.Binary (Tacky.Add, v_var, one, dst)];
            instrs := !instrs @ [Tacky.Copy (dst, v_var)];
            v_var
        | _ -> failwith "Internal Error: Lvalue must be a variable"
      end
  | Ast.PrefixDecrement e ->
      begin
        match e with
        | Ast.Var name ->
            let v_var = Tacky.Var name in
            let one = Tacky.Constant 1 in
            let dst_name = make_temporary () in
            let dst = Tacky.Var dst_name in
            instrs := !instrs @ [Tacky.Binary (Tacky.Subtract, v_var, one, dst)];
            instrs := !instrs @ [Tacky.Copy (dst, v_var)];
            v_var
        | _ -> failwith "Internal Error: Lvalue must be a variable"
      end
  | Ast.PostfixIncrement e ->
      begin
        match e with
        | Ast.Var name ->
            let v_var = Tacky.Var name in
            let old_val_name = make_temporary () in
            let old_val = Tacky.Var old_val_name in
            instrs := !instrs @ [Tacky.Copy (v_var, old_val)];
            let one = Tacky.Constant 1 in
            let dst_name = make_temporary () in
            let dst = Tacky.Var dst_name in
            instrs := !instrs @ [Tacky.Binary (Tacky.Add, v_var, one, dst)];
            instrs := !instrs @ [Tacky.Copy (dst, v_var)];
            old_val
        | _ -> failwith "Internal Error: Lvalue must be a variable"
      end
  | Ast.PostfixDecrement e ->
      begin
        match e with
        | Ast.Var name ->
            let v_var = Tacky.Var name in
            let old_val_name = make_temporary () in
            let old_val = Tacky.Var old_val_name in
            instrs := !instrs @ [Tacky.Copy (v_var, old_val)];
            let one = Tacky.Constant 1 in
            let dst_name = make_temporary () in
            let dst = Tacky.Var dst_name in
            instrs := !instrs @ [Tacky.Binary (Tacky.Subtract, v_var, one, dst)];
            instrs := !instrs @ [Tacky.Copy (dst, v_var)];
            old_val
        | _ -> failwith "Internal Error: Lvalue must be a variable"
      end

let gen_function (Ast.Function (name, body)) =
  let instrs = ref [] in
  let rec process_item item =
    match item with
    | Ast.S (Ast.Return e) ->
        let v = emit_tacky e instrs in
        instrs := !instrs @ [Tacky.Return v]
    | Ast.S (Ast.Expression e) ->
        let _ = emit_tacky e instrs in
        ()
    | Ast.S Ast.Null ->
        ()
    | Ast.D (Ast.Declaration (name, Some init)) ->
        let v = emit_tacky init instrs in
        instrs := !instrs @ [Tacky.Copy (v, Tacky.Var name)]
    | Ast.D (Ast.Declaration (_, None)) ->
        ()
  in
  List.iter process_item body;
  instrs := !instrs @ [Tacky.Return (Tacky.Constant 0)];
  { Tacky.name = name; Tacky.body = !instrs }

let gen_program (Ast.Program func_def) =
  let tacky_func = gen_function func_def in
  Tacky.Program tacky_func
