open Ast
open Tacky
open Printf

let tmp_counter = ref 0

let make_temporary () =
  let name = sprintf "tmp.%d" !tmp_counter in
  incr tmp_counter;
  name

let make_label () = 
  let name = sprintf "L.%d" !tmp_counter in 
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
  | Ast.Conditional (cond, e1, e2) ->
      let e2_lbl = make_label () in
      let end_lbl = make_label () in
      let dst_name = make_temporary () in
      let dst = Tacky.Var dst_name in
      
      let c = emit_tacky cond instrs in
      instrs := !instrs @ [Tacky.JumpIfZero (c, e2_lbl)];
      
      let v1 = emit_tacky e1 instrs in
      instrs := !instrs @ [Tacky.Copy (v1, dst); Tacky.Jump end_lbl];
      
      instrs := !instrs @ [Tacky.Label e2_lbl];
      let v2 = emit_tacky e2 instrs in
      instrs := !instrs @ [Tacky.Copy (v2, dst)];
      
      instrs := !instrs @ [Tacky.Label end_lbl];
      dst

let break_label l = sprintf "break_%s" l
let continue_label l = sprintf "continue_%s" l

let rec emit_statement stmt instrs =
  match stmt with
  | Ast.Return e ->
      let v = emit_tacky e instrs in
      instrs := !instrs @ [Tacky.Return v]
  | Ast.Expression e ->
      let _ = emit_tacky e instrs in
      ()
  | Ast.If (cond, then_stmt, else_stmt_opt) ->
      let else_lbl = make_label () in
      let end_lbl = make_label () in
      let c = emit_tacky cond instrs in
      instrs := !instrs @ [Tacky.JumpIfZero (c, else_lbl)];
      
      emit_statement then_stmt instrs;
      
      begin
        match else_stmt_opt with
        | Some else_stmt ->
            instrs := !instrs @ [Tacky.Jump end_lbl; Tacky.Label else_lbl];
            emit_statement else_stmt instrs;
            instrs := !instrs @ [Tacky.Label end_lbl]
        | None ->
            instrs := !instrs @ [Tacky.Label else_lbl]
      end
  | Ast.Goto target ->
      instrs := !instrs @ [Tacky.Jump target]
  | Ast.Label (label, inner) ->
      instrs := !instrs @ [Tacky.Label label];
      emit_statement inner instrs
  | Ast.Compound (Block items) ->
      let process_item = function
        | Ast.S stmt -> emit_statement stmt instrs
        | Ast.D (Ast.Declaration (name, Some init)) ->
            let v = emit_tacky init instrs in
            instrs := !instrs @ [Tacky.Copy (v, Tacky.Var name)]
        | Ast.D (Ast.Declaration (_, None)) -> ()
      in
      List.iter process_item items
  | Ast.While (cond, body, Some lbl) ->
      let cont_lbl = continue_label lbl in
      let brk_lbl = break_label lbl in
      instrs := !instrs @ [Tacky.Label cont_lbl];
      let c = emit_tacky cond instrs in
      instrs := !instrs @ [Tacky.JumpIfZero (c, brk_lbl)];
      emit_statement body instrs;
      instrs := !instrs @ [Tacky.Jump cont_lbl; Tacky.Label brk_lbl]
  | Ast.DoWhile (body, cond, Some lbl) ->
      let start_lbl = make_label () in
      let cont_lbl = continue_label lbl in
      let brk_lbl = break_label lbl in
      instrs := !instrs @ [Tacky.Label start_lbl];
      emit_statement body instrs;
      instrs := !instrs @ [Tacky.Label cont_lbl];
      let c = emit_tacky cond instrs in
      instrs := !instrs @ [Tacky.JumpIfNotZero (c, start_lbl); Tacky.Label brk_lbl]
  | Ast.For (init, cond, post, body, Some lbl) ->
      let start_lbl = make_label () in
      let cont_lbl = continue_label lbl in
      let brk_lbl = break_label lbl in
      (match init with
       | Ast.InitDecl (Ast.Declaration (name, Some init_exp)) ->
           let v = emit_tacky init_exp instrs in
           instrs := !instrs @ [Tacky.Copy (v, Tacky.Var name)]
       | Ast.InitExp (Some e) -> let _ = emit_tacky e instrs in ()
       | _ -> ());
      instrs := !instrs @ [Tacky.Label start_lbl];
      (match cond with
       | Some c ->
           let v = emit_tacky c instrs in
           instrs := !instrs @ [Tacky.JumpIfZero (v, brk_lbl)]
       | None -> ());
      emit_statement body instrs;
      instrs := !instrs @ [Tacky.Label cont_lbl];
      (match post with
       | Some p -> let _ = emit_tacky p instrs in ()
       | None -> ());
      instrs := !instrs @ [Tacky.Jump start_lbl; Tacky.Label brk_lbl]
  | Ast.Switch (cond, body, Some lbl, Some cases) ->
      let c = emit_tacky cond instrs in
      let brk_lbl = break_label lbl in
      
      (* Generate comparisons for cases *)
      List.iter (fun (exp, target) ->
        let v_exp = emit_tacky exp instrs in
        let tmp = make_temporary () in
        let dst = Tacky.Var tmp in
        (* Calculate e1 == e2 *)
        instrs := !instrs @ [Tacky.Binary (Tacky.Equal, c, v_exp, dst)];
        instrs := !instrs @ [Tacky.JumpIfNotZero (dst, target)]
      ) cases.case_list;
      
      (* Jump to default or break *)
      (match cases.default_label with
       | Some l -> instrs := !instrs @ [Tacky.Jump l]
       | None -> instrs := !instrs @ [Tacky.Jump brk_lbl]);
      
      emit_statement body instrs;
      instrs := !instrs @ [Tacky.Label brk_lbl]
  | Ast.Case (_, stmt, Some lbl) ->
      instrs := !instrs @ [Tacky.Label lbl];
      emit_statement stmt instrs
  | Ast.Default (stmt, Some lbl) ->
      instrs := !instrs @ [Tacky.Label lbl];
      emit_statement stmt instrs
  | Ast.Break (Some lbl) ->
      (* This now contains full label name, no need to prefix *)
      instrs := !instrs @ [Tacky.Jump lbl]
  | Ast.Continue (Some lbl) ->
      instrs := !instrs @ [Tacky.Jump lbl]
  | Ast.Null -> ()
  | _ -> failwith "Loop/Switch statement missing label annotation"

let gen_function (Ast.Function (name, body)) =
  let instrs = ref [] in
  let process_item item =
    match item with
    | Ast.S stmt -> emit_statement stmt instrs
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
