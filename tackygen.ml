open Ast
open Tacky
open Printf

let tmp_counter = ref 0
let make_temporary () = let name = sprintf "tmp.%d" !tmp_counter in incr tmp_counter; name
let make_label () = let name = sprintf "L.%d" !tmp_counter in incr tmp_counter; name

let convert_unop = function
  | Ast.Complement -> Tacky.Complement | Ast.Negate -> Tacky.Negate | Ast.Not -> Tacky.Not

let convert_binop = function
  | Ast.Add -> Tacky.Add | Ast.Subtract -> Tacky.Subtract | Ast.Multiply -> Tacky.Multiply
  | Ast.Divide -> Tacky.Divide | Ast.Remainder -> Tacky.Remainder | Ast.BitAnd -> Tacky.BitAnd
  | Ast.BitOr -> Tacky.BitOr | Ast.Xor -> Tacky.Xor | Ast.ShiftLeft -> Tacky.ShiftLeft
  | Ast.ShiftRight -> Tacky.ShiftRight | Ast.Equal -> Tacky.Equal | Ast.NotEqual -> Tacky.NotEqual
  | Ast.LessThan -> Tacky.LessThan | Ast.LessOrEqual -> Tacky.LessOrEqual | Ast.GreaterThan -> Tacky.GreaterThan
  | Ast.GreaterOrEqual -> Tacky.GreaterOrEqual | _ -> failwith "Internal Error"

let rec emit_tacky e instrs =
  match e with
  | Ast.Constant c -> Tacky.Constant c
  | Ast.Var v -> Tacky.Var v
  | Ast.Unary (op, inner) ->
      let src = emit_tacky inner instrs in
      let dst = Tacky.Var (make_temporary ()) in
      instrs := !instrs @ [ Tacky.Unary (convert_unop op, src, dst) ];
      dst
  | Ast.Binary (Ast.And, e1, e2) ->
      let false_lbl = make_label () in let end_lbl = make_label () in let dst = Tacky.Var (make_temporary ()) in
      let v1 = emit_tacky e1 instrs in instrs := !instrs @ [ Tacky.JumpIfZero (v1, false_lbl) ];
      let v2 = emit_tacky e2 instrs in instrs := !instrs @ [ Tacky.JumpIfZero (v2, false_lbl) ];
      instrs := !instrs @ [ Tacky.Copy (Tacky.Constant 1, dst); Tacky.Jump end_lbl; Tacky.Label false_lbl; Tacky.Copy (Tacky.Constant 0, dst); Tacky.Label end_lbl ];
      dst
  | Ast.Binary (Ast.Or, e1, e2) ->
      let true_lbl = make_label () in let end_lbl = make_label () in let dst = Tacky.Var (make_temporary ()) in
      let v1 = emit_tacky e1 instrs in instrs := !instrs @ [ Tacky.JumpIfNotZero (v1, true_lbl) ];
      let v2 = emit_tacky e2 instrs in instrs := !instrs @ [ Tacky.JumpIfNotZero (v2, true_lbl) ];
      instrs := !instrs @ [ Tacky.Copy (Tacky.Constant 0, dst); Tacky.Jump end_lbl; Tacky.Label true_lbl; Tacky.Copy (Tacky.Constant 1, dst); Tacky.Label end_lbl ];
      dst
  | Ast.Binary (op, e1, e2) ->
      let v1 = emit_tacky e1 instrs in
      let v2 = emit_tacky e2 instrs in
      let dst = Tacky.Var (make_temporary ()) in
      instrs := !instrs @ [ Tacky.Binary (convert_binop op, v1, v2, dst) ];
      dst
  | Ast.Assignment (e1, e2) ->
      let v = emit_tacky e2 instrs in
      let dst = match e1 with Ast.Var v -> Tacky.Var v | _ -> failwith "Lvalue required" in
      instrs := !instrs @ [ Tacky.Copy (v, dst) ];
      dst
  | Ast.Conditional (c, t, e) ->
      let false_lbl = make_label () in let end_lbl = make_label () in let dst = Tacky.Var (make_temporary ()) in
      let vc = emit_tacky c instrs in instrs := !instrs @ [ Tacky.JumpIfZero (vc, false_lbl) ];
      let vt = emit_tacky t instrs in instrs := !instrs @ [ Tacky.Copy (vt, dst); Tacky.Jump end_lbl; Tacky.Label false_lbl ];
      let ve = emit_tacky e instrs in instrs := !instrs @ [ Tacky.Copy (ve, dst); Tacky.Label end_lbl ];
      dst
  | Ast.FunctionCall (name, args) ->
      let vargs = List.map (fun a -> emit_tacky a instrs) args in
      let dst = Tacky.Var (make_temporary ()) in
      instrs := !instrs @ [ Tacky.FunCall (name, vargs, dst) ];
      dst
  | _ -> failwith "Not implemented"

let rec emit_statement s instrs =
  match s with
  | Ast.Return e ->
      let v = emit_tacky e instrs in
      instrs := !instrs @ [ Tacky.Return v ]
  | Ast.Expression e -> ignore (emit_tacky e instrs)
  | Ast.If (c, t, e) ->
      let else_lbl = make_label () in let end_lbl = make_label () in
      let v = emit_tacky c instrs in
      instrs := !instrs @ [ Tacky.JumpIfZero (v, else_lbl) ];
      emit_statement t instrs;
      instrs := !instrs @ [ Tacky.Jump end_lbl; Tacky.Label else_lbl ];
      (match e with Some s -> emit_statement s instrs | None -> ());
      instrs := !instrs @ [ Tacky.Label end_lbl ]
  | Ast.Compound (Ast.Block items) -> List.iter (function Ast.S s -> emit_statement s instrs | Ast.D _ -> ()) items
  | Ast.While (c, b, lbl) ->
      let start_lbl = match lbl with Some l -> "continue." ^ l | None -> make_label () in
      let end_lbl = match lbl with Some l -> "break." ^ l | None -> make_label () in
      instrs := !instrs @ [ Tacky.Label start_lbl ];
      let v = emit_tacky c instrs in
      instrs := !instrs @ [ Tacky.JumpIfZero (v, end_lbl) ];
      emit_statement b instrs;
      instrs := !instrs @ [ Tacky.Jump start_lbl; Tacky.Label end_lbl ]
  | Ast.DoWhile (b, c, lbl) ->
      let start_lbl = make_label () in
      let cont_lbl = match lbl with Some l -> "continue." ^ l | None -> make_label () in
      let end_lbl = match lbl with Some l -> "break." ^ l | None -> make_label () in
      instrs := !instrs @ [ Tacky.Label start_lbl ];
      emit_statement b instrs;
      instrs := !instrs @ [ Tacky.Label cont_lbl ];
      let v = emit_tacky c instrs in
      instrs := !instrs @ [ Tacky.JumpIfNotZero (v, start_lbl); Tacky.Label end_lbl ]
  | Ast.For (init, cond, post, body, lbl) ->
      let start_lbl = make_label () in
      let cont_lbl = match lbl with Some l -> "continue." ^ l | None -> make_label () in
      let end_lbl = match lbl with Some l -> "break." ^ l | None -> make_label () in
      (match init with Ast.InitDecl _ -> () | Ast.InitExp (Some e) -> ignore (emit_tacky e instrs) | Ast.InitExp None -> ());
      instrs := !instrs @ [ Tacky.Label start_lbl ];
      (match cond with Some e -> let v = emit_tacky e instrs in instrs := !instrs @ [ Tacky.JumpIfZero (v, end_lbl) ] | None -> ());
      emit_statement body instrs;
      instrs := !instrs @ [ Tacky.Label cont_lbl ];
      (match post with Some e -> ignore (emit_tacky e instrs) | None -> ());
      instrs := !instrs @ [ Tacky.Jump start_lbl; Tacky.Label end_lbl ]
  | Ast.Break (Some lbl) -> instrs := !instrs @ [ Tacky.Jump ("break." ^ lbl) ]
  | Ast.Continue (Some lbl) -> instrs := !instrs @ [ Tacky.Jump ("continue." ^ lbl) ]
  | Ast.Null -> ()
  | _ -> failwith "Missing label annotation"

let gen_function fd =
  match fd.fd_body with
  | Some b ->
      let instrs = ref [] in
      emit_statement (Compound b) instrs;
      instrs := !instrs @ [ Tacky.Return (Constant 0) ];
      let global = fd.fd_storage_class <> Some Ast.Static in
      Some (Tacky.Function (fd.fd_name, global, fd.fd_params, !instrs))
  | None -> None

let gen_program (Ast.Program decls) symbols =
  let functions = List.filter_map (function
    | Ast.FunDecl fd -> gen_function fd
    | Ast.VarDecl _ -> None
  ) decls in
  let static_vars = Semanticanalysis.StringMap.fold (fun name entry acc ->
    match entry.Semanticanalysis.sym_attrs with
    | Semanticanalysis.StaticAttr (initial_value, global) ->
        let init_val = match initial_value with
          | Semanticanalysis.Initial i -> i
          | Semanticanalysis.Tentative -> 0
          | Semanticanalysis.NoInitializer -> 0
        in Tacky.StaticVariable (name, global, init_val) :: acc
    | _ -> acc
  ) symbols [] in
  Tacky.Program (functions @ static_vars)
