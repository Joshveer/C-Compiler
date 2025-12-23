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
      instrs := !instrs @ [ Tacky.Unary (convert_unop op, src, dst) ]; dst
  | Ast.Binary (Ast.And, e1, e2) ->
      let false_lbl = make_label () in let end_lbl = make_label () in let dst = Tacky.Var (make_temporary ()) in
      let v1 = emit_tacky e1 instrs in instrs := !instrs @ [ Tacky.JumpIfZero (v1, false_lbl) ];
      let v2 = emit_tacky e2 instrs in instrs := !instrs @ [ Tacky.JumpIfZero (v2, false_lbl) ];
      instrs := !instrs @ [ Tacky.Copy (Tacky.Constant 1, dst); Tacky.Jump end_lbl; Tacky.Label false_lbl; Tacky.Copy (Tacky.Constant 0, dst); Tacky.Label end_lbl ]; dst
  | Ast.Binary (Ast.Or, e1, e2) ->
      let true_lbl = make_label () in let end_lbl = make_label () in let dst = Tacky.Var (make_temporary ()) in
      let v1 = emit_tacky e1 instrs in instrs := !instrs @ [ Tacky.JumpIfNotZero (v1, true_lbl) ];
      let v2 = emit_tacky e2 instrs in instrs := !instrs @ [ Tacky.JumpIfNotZero (v2, true_lbl) ];
      instrs := !instrs @ [ Tacky.Copy (Tacky.Constant 0, dst); Tacky.Jump end_lbl; Tacky.Label true_lbl; Tacky.Copy (Tacky.Constant 1, dst); Tacky.Label end_lbl ]; dst
  | Ast.Binary (op, e1, e2) ->
      let v1 = emit_tacky e1 instrs in let v2 = emit_tacky e2 instrs in let dst = Tacky.Var (make_temporary ()) in
      instrs := !instrs @ [ Tacky.Binary (convert_binop op, v1, v2, dst) ]; dst
  | Ast.Assignment (Var name, right) ->
      let v = emit_tacky right instrs in instrs := !instrs @ [ Tacky.Copy (v, Var name) ]; Var name
  | Ast.CompoundAssignment (op, Var name, right) ->
      let v1 = Tacky.Var name in let v2 = emit_tacky right instrs in let dst = Tacky.Var (make_temporary ()) in
      instrs := !instrs @ [ Tacky.Binary (convert_binop op, v1, v2, dst); Tacky.Copy (dst, Var name) ]; Var name
  | Ast.PrefixIncrement (Var name) -> instrs := !instrs @ [ Tacky.Binary (Tacky.Add, Var name, Constant 1, Var name) ]; Var name
  | Ast.PostfixIncrement (Var name) -> let old = Tacky.Var (make_temporary ()) in instrs := !instrs @ [ Tacky.Copy (Var name, old); Tacky.Binary (Tacky.Add, Var name, Constant 1, Var name) ]; old
  | Ast.PrefixDecrement (Var name) -> instrs := !instrs @ [ Tacky.Binary (Tacky.Subtract, Var name, Constant 1, Var name) ]; Var name
  | Ast.PostfixDecrement (Var name) -> let old = Tacky.Var (make_temporary ()) in instrs := !instrs @ [ Tacky.Copy (Var name, old); Tacky.Binary (Tacky.Subtract, Var name, Constant 1, Var name) ]; old
  | Ast.Conditional (c, t, f) ->
      let end_lbl = make_label () in let false_lbl = make_label () in let dst = Tacky.Var (make_temporary ()) in
      let v_cond = emit_tacky c instrs in instrs := !instrs @ [ Tacky.JumpIfZero (v_cond, false_lbl) ];
      let v_then = emit_tacky t instrs in instrs := !instrs @ [ Tacky.Copy (v_then, dst); Tacky.Jump end_lbl; Tacky.Label false_lbl ];
      let v_else = emit_tacky f instrs in instrs := !instrs @ [ Tacky.Copy (v_else, dst); Tacky.Label end_lbl ]; dst
  | Ast.FunctionCall (name, args) ->
      let args_v = List.map (fun a -> emit_tacky a instrs) args in let dst = Tacky.Var (make_temporary ()) in
      instrs := !instrs @ [ Tacky.FunCall (name, args_v, dst) ]; dst
  | _ -> failwith "Invalid expression"

let rec emit_statement stmt instrs =
  match stmt with
  | Ast.Return e -> let v = emit_tacky e instrs in instrs := !instrs @ [ Tacky.Return v ]
  | Ast.Expression e -> ignore (emit_tacky e instrs)
  | Ast.If (cond, then_s, else_s) ->
      let end_lbl = make_label () in let v = emit_tacky cond instrs in
      (match else_s with
       | Some s -> let else_lbl = make_label () in instrs := !instrs @ [ Tacky.JumpIfZero (v, else_lbl) ]; emit_statement then_s instrs; instrs := !instrs @ [ Tacky.Jump end_lbl; Tacky.Label else_lbl ]; emit_statement s instrs
       | None -> instrs := !instrs @ [ Tacky.JumpIfZero (v, end_lbl) ]; emit_statement then_s instrs);
      instrs := !instrs @ [ Tacky.Label end_lbl ]
  | Ast.Compound (Block items) -> List.iter (function
      | Ast.S s -> emit_statement s instrs
      | Ast.D (Ast.VarDecl { vd_name; vd_init = Some init }) -> let v = emit_tacky init instrs in instrs := !instrs @ [ Tacky.Copy (v, Var vd_name) ]
      | Ast.D _ -> ()
    ) items
  | Ast.Label (name, s) -> instrs := !instrs @ [ Tacky.Label name ]; emit_statement s instrs
  | Ast.Goto name -> instrs := !instrs @ [ Tacky.Jump name ]
  | Ast.While (cond, body, Some lbl) ->
      let cont = "continue." ^ lbl in let brk = "break." ^ lbl in
      instrs := !instrs @ [ Tacky.Label cont ]; let v = emit_tacky cond instrs in instrs := !instrs @ [ Tacky.JumpIfZero (v, brk) ]; emit_statement body instrs; instrs := !instrs @ [ Tacky.Jump cont; Tacky.Label brk ]
  | Ast.DoWhile (body, cond, Some lbl) ->
      let start = "start." ^ lbl in let cont = "continue." ^ lbl in let brk = "break." ^ lbl in
      instrs := !instrs @ [ Tacky.Label start ]; emit_statement body instrs; instrs := !instrs @ [ Tacky.Label cont ]; let v = emit_tacky cond instrs in instrs := !instrs @ [ Tacky.JumpIfNotZero (v, start); Tacky.Label brk ]
  | Ast.For (init, cond, post, body, Some lbl) ->
      let start = "start." ^ lbl in let cont = "continue." ^ lbl in let brk = "break." ^ lbl in
      (match init with InitDecl { vd_name; vd_init = Some e } -> let v = emit_tacky e instrs in instrs := !instrs @ [ Tacky.Copy (v, Var vd_name) ] | InitExp (Some e) -> ignore (emit_tacky e instrs) | _ -> ());
      instrs := !instrs @ [ Tacky.Label start ];
      (match cond with Some e -> let v = emit_tacky e instrs in instrs := !instrs @ [ Tacky.JumpIfZero (v, brk) ] | None -> ());
      emit_statement body instrs; instrs := !instrs @ [ Tacky.Label cont ];
      (match post with Some e -> ignore (emit_tacky e instrs) | None -> ()); instrs := !instrs @ [ Tacky.Jump start; Tacky.Label brk ]
  | Ast.Switch (cond, body, Some lbl, Some cases) ->
      let brk = "break." ^ lbl in let v = emit_tacky cond instrs in
      List.iter (fun (exp, target) -> let dst = Tacky.Var (make_temporary ()) in let ev = emit_tacky exp instrs in instrs := !instrs @ [ Tacky.Binary (Tacky.Equal, v, ev, dst); Tacky.JumpIfNotZero (dst, target) ]) cases.case_list;
      (match cases.default_label with Some l -> instrs := !instrs @ [ Tacky.Jump l ] | None -> instrs := !instrs @ [ Tacky.Jump brk ]);
      emit_statement body instrs; instrs := !instrs @ [ Tacky.Label brk ]
  | Ast.Case (_, stmt, Some lbl) -> instrs := !instrs @ [ Tacky.Label lbl ]; emit_statement stmt instrs
  | Ast.Default (stmt, Some lbl) -> instrs := !instrs @ [ Tacky.Label lbl ]; emit_statement stmt instrs
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
