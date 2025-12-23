open Ast
open Printf

module StringMap = Map.Make(String)

exception SemanticError of string

let var_counter = ref 0
let label_counter = ref 0
let loop_counter = ref 0
let switch_counter = ref 0

let make_unique_name name =
  let id = !var_counter in
  incr var_counter;
  sprintf "%s.%d" name id

let make_unique_label name =
  let id = !label_counter in
  incr label_counter;
  sprintf "%s.%d" name id

let make_loop_label () =
  let id = !loop_counter in
  incr loop_counter;
  sprintf "loop.%d" id

let make_switch_label () =
  let id = !switch_counter in
  incr switch_counter;
  sprintf "switch.%d" id

let make_case_label switch_lbl i =
  sprintf "case.%s.%d" switch_lbl i

let make_default_label switch_lbl =
  sprintf "default.%s" switch_lbl

let fail msg =
  raise (SemanticError msg)

type map_entry = {
  unique_name : string;
  from_current_block : bool;
}

(* Constant Evaluation for Switch Cases *)
let rec eval_constant_int exp =
  match exp with
  | Constant i -> i
  | Unary (op, e) ->
      let v = eval_constant_int e in
      (match op with
       | Complement -> lnot v
       | Negate -> -v
       | Not -> if v = 0 then 1 else 0)
  | Binary (op, e1, e2) ->
      let v1 = eval_constant_int e1 in
      let v2 = eval_constant_int e2 in
      (match op with
       | Add -> v1 + v2
       | Subtract -> v1 - v2
       | Multiply -> v1 * v2
       | Divide -> if v2 = 0 then fail "Division by zero in constant expression" else v1 / v2
       | Remainder -> if v2 = 0 then fail "Division by zero in constant expression" else v1 mod v2
       | BitAnd -> v1 land v2
       | BitOr -> v1 lor v2
       | Xor -> v1 lxor v2
       | ShiftLeft -> v1 lsl v2
       | ShiftRight -> v1 lsr v2
       | Equal -> if v1 = v2 then 1 else 0
       | NotEqual -> if v1 <> v2 then 1 else 0
       | LessThan -> if v1 < v2 then 1 else 0
       | LessOrEqual -> if v1 <= v2 then 1 else 0
       | GreaterThan -> if v1 > v2 then 1 else 0
       | GreaterOrEqual -> if v1 >= v2 then 1 else 0
       | And -> if v1 <> 0 && v2 <> 0 then 1 else 0
       | Or -> if v1 <> 0 || v2 <> 0 then 1 else 0)
  | Conditional (e1, e2, e3) ->
      if eval_constant_int e1 <> 0 then eval_constant_int e2 else eval_constant_int e3
  | _ -> fail "Non-constant expression in case label"

let check_lvalue e =
  match e with
  | Var _ -> ()
  | _ -> fail "Invalid lvalue"

let rec resolve_exp exp map =
  match exp with
  | Constant _ -> exp
  | Var name ->
      (match StringMap.find_opt name map with
       | Some entry -> Var entry.unique_name
       | None -> fail (sprintf "Undeclared variable: %s" name))
  | Unary (op, e) ->
      Unary (op, resolve_exp e map)
  | Binary (op, e1, e2) ->
      Binary (op, resolve_exp e1 map, resolve_exp e2 map)
  | Assignment (e1, e2) ->
      check_lvalue e1;
      let resolved_left = resolve_exp e1 map in
      let resolved_right = resolve_exp e2 map in
      Assignment (resolved_left, resolved_right)
  | CompoundAssignment (op, e1, e2) ->
      check_lvalue e1;
      let resolved_left = resolve_exp e1 map in
      let resolved_right = resolve_exp e2 map in
      CompoundAssignment (op, resolved_left, resolved_right)
  | PrefixIncrement e ->
      check_lvalue e;
      PrefixIncrement (resolve_exp e map)
  | PostfixIncrement e ->
      check_lvalue e;
      PostfixIncrement (resolve_exp e map)
  | PrefixDecrement e ->
      check_lvalue e;
      PrefixDecrement (resolve_exp e map)
  | PostfixDecrement e ->
      check_lvalue e;
      PostfixDecrement (resolve_exp e map)
  | Conditional (e1, e2, e3) ->
      let r1 = resolve_exp e1 map in
      let r2 = resolve_exp e2 map in
      let r3 = resolve_exp e3 map in
      Conditional (r1, r2, r3)

let resolve_declaration decl map =
  match decl with
  | Declaration (name, init_opt) ->
      (match StringMap.find_opt name map with
       | Some entry when entry.from_current_block ->
           fail (sprintf "Duplicate variable declaration: %s" name)
       | _ -> ());
      let unique_name = make_unique_name name in
      let new_entry = { unique_name; from_current_block = true } in
      let new_map = StringMap.add name new_entry map in
      let resolved_init =
        match init_opt with
        | Some init -> Some (resolve_exp init new_map)
        | None -> None
      in
      (Declaration (unique_name, resolved_init), new_map)

let copy_variable_map map =
  StringMap.map (fun entry -> { entry with from_current_block = false }) map

let rec resolve_block_item item map =
  match item with
  | D decl ->
      let (resolved_decl, new_map) = resolve_declaration decl map in
      (D resolved_decl, new_map)
  | S stmt ->
      (S (resolve_statement stmt map), map)

and resolve_statement stmt map =
  match stmt with
  | Return e -> Return (resolve_exp e map)
  | Expression e -> Expression (resolve_exp e map)
  | Null -> Null
  | If (cond, then_s, else_s_opt) ->
      let cond = resolve_exp cond map in
      let then_s = resolve_statement then_s map in
      let else_s_opt =
        match else_s_opt with
        | Some s -> Some (resolve_statement s map)
        | None -> None
      in
      If (cond, then_s, else_s_opt)
  | Goto _ -> stmt
  | Label (name, inner) ->
      Label (name, resolve_statement inner map)
  | Compound (Block items) ->
      let new_map = copy_variable_map map in
      let rec resolve_items items map acc =
        match items with
        | [] -> List.rev acc
        | item :: rest ->
            let (resolved_item, new_map) = resolve_block_item item map in
            resolve_items rest new_map (resolved_item :: acc)
      in
      let resolved_items = resolve_items items new_map [] in
      Compound (Block resolved_items)
  | While (cond, body, lbl) ->
      let cond = resolve_exp cond map in
      let body = resolve_statement body map in
      While (cond, body, lbl)
  | DoWhile (body, cond, lbl) ->
      let body = resolve_statement body map in
      let cond = resolve_exp cond map in
      DoWhile (body, cond, lbl)
  | For (init, cond, post, body, lbl) ->
      let new_map = copy_variable_map map in
      let (resolved_init, new_map) =
        match init with
        | InitDecl d ->
             let (d, m) = resolve_declaration d new_map in
             (InitDecl d, m)
        | InitExp (Some e) ->
             (InitExp (Some (resolve_exp e new_map)), new_map)
        | InitExp None ->
             (InitExp None, new_map)
      in
      let resolved_cond = Option.map (fun e -> resolve_exp e new_map) cond in
      let resolved_post = Option.map (fun e -> resolve_exp e new_map) post in
      let resolved_body = resolve_statement body new_map in
      For (resolved_init, resolved_cond, resolved_post, resolved_body, lbl)
  | Switch (cond, body, lbl, cases) ->
      let cond = resolve_exp cond map in
      let body = resolve_statement body map in
      Switch (cond, body, lbl, cases)
  | Case (exp, inner, lbl) ->
      (* We resolve the inner statement here. Constant validation happens in label_loops_in_stmt *)
      Case (exp, resolve_statement inner map, lbl)
  | Default (inner, lbl) ->
      Default (resolve_statement inner map, lbl)
  | Break _ | Continue _ -> stmt

(* Label Resolution Logic *)
let rec collect_labels_in_stmt stmt map =
  match stmt with
  | Label (name, inner) ->
      if StringMap.mem name map then fail (sprintf "Duplicate label: %s" name);
      let unique = make_unique_label name in
      let map = StringMap.add name unique map in
      collect_labels_in_stmt inner map
  | If (_, then_s, else_s_opt) ->
      let map = collect_labels_in_stmt then_s map in
      (match else_s_opt with
       | Some s -> collect_labels_in_stmt s map
       | None -> map)
  | Compound (Block items) ->
      collect_labels_in_block items map
  | While (_, body, _) | DoWhile (body, _, _) | For (_, _, _, body, _) | Switch (_, body, _, _) 
  | Case (_, body, _) | Default (body, _) ->
      collect_labels_in_stmt body map
  | _ -> map

and collect_labels_in_block items map =
  match items with
  | [] -> map
  | S stmt :: rest ->
      let map = collect_labels_in_stmt stmt map in
      collect_labels_in_block rest map
  | D _ :: rest -> collect_labels_in_block rest map

let rec resolve_labels_in_stmt stmt map =
  match stmt with
  | Label (name, inner) ->
      Label (StringMap.find name map, resolve_labels_in_stmt inner map)
  | Goto name ->
      if not (StringMap.mem name map) then fail (sprintf "Undeclared label: %s" name);
      Goto (StringMap.find name map)
  | If (cond, then_s, else_s_opt) ->
      let then_s = resolve_labels_in_stmt then_s map in
      let else_s_opt =
        match else_s_opt with
        | Some s -> Some (resolve_labels_in_stmt s map)
        | None -> None
      in
      If (cond, then_s, else_s_opt)
  | Compound (Block items) ->
      Compound (Block (resolve_labels_in_block items map))
  | While (c, body, l) ->
      While (c, resolve_labels_in_stmt body map, l)
  | DoWhile (body, c, l) ->
      DoWhile (resolve_labels_in_stmt body map, c, l)
  | For (i, c, p, body, l) ->
      For (i, c, p, resolve_labels_in_stmt body map, l)
  | Switch (c, body, l, cases) ->
      Switch (c, resolve_labels_in_stmt body map, l, cases)
  | Case (e, s, l) -> Case (e, resolve_labels_in_stmt s map, l)
  | Default (s, l) -> Default (resolve_labels_in_stmt s map, l)
  | _ -> stmt

and resolve_labels_in_block items map =
  match items with
  | [] -> []
  | S stmt :: rest ->
      S (resolve_labels_in_stmt stmt map) :: resolve_labels_in_block rest map
  | D decl :: rest ->
      D decl :: resolve_labels_in_block rest map

(* Collect Switch Cases *)
let rec collect_switch_cases_in_stmt stmt switch_lbl case_count = 
  match stmt with
  | Case (exp, inner, _) ->
      let lbl = make_case_label switch_lbl !case_count in
      incr case_count;
      let (cases, def) = collect_switch_cases_in_stmt inner switch_lbl case_count in
      ((exp, lbl) :: cases, def)
  | Default (inner, _) ->
      let lbl = make_default_label switch_lbl in
      let (cases, def) = collect_switch_cases_in_stmt inner switch_lbl case_count in
      (match def with
       | Some _ -> fail "Duplicate default statement"
       | None -> (cases, Some lbl))
  | Compound (Block items) ->
      collect_switch_cases_in_block items switch_lbl case_count
  | If (_, then_s, else_s) ->
      let (c1, d1) = collect_switch_cases_in_stmt then_s switch_lbl case_count in
      let (c2, d2) = 
        match else_s with
        | Some s -> collect_switch_cases_in_stmt s switch_lbl case_count
        | None -> ([], None)
      in
      let def = match (d1, d2) with (Some _, Some _) -> fail "Duplicate default" | (Some d, _) -> Some d | (_, Some d) -> Some d | _ -> None in
      (c1 @ c2, def)
  | While (_, body, _) | DoWhile (body, _, _) | For (_, _, _, body, _) ->
      collect_switch_cases_in_stmt body switch_lbl case_count
  | Switch _ -> ([], None) 
  | Label (_, s) -> collect_switch_cases_in_stmt s switch_lbl case_count
  | _ -> ([], None)

and collect_switch_cases_in_block items switch_lbl case_count =
  List.fold_left (fun (acc_c, acc_d) item ->
    match item with
    | S s -> 
        let (c, d) = collect_switch_cases_in_stmt s switch_lbl case_count in
        let new_d = match (acc_d, d) with (Some _, Some _) -> fail "Duplicate default" | (Some d, _) -> Some d | (_, Some d) -> Some d | _ -> None in
        (acc_c @ c, new_d)
    | D _ -> (acc_c, acc_d)
  ) ([], None) items

(* Loop/Switch Labeling Pass *)
let rec label_loops_in_stmt stmt break_target continue_target =
  match stmt with
  | While (c, body, _) ->
      let loop_id = make_loop_label () in
      let break_lbl = sprintf "break_%s" loop_id in
      let continue_lbl = sprintf "continue_%s" loop_id in
      let body = label_loops_in_stmt body (Some break_lbl) (Some continue_lbl) in
      While (c, body, Some loop_id)
  | DoWhile (body, c, _) ->
      let loop_id = make_loop_label () in
      let break_lbl = sprintf "break_%s" loop_id in
      let continue_lbl = sprintf "continue_%s" loop_id in
      let body = label_loops_in_stmt body (Some break_lbl) (Some continue_lbl) in
      DoWhile (body, c, Some loop_id)
  | For (i, c, p, body, _) ->
      let loop_id = make_loop_label () in
      let break_lbl = sprintf "break_%s" loop_id in
      let continue_lbl = sprintf "continue_%s" loop_id in
      let body = label_loops_in_stmt body (Some break_lbl) (Some continue_lbl) in
      For (i, c, p, body, Some loop_id)
  | Switch (c, body, _, _) ->
      let switch_id = make_switch_label () in
      let break_lbl = sprintf "break_%s" switch_id in
      (* continue passes through from enclosing loop *)
      let body = label_loops_in_stmt body (Some break_lbl) continue_target in
      
      (* Collect cases *)
      let case_count = ref 0 in
      let (cases, def) = collect_switch_cases_in_stmt body switch_id case_count in
      
      (* Validate Case Constants and Duplicates *)
      let seen_cases = Hashtbl.create 16 in
      List.iter (fun (exp, _) ->
        let val_int = eval_constant_int exp in
        if Hashtbl.mem seen_cases val_int then
          fail "Duplicate case value"
        else
          Hashtbl.add seen_cases val_int ()
      ) cases;

      (* Also need to annotate the Case/Default nodes inside body with their labels *)
      let body = label_cases_in_stmt body switch_id (ref 0) in
      Switch (c, body, Some switch_id, Some { case_list = cases; default_label = def })
  | Break _ ->
      (match break_target with
       | Some l -> Break (Some l)
       | None -> fail "Break outside of loop or switch")
  | Continue _ ->
      (match continue_target with
       | Some l -> Continue (Some l)
       | None -> fail "Continue outside of loop")
  | If (c, then_s, else_s) ->
      let then_s = label_loops_in_stmt then_s break_target continue_target in
      let else_s = Option.map (fun s -> label_loops_in_stmt s break_target continue_target) else_s in
      If (c, then_s, else_s)
  | Compound (Block items) ->
      let items = List.map (function
        | S s -> S (label_loops_in_stmt s break_target continue_target)
        | D d -> D d) items in
      Compound (Block items)
  | Label (l, s) -> Label (l, label_loops_in_stmt s break_target continue_target)
  | Case (e, s, _) -> 
      Case (e, label_loops_in_stmt s break_target continue_target, None) 
  | Default (s, _) -> 
      Default (label_loops_in_stmt s break_target continue_target, None)
  | _ -> stmt

and label_cases_in_stmt stmt switch_lbl case_count =
  match stmt with
  | Case (e, s, _) ->
      let lbl = make_case_label switch_lbl !case_count in
      incr case_count;
      Case (e, label_cases_in_stmt s switch_lbl case_count, Some lbl)
  | Default (s, _) ->
      let lbl = make_default_label switch_lbl in
      Default (label_cases_in_stmt s switch_lbl case_count, Some lbl)
  | Compound (Block items) ->
      let items = List.map (function
        | S s -> S (label_cases_in_stmt s switch_lbl case_count)
        | D d -> D d) items in
      Compound (Block items)
  | If (c, t, e) ->
      let t = label_cases_in_stmt t switch_lbl case_count in
      let e = Option.map (fun s -> label_cases_in_stmt s switch_lbl case_count) e in
      If (c, t, e)
  | While (c, b, l) -> While (c, label_cases_in_stmt b switch_lbl case_count, l)
  | DoWhile (b, c, l) -> DoWhile (label_cases_in_stmt b switch_lbl case_count, c, l)
  | For (i, c, p, b, l) -> For (i, c, p, label_cases_in_stmt b switch_lbl case_count, l)
  | Label (l, s) -> Label (l, label_cases_in_stmt s switch_lbl case_count)
  | Switch _ -> stmt 
  | _ -> stmt

let resolve_function (Function (name, body)) =
  let label_map = collect_labels_in_block body StringMap.empty in
  let body_with_resolved_labels = resolve_labels_in_block body label_map in
  let rec resolve_vars items map acc =
    match items with
    | [] -> List.rev acc
    | item :: rest ->
        let (resolved_item, new_map) = resolve_block_item item map in
        resolve_vars rest new_map (resolved_item :: acc)
  in
  let body_resolved_vars = resolve_vars body_with_resolved_labels StringMap.empty [] in
  let body_labeled_loops = List.map (function
    | S s -> S (label_loops_in_stmt s None None)
    | D d -> D d) body_resolved_vars in
  Function (name, body_labeled_loops)

let resolve_program (Program func_def) =
  Program (resolve_function func_def)
