open Ast
open Printf

module StringMap = Map.Make (String)

exception SemanticError of string

let var_counter = ref 0
let label_counter = ref 0
let loop_counter = ref 0
let switch_counter = ref 0

let make_unique_name name =
  let id = !var_counter in incr var_counter; sprintf "%s.%d" name id

let make_unique_label name =
  let id = !label_counter in incr label_counter; sprintf "%s.%d" name id

let make_loop_label () =
  let id = !loop_counter in incr loop_counter; sprintf "loop.%d" id

let make_switch_label () =
  let id = !switch_counter in incr switch_counter; sprintf "switch.%d" id

let make_case_label switch_lbl i = sprintf "case.%s.%d" switch_lbl i
let make_default_label switch_lbl = sprintf "default.%s" switch_lbl
let fail msg = raise (SemanticError msg)

type identifier_info = { params_count : int; is_defined : bool }
type symbol_table = identifier_info StringMap.t
type map_entry = { unique_name : string; from_current_block : bool }

let rec eval_constant_int exp =
  match exp with
  | Constant i -> i
  | Unary (op, e) ->
      let v = eval_constant_int e in
      (match op with Complement -> lnot v | Negate -> -v | Not -> if v = 0 then 1 else 0)
  | Binary (op, e1, e2) ->
      let v1 = eval_constant_int e1 in
      let v2 = eval_constant_int e2 in
      (match op with
       | Add -> v1 + v2 | Subtract -> v1 - v2 | Multiply -> v1 * v2
       | Divide -> if v2 = 0 then fail "Division by zero" else v1 / v2
       | Remainder -> if v2 = 0 then fail "Division by zero" else v1 mod v2
       | BitAnd -> v1 land v2 | BitOr -> v1 lor v2 | Xor -> v1 lxor v2
       | ShiftLeft -> v1 lsl v2 | ShiftRight -> v1 lsr v2
       | Equal -> if v1 = v2 then 1 else 0 | NotEqual -> if v1 <> v2 then 1 else 0
       | LessThan -> if v1 < v2 then 1 else 0 | LessOrEqual -> if v1 <= v2 then 1 else 0
       | GreaterThan -> if v1 > v2 then 1 else 0 | GreaterOrEqual -> if v1 >= v2 then 1 else 0
       | And -> if v1 <> 0 && v2 <> 0 then 1 else 0 | Or -> if v1 <> 0 || v2 <> 0 then 1 else 0)
  | Conditional (e1, e2, e3) -> if eval_constant_int e1 <> 0 then eval_constant_int e2 else eval_constant_int e3
  | _ -> fail "Non-constant expression in case label"

let check_lvalue e = match e with Var _ -> () | _ -> fail "Invalid lvalue"

let rec resolve_exp exp map symbols =
  match exp with
  | Constant _ -> exp
  | Var name -> (match StringMap.find_opt name map with Some entry -> Var entry.unique_name | None -> fail (sprintf "Undeclared variable: %s" name))
  | Unary (op, e) -> Unary (op, resolve_exp e map symbols)
  | Binary (op, e1, e2) -> Binary (op, resolve_exp e1 map symbols, resolve_exp e2 map symbols)
  | Assignment (e1, e2) -> check_lvalue e1; Assignment (resolve_exp e1 map symbols, resolve_exp e2 map symbols)
  | CompoundAssignment (op, e1, e2) -> check_lvalue e1; CompoundAssignment (op, resolve_exp e1 map symbols, resolve_exp e2 map symbols)
  | PrefixIncrement e -> check_lvalue e; PrefixIncrement (resolve_exp e map symbols)
  | PostfixIncrement e -> check_lvalue e; PostfixIncrement (resolve_exp e map symbols)
  | PrefixDecrement e -> check_lvalue e; PrefixDecrement (resolve_exp e map symbols)
  | PostfixDecrement e -> check_lvalue e; PostfixDecrement (resolve_exp e map symbols)
  | Conditional (e1, e2, e3) -> Conditional (resolve_exp e1 map symbols, resolve_exp e2 map symbols, resolve_exp e3 map symbols)
  | FunctionCall (name, args) ->
      match StringMap.find_opt name symbols with
      | Some info ->
          if List.length args <> info.params_count then fail (sprintf "Wrong number of arguments for: %s" name);
          FunctionCall (name, List.map (fun a -> resolve_exp a map symbols) args)
      | None -> fail (sprintf "Undeclared function: %s" name)

let resolve_declaration decl map symbols =
  match decl with
  | Declaration (name, init_opt) ->
      (match StringMap.find_opt name map with Some entry when entry.from_current_block -> fail (sprintf "Duplicate variable declaration: %s" name) | _ -> ());
      let unique_name = make_unique_name name in
      let new_map = StringMap.add name { unique_name; from_current_block = true } map in
      let resolved_init = Option.map (fun e -> resolve_exp e map symbols) init_opt in
      (Declaration (unique_name, resolved_init), new_map)

let rec resolve_statement stmt map symbols =
  match stmt with
  | Return e -> Return (resolve_exp e map symbols)
  | Expression e -> Expression (resolve_exp e map symbols)
  | If (c, t, e) -> If (resolve_exp c map symbols, resolve_statement t map symbols, Option.map (fun s -> resolve_statement s map symbols) e)
  | Compound b -> Compound (resolve_block b map symbols)
  | Label (l, s) -> Label (l, resolve_statement s map symbols)
  | While (c, b, l) -> While (resolve_exp c map symbols, resolve_statement b map symbols, l)
  | DoWhile (b, c, l) -> DoWhile (resolve_statement b map symbols, resolve_exp c map symbols, l)
  | For (i, c, p, b, l) ->
      let resolved_init, map_with_init = match i with InitDecl d -> let d, m = resolve_declaration d map symbols in (InitDecl d, m) | InitExp e -> (InitExp (Option.map (fun e -> resolve_exp e map symbols) e), map) in
      For (resolved_init, Option.map (fun e -> resolve_exp e map_with_init symbols) c, Option.map (fun e -> resolve_exp e map_with_init symbols) p, resolve_statement b map_with_init symbols, l)
  | Switch (c, b, l, cases) -> Switch (resolve_exp c map symbols, resolve_statement b map symbols, l, cases)
  | Case (e, s, l) -> Case (resolve_exp e map symbols, resolve_statement s map symbols, l)
  | Default (s, l) -> Default (resolve_statement s map symbols, l)
  | _ -> stmt

and resolve_block (Block items) map symbols =
  let rec loop acc items map =
    match items with
    | [] -> Block (List.rev acc)
    | D d :: rest -> let d, m = resolve_declaration d map symbols in loop (D d :: acc) rest m
    | S s :: rest -> let s = resolve_statement s map symbols in loop (S s :: acc) rest map
  in loop [] items map

let rec collect_labels_in_stmt stmt map =
  match stmt with
  | Label (l, s) -> if StringMap.mem l map then fail (sprintf "Duplicate label: %s" l); collect_labels_in_stmt s (StringMap.add l (make_unique_label l) map)
  | If (_, t, e) -> let map = collect_labels_in_stmt t map in Option.fold ~none:map ~some:(fun s -> collect_labels_in_stmt s map) e
  | Compound b -> collect_labels_in_block b map
  | While (_, b, _) | DoWhile (b, _, _) | For (_, _, _, b, _) | Switch (_, b, _, _) -> collect_labels_in_stmt b map
  | Case (_, s, _) | Default (s, _) -> collect_labels_in_stmt s map
  | _ -> map

and collect_labels_in_block (Block items) map =
  List.fold_left (fun acc item -> match item with S s -> collect_labels_in_stmt s acc | D _ -> acc) map items

let rec resolve_labels_in_stmt stmt map =
  match stmt with
  | Goto l -> (match StringMap.find_opt l map with Some unique -> Goto unique | None -> fail (sprintf "Undeclared label: %s" l))
  | Label (l, s) -> Label (StringMap.find l map, resolve_labels_in_stmt s map)
  | If (c, t, e) -> If (c, resolve_labels_in_stmt t map, Option.map (fun s -> resolve_labels_in_stmt s map) e)
  | Compound b -> Compound (resolve_labels_in_block b map)
  | While (c, b, l) -> While (c, resolve_labels_in_stmt b map, l)
  | DoWhile (b, c, l) -> DoWhile (resolve_labels_in_stmt b map, c, l)
  | For (i, c, p, b, l) -> For (i, c, p, resolve_labels_in_stmt b map, l)
  | Switch (c, b, l, cases) -> Switch (c, resolve_labels_in_stmt b map, l, cases)
  | Case (e, s, l) -> Case (e, resolve_labels_in_stmt s map, l)
  | Default (s, l) -> Default (resolve_labels_in_stmt s map, l)
  | _ -> stmt

and resolve_labels_in_block (Block items) map =
  Block (List.map (function S s -> S (resolve_labels_in_stmt s map) | D d -> D d) items)

let rec label_cases_in_stmt stmt switch_lbl case_count =
  match stmt with
  | Compound (Block items) -> Compound (Block (List.map (function S s -> S (label_cases_in_stmt s switch_lbl case_count) | D d -> D d) items))
  | If (c, t, e) -> If (c, label_cases_in_stmt t switch_lbl case_count, Option.map (fun s -> label_cases_in_stmt s switch_lbl case_count) e)
  | While (c, b, l) -> While (c, label_cases_in_stmt b switch_lbl case_count, l)
  | DoWhile (b, c, l) -> DoWhile (label_cases_in_stmt b switch_lbl case_count, c, l)
  | For (i, c, p, b, l) -> For (i, c, p, label_cases_in_stmt b switch_lbl case_count, l)
  | Label (l, s) -> Label (l, label_cases_in_stmt s switch_lbl case_count)
  | Switch _ -> stmt
  | Case (e, s, _) -> let lbl = make_case_label switch_lbl !case_count in incr case_count; Case (e, label_cases_in_stmt s switch_lbl case_count, Some lbl)
  | Default (s, _) -> let lbl = make_default_label switch_lbl in Default (label_cases_in_stmt s switch_lbl case_count, Some lbl)
  | _ -> stmt

let rec annotate_loops_and_switches stmt loop_stack switch_stack =
  match stmt with
  | Break _ -> (match loop_stack with l :: _ -> Break (Some l) | [] -> (match switch_stack with s :: _ -> Break (Some s) | [] -> fail "Break outside of loop or switch"))
  | Continue _ -> (match loop_stack with l :: _ -> Continue (Some l) | [] -> fail "Continue outside of loop")
  | While (c, b, _) -> let lbl = make_loop_label () in While (c, annotate_loops_and_switches b (lbl :: loop_stack) switch_stack, Some lbl)
  | DoWhile (b, c, _) -> let lbl = make_loop_label () in DoWhile (annotate_loops_and_switches b (lbl :: loop_stack) switch_stack, c, Some lbl)
  | For (i, c, p, b, _) -> let lbl = make_loop_label () in For (i, c, p, annotate_loops_and_switches b (lbl :: loop_stack) switch_stack, Some lbl)
  | If (c, t, e) -> If (c, annotate_loops_and_switches t loop_stack switch_stack, Option.map (fun s -> annotate_loops_and_switches s loop_stack switch_stack) e)
  | Compound (Block items) -> Compound (Block (List.map (function S s -> S (annotate_loops_and_switches s loop_stack switch_stack) | D d -> D d) items))
  | Label (l, s) -> Label (l, annotate_loops_and_switches s loop_stack switch_stack)
  | Switch (c, b, _, _) ->
      let lbl = make_switch_label () in
      let case_count = ref 0 in
      let body = label_cases_in_stmt b lbl case_count in
      let rec collect_cases s acc =
        match s with
        | Case (e, inner, Some l) -> let acc = collect_cases inner acc in (e, l) :: acc
        | Default (inner, Some l) -> let acc = collect_cases inner acc in acc
        | Compound (Block items) -> List.fold_left (fun a -> function S s -> collect_cases s a | D _ -> a) acc items
        | If (_, t, e) -> let a = collect_cases t acc in Option.fold ~none:a ~some:(fun s -> collect_cases s a) e
        | Label (_, s) | While (_, s, _) | DoWhile (s, _, _) | For (_, _, _, s, _) -> collect_cases s acc
        | _ -> acc
      in
      let rec collect_default s =
        match s with
        | Default (_, Some l) -> Some l
        | Case (_, inner, _) -> collect_default inner
        | Compound (Block items) -> List.fold_left (fun a -> function S s -> (match a with Some _ -> a | None -> collect_default s) | D _ -> a) None items
        | If (_, t, e) -> (match collect_default t with Some l -> Some l | None -> Option.bind e collect_default)
        | Label (_, s) | While (_, s, _) | DoWhile (s, _, _) | For (_, _, _, s, _) -> collect_default s
        | _ -> None
      in
      let cases = { case_list = collect_cases body []; default_label = collect_default body } in
      Switch (c, annotate_loops_and_switches body loop_stack (lbl :: switch_stack), Some lbl, Some cases)
  | Case (e, s, l) -> Case (e, annotate_loops_and_switches s loop_stack switch_stack, l)
  | Default (s, l) -> Default (annotate_loops_and_switches s loop_stack switch_stack, l)
  | _ -> stmt

let resolve_program (Program funs) =
  let rec loop funs symbols acc =
    match funs with
    | [] -> Program (List.rev acc)
    | Function { name; params; body } :: rest ->
        let params_count = List.length params in
        let symbols =
          match StringMap.find_opt name symbols with
          | Some info ->
              if info.params_count <> params_count then fail (sprintf "Redeclaration of %s with different signature" name);
              if info.is_defined && Option.is_some body then fail (sprintf "Redefinition of function: %s" name);
              StringMap.add name { info with is_defined = info.is_defined || Option.is_some body } symbols
          | None -> StringMap.add name { params_count; is_defined = Option.is_some body } symbols
        in
        match body with
        | Some b ->
            let param_map = List.fold_left (fun m p -> if StringMap.mem p m then fail (sprintf "Duplicate parameter name: %s" p); StringMap.add p { unique_name = make_unique_name p; from_current_block = true } m) StringMap.empty params in
            let resolved_body = resolve_block b param_map symbols in
            let label_map = collect_labels_in_block resolved_body StringMap.empty in
            let body_with_labels = resolve_labels_in_block resolved_body label_map in
            let final_body = match body_with_labels with Block items -> Block (List.map (function S s -> S (annotate_loops_and_switches s [] []) | D d -> D d) items) in
            let resolved_params = List.map (fun p -> (StringMap.find p param_map).unique_name) params in
            loop rest symbols (Function { name; params = resolved_params; body = Some final_body } :: acc)
        | None -> loop rest symbols (Function { name; params; body = None } :: acc)
  in loop funs StringMap.empty []
