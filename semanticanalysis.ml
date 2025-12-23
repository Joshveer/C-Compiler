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

(* --- Pass 1: Identifier Resolution --- *)

type map_entry = { unique_name : string; from_current_block : bool; has_linkage : bool }

let rec resolve_exp exp map =
  match exp with
  | Constant _ -> exp
  | Var name -> (match StringMap.find_opt name map with Some entry -> Var entry.unique_name | None -> fail (sprintf "Undeclared variable: %s" name))
  | Unary (op, e) -> Unary (op, resolve_exp e map)
  | Binary (op, e1, e2) -> Binary (op, resolve_exp e1 map, resolve_exp e2 map)
  | Assignment (e1, e2) -> (match e1 with Var _ -> () | _ -> fail "Invalid lvalue"); Assignment (resolve_exp e1 map, resolve_exp e2 map)
  | CompoundAssignment (op, e1, e2) -> (match e1 with Var _ -> () | _ -> fail "Invalid lvalue"); CompoundAssignment (op, resolve_exp e1 map, resolve_exp e2 map)
  | PrefixIncrement e -> (match e with Var _ -> () | _ -> fail "Invalid lvalue"); PrefixIncrement (resolve_exp e map)
  | PostfixIncrement e -> (match e with Var _ -> () | _ -> fail "Invalid lvalue"); PostfixIncrement (resolve_exp e map)
  | PrefixDecrement e -> (match e with Var _ -> () | _ -> fail "Invalid lvalue"); PrefixDecrement (resolve_exp e map)
  | PostfixDecrement e -> (match e with Var _ -> () | _ -> fail "Invalid lvalue"); PostfixDecrement (resolve_exp e map)
  | Conditional (e1, e2, e3) -> Conditional (resolve_exp e1 map, resolve_exp e2 map, resolve_exp e3 map)
  | FunctionCall (name, args) ->
      let new_name = match StringMap.find_opt name map with Some entry -> entry.unique_name | None -> fail (sprintf "Undeclared function: %s" name) in
      FunctionCall (new_name, List.map (fun a -> resolve_exp a map) args)

let resolve_variable_declaration { vd_name; vd_init } map =
  (match StringMap.find_opt vd_name map with Some entry when entry.from_current_block -> fail (sprintf "Duplicate declaration: %s" vd_name) | _ -> ());
  let unique_name = make_unique_name vd_name in
  let new_map = StringMap.add vd_name { unique_name; from_current_block = true; has_linkage = false } map in
  let resolved_init = Option.map (fun e -> resolve_exp e map) vd_init in
  ({ vd_name = unique_name; vd_init = resolved_init }, new_map)

let rec resolve_block (Block items) map =
  let rec loop acc items map =
    match items with
    | [] -> Block (List.rev acc)
    | D decl :: rest ->
        (match decl with
         | VarDecl vd ->
             let resolved_vd, new_map = resolve_variable_declaration vd map in
             loop (D (VarDecl resolved_vd) :: acc) rest new_map
         | FunDecl fd ->
             (match StringMap.find_opt fd.fd_name map with
              | Some entry when entry.from_current_block && not entry.has_linkage -> fail (sprintf "Function %s redeclared as different kind of symbol" fd.fd_name)
              | _ -> ());
             let new_map = StringMap.add fd.fd_name { unique_name = fd.fd_name; from_current_block = true; has_linkage = true } map in
             let resolved_fd = resolve_function_declaration fd map in
             loop (D (FunDecl resolved_fd) :: acc) rest new_map)
    | S s :: rest ->
        let s = resolve_statement s map in
        loop (S s :: acc) rest map
  in loop [] items map

and resolve_statement stmt map =
  match stmt with
  | Return e -> Return (resolve_exp e map)
  | Expression e -> Expression (resolve_exp e map)
  | If (c, t, e) -> If (resolve_exp c map, resolve_statement t map, Option.map (fun s -> resolve_statement s map) e)
  | Compound b -> Compound (resolve_block b map)
  | Label (l, s) -> Label (l, resolve_statement s map)
  | While (c, b, l) -> While (resolve_exp c map, resolve_statement b map, l)
  | DoWhile (b, c, l) -> DoWhile (resolve_statement b map, resolve_exp c map, l)
  | For (i, c, p, b, l) ->
      let resolved_init, map_with_init = match i with InitDecl d -> let d, m = resolve_variable_declaration d map in (InitDecl d, m) | InitExp e -> (InitExp (Option.map (fun e -> resolve_exp e map) e), map) in
      For (resolved_init, Option.map (fun e -> resolve_exp e map_with_init) c, Option.map (fun e -> resolve_exp e map_with_init) p, resolve_statement b map_with_init, l)
  | Switch (c, b, l, cases) -> Switch (resolve_exp c map, resolve_statement b map, l, cases)
  | Case (e, s, l) -> Case (resolve_exp e map, resolve_statement s map, l)
  | Default (s, l) -> Default (resolve_statement s map, l)
  | _ -> stmt

and resolve_function_declaration fd map =
  if Option.is_some fd.fd_body && StringMap.mem fd.fd_name map && (StringMap.find fd.fd_name map).from_current_block then
    fail (sprintf "Nested function definition: %s" fd.fd_name);
  
  let inner_map = StringMap.add fd.fd_name { unique_name = fd.fd_name; from_current_block = true; has_linkage = true } map in
  let param_map = List.fold_left (fun m p -> 
      if StringMap.mem p m then fail (sprintf "Duplicate parameter: %s" p);
      StringMap.add p { unique_name = make_unique_name p; from_current_block = true; has_linkage = false } m
    ) inner_map fd.fd_params in
  
  let resolved_params = List.map (fun p -> (StringMap.find p param_map).unique_name) fd.fd_params in
  let resolved_body = match fd.fd_body with
    | Some b -> Some (resolve_block b param_map)
    | None -> None
  in
  { fd_name = fd.fd_name; fd_params = resolved_params; fd_body = resolved_body }

(* Label collection and loop annotation logic *)
let rec collect_labels_in_stmt stmt map =
  match stmt with
  | Label (l, s) -> if StringMap.mem l map then fail (sprintf "Duplicate label: %s" l); collect_labels_in_stmt s (StringMap.add l (make_unique_label l) map)
  | If (_, t, e) -> let map = collect_labels_in_stmt t map in Option.fold ~none:map ~some:(fun s -> collect_labels_in_stmt s map) e
  | Compound (Block items) -> List.fold_left (fun acc item -> match item with S s -> collect_labels_in_stmt s acc | D _ -> acc) map items
  | While (_, b, _) | DoWhile (b, _, _) | For (_, _, _, b, _) | Switch (_, b, _, _) -> collect_labels_in_stmt b map
  | Case (_, s, _) | Default (s, _) -> collect_labels_in_stmt s map
  | _ -> map

let rec resolve_labels_in_stmt stmt map =
  match stmt with
  | Goto l -> (match StringMap.find_opt l map with Some unique -> Goto unique | None -> fail (sprintf "Undeclared label: %s" l))
  | Label (l, s) -> Label (StringMap.find l map, resolve_labels_in_stmt s map)
  | If (c, t, e) -> If (c, resolve_labels_in_stmt t map, Option.map (fun s -> resolve_labels_in_stmt s map) e)
  | Compound (Block items) -> Compound (Block (List.map (function S s -> S (resolve_labels_in_stmt s map) | D d -> D d) items))
  | While (c, b, l) -> While (c, resolve_labels_in_stmt b map, l)
  | DoWhile (b, c, l) -> DoWhile (resolve_labels_in_stmt b map, c, l)
  | For (i, c, p, b, l) -> For (i, c, p, resolve_labels_in_stmt b map, l)
  | Switch (c, b, l, cases) -> Switch (c, resolve_labels_in_stmt b map, l, cases)
  | Case (e, s, l) -> Case (e, resolve_labels_in_stmt s map, l)
  | Default (s, l) -> Default (resolve_labels_in_stmt s map, l)
  | _ -> stmt

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
  let rec loop funs map acc =
    match funs with
    | [] -> Program (List.rev acc)
    | fd :: rest ->
        let (match_entry, new_map) = match StringMap.find_opt fd.fd_name map with
          | Some e when e.from_current_block && not e.has_linkage -> fail (sprintf "Function %s redeclared as variable" fd.fd_name)
          | _ -> ({ unique_name = fd.fd_name; from_current_block = true; has_linkage = true }, StringMap.add fd.fd_name { unique_name = fd.fd_name; from_current_block = true; has_linkage = true } map)
        in
        let resolved_fd = resolve_function_declaration fd new_map in
        let final_body = match resolved_fd.fd_body with
          | Some (Block items) ->
               let label_map = collect_labels_in_stmt (Compound (Block items)) StringMap.empty in
               let resolved = resolve_labels_in_stmt (Compound (Block items)) label_map in
               (match resolved with Compound (Block b) -> Block (List.map (function S s -> S (annotate_loops_and_switches s [] []) | D d -> D d) b) | _ -> Block [])
          | None -> Block []
        in
        let final_fd = { resolved_fd with fd_body = if Option.is_some resolved_fd.fd_body then Some final_body else None } in
        loop rest new_map (final_fd :: acc)
  in loop funs StringMap.empty []

(* --- Pass 2: Type Checking --- *)

type type_info = Int | FunType of int * bool (* param_count, defined *)
type symbol_table = type_info StringMap.t

let rec typecheck_exp exp symbols =
  match exp with
  | FunctionCall (name, args) ->
      (match StringMap.find_opt name symbols with
       | Some (FunType (count, _)) ->
           if List.length args <> count then fail (sprintf "Function %s called with wrong number of arguments" name);
           List.iter (fun a -> typecheck_exp a symbols) args
       | Some Int -> fail (sprintf "Called variable %s as a function" name)
       | None -> fail (sprintf "Undeclared function %s" name))
  | Var name ->
      (match StringMap.find_opt name symbols with
       | Some Int -> ()
       | Some (FunType _) -> fail (sprintf "Function %s used as a variable" name)
       | None -> fail "Undeclared variable")
  | Unary (_, e) -> typecheck_exp e symbols
  | Binary (_, e1, e2) -> typecheck_exp e1 symbols; typecheck_exp e2 symbols
  | Assignment (e1, e2) -> typecheck_exp e1 symbols; typecheck_exp e2 symbols
  | _ -> ()

let rec typecheck_block (Block items) symbols =
  List.fold_left (fun syms item ->
    match item with
    | D (VarDecl { vd_name; vd_init }) ->
        Option.iter (fun e -> typecheck_exp e syms) vd_init;
        StringMap.add vd_name Int syms
    | D (FunDecl { fd_name; fd_params; fd_body }) ->
        let new_type = FunType (List.length fd_params, Option.is_some fd_body) in
        (match StringMap.find_opt fd_name syms with
         | Some (FunType (count, defined)) ->
             if count <> List.length fd_params then fail "Function redeclared with different signature";
             if defined && Option.is_some fd_body then fail "Function redefinition";
             StringMap.add fd_name (FunType (count, defined || Option.is_some fd_body)) syms
         | Some Int -> fail "Function name conflicts with variable"
         | None -> StringMap.add fd_name new_type syms)
    | S stmt -> typecheck_statement stmt syms; syms
  ) symbols items

and typecheck_statement stmt symbols =
  match stmt with
  | Return e -> typecheck_exp e symbols
  | Expression e -> typecheck_exp e symbols
  | If (c, t, e) -> typecheck_exp c symbols; typecheck_statement t symbols; Option.iter (fun s -> typecheck_statement s symbols) e
  | Compound b -> ignore (typecheck_block b symbols)
  | While (c, b, _) -> typecheck_exp c symbols; typecheck_statement b symbols
  | DoWhile (b, c, _) -> typecheck_statement b symbols; typecheck_exp c symbols
  | For (init, c, p, b, _) ->
      let syms = match init with InitDecl d -> let _ = typecheck_block (Block [D (VarDecl d)]) symbols in symbols | _ -> symbols in
      Option.iter (fun e -> typecheck_exp e syms) c; Option.iter (fun e -> typecheck_exp e syms) p; typecheck_statement b syms
  | _ -> ()

let typecheck_program (Program funs) =
  ignore (List.fold_left (fun symbols fd ->
      let param_count = List.length fd.fd_params in
      let has_body = Option.is_some fd.fd_body in
      let symbols = match StringMap.find_opt fd.fd_name symbols with
        | Some (FunType (old_count, old_def)) ->
            if old_count <> param_count then fail "Function redeclared with different signature";
            if old_def && has_body then fail "Function redefinition";
            StringMap.add fd.fd_name (FunType (old_count, old_def || has_body)) symbols
        | _ -> StringMap.add fd.fd_name (FunType (param_count, has_body)) symbols
      in
      match fd.fd_body with
      | Some b ->
          let inner_symbols = List.fold_left (fun s p -> StringMap.add p Int s) symbols fd.fd_params in
          ignore (typecheck_block b inner_symbols); symbols
      | None -> symbols
    ) StringMap.empty funs)
