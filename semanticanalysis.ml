open Ast
open Printf

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

exception SemanticError of string
let fail msg = raise (SemanticError msg)

let var_counter = ref 0
let label_counter = ref 0

let make_unique_name name =
  let id = !var_counter in incr var_counter; sprintf "%s.%d" name id

let make_label name =
  let id = !label_counter in incr label_counter; sprintf "%s.%d" name id

type map_entry = {
  unique_name : string;
  has_linkage : bool;
}

type identifier_map = {
  symbols : map_entry StringMap.t;
  current_scope : StringSet.t;
}

type resolve_context = {
  break_label : string option;
  continue_label : string option;
  current_switch : (switch_cases ref) option;
}

let empty_map = { symbols = StringMap.empty; current_scope = StringSet.empty }
let default_ctx = { break_label = None; continue_label = None; current_switch = None }

let enter_scope map = { map with current_scope = StringSet.empty }

let add_entry name entry map =
  { symbols = StringMap.add name entry map.symbols;
    current_scope = StringSet.add name map.current_scope }

let find_entry name map = StringMap.find_opt name map.symbols
let in_current_scope name map = StringSet.mem name map.current_scope

let rec resolve_exp exp map =
  match exp with
  | Constant _ -> exp
  | Var name ->
      (match find_entry name map with
       | Some entry -> Var entry.unique_name
       | None -> fail (sprintf "Undeclared identifier: %s" name))
  | Unary (op, e) -> Unary (op, resolve_exp e map)
  | Binary (op, e1, e2) -> Binary (op, resolve_exp e1 map, resolve_exp e2 map)
  | Assignment (e1, e2) -> Assignment (resolve_exp e1 map, resolve_exp e2 map)
  | CompoundAssignment (op, e1, e2) -> CompoundAssignment (op, resolve_exp e1 map, resolve_exp e2 map)
  | PrefixIncrement e -> PrefixIncrement (resolve_exp e map)
  | PostfixIncrement e -> PostfixIncrement (resolve_exp e map)
  | PrefixDecrement e -> PrefixDecrement (resolve_exp e map)
  | PostfixDecrement e -> PostfixDecrement (resolve_exp e map)
  | Conditional (c, t, e) -> Conditional (resolve_exp c map, resolve_exp t map, resolve_exp e map)
  | FunctionCall (name, args) ->
      (match find_entry name map with
       | Some entry ->
           FunctionCall (entry.unique_name, List.map (fun a -> resolve_exp a map) args)
       | None -> fail (sprintf "Undeclared function: %s" name))

and resolve_statement stmt map ctx =
  match stmt with
  | Return e -> Return (resolve_exp e map)
  | Expression e -> Expression (resolve_exp e map)
  | If (c, t, e) ->
      If (resolve_exp c map,
          resolve_statement t map ctx,
          Option.map (fun s -> resolve_statement s map ctx) e)
  | Compound block -> Compound (resolve_block block map ctx)
  | While (c, b, _) ->
      let lbl = make_label "loop" in
      let inner_ctx = { ctx with break_label = Some lbl; continue_label = Some lbl } in
      While (resolve_exp c map, resolve_statement b map inner_ctx, Some lbl)
  | DoWhile (b, c, _) ->
      let lbl = make_label "loop" in
      let inner_ctx = { ctx with break_label = Some lbl; continue_label = Some lbl } in
      DoWhile (resolve_statement b map inner_ctx, resolve_exp c map, Some lbl)
  | For (init, c, p, b, _) ->
      let inner_map = enter_scope map in
      let (res_init, map_with_init) = resolve_for_init init inner_map in
      let lbl = make_label "loop" in
      let inner_ctx = { ctx with break_label = Some lbl; continue_label = Some lbl } in
      For (res_init,
           Option.map (fun e -> resolve_exp e map_with_init) c,
           Option.map (fun e -> resolve_exp e map_with_init) p,
           resolve_statement b map_with_init inner_ctx,
           Some lbl)
  | Switch (e, b, _, _) ->
      let lbl = make_label "switch" in
      let cases_ref = ref { case_list = []; default_label = None } in
      let inner_ctx = { ctx with break_label = Some lbl; current_switch = Some cases_ref } in
      let res_body = resolve_statement b map inner_ctx in
      Switch (resolve_exp e map, res_body, Some lbl, Some !cases_ref)
  | Case (e, s, _) ->
      (match ctx.current_switch with
       | Some cases_ref ->
           let lbl = make_label "case" in
           let res_e = resolve_exp e map in
           cases_ref := { !cases_ref with case_list = (res_e, lbl) :: (!cases_ref).case_list };
           Case (res_e, resolve_statement s map ctx, Some lbl)
       | None -> fail "Case statement not in switch")
  | Default (s, _) ->
      (match ctx.current_switch with
       | Some cases_ref ->
           if Option.is_some (!cases_ref).default_label then fail "Multiple default labels in switch";
           let lbl = make_label "default" in
           cases_ref := { !cases_ref with default_label = Some lbl };
           Default (resolve_statement s map ctx, Some lbl)
       | None -> fail "Default statement not in switch")
  | Break _ ->
      (match ctx.break_label with
       | Some lbl -> Break (Some lbl)
       | None -> fail "Break not in loop or switch")
  | Continue _ ->
      (match ctx.continue_label with
       | Some lbl -> Continue (Some lbl)
       | None -> fail "Continue not in loop")
  | Goto l -> Goto l
  | Label (l, s) -> Label (l, resolve_statement s map ctx)
  | Null -> Null

and resolve_for_init init map =
  match init with
  | InitDecl vd ->
      let (res_decl, new_map) = resolve_declaration (VarDecl vd) map ~is_top_level:false in
      (match res_decl with VarDecl rvd -> (InitDecl rvd, new_map) | _ -> failwith "Logic error")
  | InitExp e -> (InitExp (Option.map (fun x -> resolve_exp x map) e), map)

and resolve_block (Block items) map ctx =
  Block (resolve_block_items items (enter_scope map) ctx)

and resolve_block_items items map ctx =
  match items with
  | [] -> []
  | D decl :: rest ->
      let (res_d, next_map) = resolve_declaration decl map ~is_top_level:false in
      D res_d :: resolve_block_items rest next_map ctx
  | S stmt :: rest ->
      S (resolve_statement stmt map ctx) :: resolve_block_items rest map ctx

and resolve_declaration decl map ~is_top_level =
  match decl with
  | VarDecl vd ->
      if in_current_scope vd.vd_name map && not is_top_level then fail (sprintf "Duplicate declaration of %s" vd.vd_name);
      let has_linkage = is_top_level || vd.vd_storage_class = Some Ast.Extern in
      let unique = if has_linkage then vd.vd_name else make_unique_name vd.vd_name in
      let new_map = add_entry vd.vd_name { unique_name = unique; has_linkage = has_linkage } map in
      let res_init = Option.map (fun e -> resolve_exp e new_map) vd.vd_init in
      (VarDecl { vd_name = unique; vd_init = res_init; vd_storage_class = vd.vd_storage_class }, new_map)
  | FunDecl fd ->
      if (not is_top_level) && Option.is_some fd.fd_body then fail "Nested function definition";
      if in_current_scope fd.fd_name map then (
        match find_entry fd.fd_name map with
        | Some e when e.has_linkage -> () (* OK, redeclaring external *)
        | _ -> fail (sprintf "Duplicate declaration of %s" fd.fd_name)
      );
      let unique = fd.fd_name in
      let new_map = add_entry fd.fd_name { unique_name = unique; has_linkage = true } map in
      let param_map_base = enter_scope new_map in
      let param_map = List.fold_left (fun m p ->
          if in_current_scope p m then fail "Duplicate parameter name";
          add_entry p { unique_name = make_unique_name p; has_linkage = false } m
        ) param_map_base fd.fd_params in
      let res_params = List.map (fun p -> (Option.get (find_entry p param_map)).unique_name) fd.fd_params in
      let res_body = Option.map (fun (Block items) ->
        Block (resolve_block_items items param_map default_ctx)
      ) fd.fd_body in
      (FunDecl { fd with fd_params = res_params; fd_body = res_body }, new_map)

let resolve_program (Program decls) =
  let resolve_one (m, acc) decl =
    let (res_d, next_map) = resolve_declaration decl m ~is_top_level:true in
    (next_map, res_d :: acc)
  in
  let (_, reversed_decls) = List.fold_left resolve_one (empty_map, []) decls in
  Program (List.rev reversed_decls)

type initial_value = Tentative | Initial of int | NoInitializer

type identifier_attrs =
  | FunAttr of bool * bool (* defined, global *)
  | StaticAttr of initial_value * bool (* init, global *)
  | LocalAttr

type symbol_type = Int | FunType of int * bool

type symbol_entry = {
  sym_type : symbol_type;
  sym_attrs : identifier_attrs;
}

let rec typecheck_exp exp symbols =
  match exp with
  | Constant _ -> Int
  | Var name ->
      (match StringMap.find_opt name symbols with
       | Some entry ->
           (match entry.sym_attrs with
            | FunAttr _ -> fail (sprintf "Function %s used as variable" name)
            | _ -> entry.sym_type)
       | None -> fail (sprintf "Internal error: Undeclared %s in Pass 2" name))
  | Unary (_, e) -> typecheck_exp e symbols
  | Binary (_, e1, e2) ->
      ignore (typecheck_exp e1 symbols); ignore (typecheck_exp e2 symbols); Int
  | Assignment (e1, e2) | CompoundAssignment (_, e1, e2) ->
      check_lvalue e1 symbols; ignore (typecheck_exp e1 symbols); ignore (typecheck_exp e2 symbols); Int
  | PrefixIncrement e | PostfixIncrement e | PrefixDecrement e | PostfixDecrement e ->
      check_lvalue e symbols; ignore (typecheck_exp e symbols); Int
  | Conditional (c, t, e) ->
      ignore (typecheck_exp c symbols); ignore (typecheck_exp t symbols); ignore (typecheck_exp e symbols); Int
  | FunctionCall (name, args) ->
      (match StringMap.find_opt name symbols with
       | Some entry ->
           (match entry.sym_type with
            | FunType (arity, _) ->
                if List.length args <> arity then fail "Argument count mismatch";
                List.iter (fun a -> ignore (typecheck_exp a symbols)) args;
                Int
            | Int -> fail (sprintf "Variable %s used as function" name))
       | None -> fail (sprintf "Undeclared function %s" name))

and check_lvalue exp symbols =
  match exp with
  | Var name ->
      (match StringMap.find_opt name symbols with
       | Some entry ->
           (match entry.sym_attrs with
            | FunAttr _ -> fail "Assignment to function"
            | _ -> ())
       | None -> fail "Undeclared")
  | _ -> fail "Invalid lvalue"

let rec typecheck_statement stmt symbols =
  match stmt with
  | Return e | Expression e -> ignore (typecheck_exp e symbols)
  | If (c, t, e) ->
      ignore (typecheck_exp c symbols); typecheck_statement t symbols; Option.iter (fun s -> typecheck_statement s symbols) e
  | Compound (Block items) -> ignore (typecheck_block_items items symbols)
  | While (c, b, _) | DoWhile (b, c, _) ->
      ignore (typecheck_exp c symbols); typecheck_statement b symbols
  | For (init, c, p, b, _) ->
      let inner_symbols = match init with
        | InitDecl vd ->
             let s = StringMap.add vd.vd_name { sym_type = Int; sym_attrs = LocalAttr } symbols in
             Option.iter (fun e -> ignore (typecheck_exp e s)) vd.vd_init; s
        | InitExp e -> Option.iter (fun x -> ignore (typecheck_exp x symbols)) e; symbols
      in
      Option.iter (fun e -> ignore (typecheck_exp e inner_symbols)) c;
      Option.iter (fun e -> ignore (typecheck_exp e inner_symbols)) p;
      typecheck_statement b inner_symbols
  | Switch (e, b, _, _) -> ignore (typecheck_exp e symbols); typecheck_statement b symbols
  | Case (e, s, _) -> ignore (typecheck_exp e symbols); typecheck_statement s symbols
  | Default (s, _) | Label (_, s) -> typecheck_statement s symbols
  | Break _ | Continue _ | Goto _ | Null -> ()

and typecheck_block_items items symbols =
  List.fold_left (fun s item ->
    match item with
    | D (VarDecl vd) ->
        (match vd.vd_storage_class with
         | Some Ast.Extern ->
             if Option.is_some vd.vd_init then fail "Initializer on local extern variable declaration";
             (match StringMap.find_opt vd.vd_name s with
              | Some _ -> s  (* Already exists, don't add local *)
              | None -> StringMap.add vd.vd_name { sym_type = Int; sym_attrs = StaticAttr (NoInitializer, true) } s)
         | _ ->
             let s' = StringMap.add vd.vd_name { sym_type = Int; sym_attrs = LocalAttr } s in
             Option.iter (fun e -> ignore (typecheck_exp e s')) vd.vd_init; s')
    | D (FunDecl fd) ->
        let arity = List.length fd.fd_params in
        let has_body = Option.is_some fd.fd_body in
        let global = fd.fd_storage_class <> Some Ast.Static in
        (match StringMap.find_opt fd.fd_name s with
         | Some entry ->
             (match entry.sym_type with
              | FunType (old_arity, old_has_body) ->
                  if old_arity <> arity then fail "Signature mismatch";
                  if old_has_body && has_body then fail "Function redefinition";
                  let attrs = FunAttr (old_has_body || has_body, global) in
                  StringMap.add fd.fd_name { sym_type = FunType (arity, old_has_body || has_body); sym_attrs = attrs } s
              | Int -> fail "Conflict with variable")
         | None ->
             let attrs = FunAttr (has_body, global) in
             StringMap.add fd.fd_name { sym_type = FunType (arity, has_body); sym_attrs = attrs } s)
    | S stmt -> typecheck_statement stmt s; s
  ) symbols items

let typecheck_program (Program decls) =
  let process_declaration s decl =
    match decl with
    | FunDecl fd ->
        let arity = List.length fd.fd_params in
        let has_body = Option.is_some fd.fd_body in
        let global = fd.fd_storage_class <> Some Ast.Static in
        (match StringMap.find_opt fd.fd_name s with
         | Some entry ->
             (match entry.sym_type with
              | FunType (old_arity, old_has_body) ->
                  if old_arity <> arity then fail "Signature mismatch";
                  if old_has_body && has_body then fail "Function redefinition";
                  let global = match entry.sym_attrs with FunAttr (_, g) -> g | _ -> fail "Type mismatch" in
                  let attrs = FunAttr (old_has_body || has_body, global) in
                  StringMap.add fd.fd_name { sym_type = FunType (arity, old_has_body || has_body); sym_attrs = attrs } s
              | Int -> fail "Conflict")
         | None ->
             let attrs = FunAttr (has_body, global) in
             StringMap.add fd.fd_name { sym_type = FunType (arity, has_body); sym_attrs = attrs } s)
    | VarDecl vd ->
        let global = vd.vd_storage_class <> Some Ast.Static in
        let initial_value = match vd.vd_init with
          | Some (Ast.Constant i) -> Initial i
          | Some _ -> fail "Non-constant initializer"
          | None -> if global then NoInitializer else Tentative
        in
        let attrs = StaticAttr (initial_value, global) in
        StringMap.add vd.vd_name { sym_type = Int; sym_attrs = attrs } s
  in
  let globals = List.fold_left process_declaration StringMap.empty decls in
  let _ = List.fold_left (fun global_scope decl ->
    match decl with
    | FunDecl fd ->
        let local_scope = List.fold_left (fun ls p ->
          StringMap.add p { sym_type = Int; sym_attrs = LocalAttr } ls
        ) global_scope fd.fd_params in
        let final_scope = match fd.fd_body with
          | Some (Block items) -> typecheck_block_items items local_scope
          | None -> local_scope
        in
        StringMap.fold (fun key entry acc ->
          match entry.sym_attrs with
          | FunAttr _ | StaticAttr _ -> StringMap.add key entry acc
          | LocalAttr -> acc (* Discard local variables *)
        ) final_scope StringMap.empty
    | VarDecl _ -> global_scope
  ) globals decls in
  (Program decls, globals)
