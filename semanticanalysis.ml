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
  | For (init, cond, post, body, _) ->
      let lbl = make_label "loop" in
      let new_map = enter_scope map in
      let resolved_init, map_with_init = match init with
        | InitDecl d -> let rd, m = resolve_variable_declaration d new_map in (InitDecl rd, m)
        | InitExp e -> (InitExp (Option.map (fun x -> resolve_exp x new_map) e), new_map)
      in
      let inner_ctx = { ctx with break_label = Some lbl; continue_label = Some lbl } in
      For (resolved_init,
           Option.map (fun x -> resolve_exp x map_with_init) cond,
           Option.map (fun x -> resolve_exp x map_with_init) post,
           resolve_statement body map_with_init inner_ctx,
           Some lbl)
  | Break _ ->
      (match ctx.break_label with
       | Some lbl -> Break (Some lbl)
       | None -> fail "Break statement outside of loop or switch")
  | Continue _ ->
      (match ctx.continue_label with
       | Some lbl -> Continue (Some lbl)
       | None -> fail "Continue statement outside of loop")
  | Switch (e, s, _, _) ->
      let lbl = make_label "switch" in
      let cases = ref { case_list = []; default_label = None } in
      let inner_ctx = { ctx with break_label = Some lbl; current_switch = Some cases } in
      let resolved_body = resolve_statement s map inner_ctx in
      Switch (resolve_exp e map, resolved_body, Some lbl, Some !cases)
  | Case (e, s, _) ->
      (match ctx.current_switch with
       | Some cases ->
           let l = make_label "case" in
           let resolved_e = resolve_exp e map in
           let case_val = match resolved_e with Constant i -> i | _ -> fail "Case value must be constant" in
           if List.exists (fun (e, _) -> match e with Constant v -> v = case_val | _ -> false) !cases.case_list then fail "Duplicate case value";
           cases := { !cases with case_list = (resolved_e, l) :: !cases.case_list };
           Case (resolved_e, resolve_statement s map ctx, Some l)
       | None -> fail "Case statement outside of switch")
  | Default (s, _) ->
      (match ctx.current_switch with
       | Some cases ->
           if Option.is_some !cases.default_label then fail "Duplicate default label";
           let l = make_label "default" in
           cases := { !cases with default_label = Some l };
           Default (resolve_statement s map ctx, Some l)
       | None -> fail "Default statement outside of switch")
  | Goto l -> Goto l
  | Label (l, s) ->
      if in_current_scope l map then fail "Duplicate label" else
      let new_name = make_unique_name l in
      let new_map = add_entry l { unique_name = new_name; has_linkage = false } map in
      Label (new_name, resolve_statement s new_map ctx)
  | Null -> Null

and resolve_block (Block items) map ctx =
  let rec loop items map acc =
    match items with
    | [] -> Block (List.rev acc)
    | D decl :: rest ->
        let resolved_decl, new_map = resolve_declaration decl map in
        loop rest new_map (D resolved_decl :: acc)
    | S stmt :: rest ->
        let resolved_stmt = resolve_statement stmt map ctx in
        loop rest map (S resolved_stmt :: acc)
  in
  let new_map = enter_scope map in
  loop items new_map []

and resolve_variable_declaration vd map =
  if in_current_scope vd.vd_name map then fail ("Redeclaration of variable " ^ vd.vd_name);
  let unique_name = if vd.vd_storage_class = Some Extern then vd.vd_name else make_unique_name vd.vd_name in
  let new_entry = { unique_name = unique_name; has_linkage = (vd.vd_storage_class = Some Extern) } in
  let new_map = add_entry vd.vd_name new_entry map in
  ({ vd with vd_name = unique_name; vd_init = Option.map (fun e -> resolve_exp e map) vd.vd_init }, new_map)

and resolve_function_declaration fd map =
  if in_current_scope fd.fd_name map then fail ("Redeclaration of function " ^ fd.fd_name);
  let unique_name = fd.fd_name in 
  let new_entry = { unique_name = unique_name; has_linkage = true } in
  let map_with_fun = add_entry fd.fd_name new_entry map in
  let new_map = enter_scope map_with_fun in
  let rec resolve_params params map acc =
    match params with
    | [] -> (List.rev acc, map)
    | p :: rest ->
        if in_current_scope p map then fail ("Duplicate parameter name " ^ p);
        let unique = make_unique_name p in
        let map = add_entry p { unique_name = unique; has_linkage = false } map in
        resolve_params rest map (unique :: acc)
  in
  let resolved_params, map_with_params = resolve_params fd.fd_params new_map [] in
  let resolved_body = Option.map (fun b -> resolve_block b map_with_params default_ctx) fd.fd_body in
  ({ fd with fd_params = resolved_params; fd_body = resolved_body }, map_with_fun)

and resolve_declaration decl map =
  match decl with
  | FunDecl fd -> let rfd, m = resolve_function_declaration fd map in (FunDecl rfd, m)
  | VarDecl vd -> let rvd, m = resolve_variable_declaration vd map in (VarDecl rvd, m)

let resolve_program (Program decls) =
  let rec loop decls map acc =
    match decls with
    | [] -> Program (List.rev acc)
    | decl :: rest ->
        let resolved_decl, new_map = resolve_declaration decl map in
        loop rest new_map (resolved_decl :: acc)
  in
  loop decls empty_map []

type symbol_type = Int | Fun of int

type symbol_attr =
  | StaticAttr of initial_value * bool 
  | FunAttr of bool 
  | LocalAttr

and initial_value = Initial of int | Tentative | NoInitializer

type symbol_entry = {
  sym_type : symbol_type;
  sym_attrs : symbol_attr;
}

let typecheck_block_items items scope = scope

let typecheck_program (Program decls) =
  let process_declaration s decl =
    match decl with
    | FunDecl fd ->
        let global = fd.fd_storage_class <> Some Ast.Static in
        let attrs = FunAttr global in
        StringMap.add fd.fd_name { sym_type = Fun (List.length fd.fd_params); sym_attrs = attrs } s
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
  let all_symbols = List.fold_left (fun global_scope decl ->
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
          | LocalAttr -> acc 
        ) final_scope StringMap.empty
    | VarDecl _ -> global_scope
  ) globals decls in
  (Program decls, all_symbols)