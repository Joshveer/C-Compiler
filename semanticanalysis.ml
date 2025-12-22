open Ast
open Printf

module StringMap = Map.Make(String)

exception SemanticError of string

let var_counter = ref 0
let label_counter = ref 0

let make_unique_name name =
  let id = !var_counter in
  incr var_counter;
  sprintf "%s.%d" name id

let make_unique_label name =
  let id = !label_counter in
  incr label_counter;
  sprintf "%s.%d" name id

let fail msg =
  raise (SemanticError msg)

let check_lvalue e =
  match e with
  | Var _ -> ()
  | _ -> fail "Invalid lvalue"

let rec resolve_exp exp map =
  match exp with
  | Constant _ -> exp
  | Var name ->
      (match StringMap.find_opt name map with
       | Some unique_name -> Var unique_name
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
      if StringMap.mem name map then
        fail (sprintf "Duplicate variable declaration: %s" name);
      let unique_name = make_unique_name name in
      let new_map = StringMap.add name unique_name map in
      let resolved_init =
        match init_opt with
        | Some init -> Some (resolve_exp init new_map)
        | None -> None
      in
      (Declaration (unique_name, resolved_init), new_map)

let rec resolve_statement stmt map =
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
      Label (name, resolve_statement inner map) (* Recursively resolve inner statement *)

let resolve_block_item item map =
  match item with
  | D decl ->
      let (resolved_decl, new_map) = resolve_declaration decl map in
      (D resolved_decl, new_map)
  | S stmt ->
      (S (resolve_statement stmt map), map)

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
  | _ -> map

let rec collect_labels_in_block items map =
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
  | _ -> stmt

let rec resolve_labels_in_block items map =
  match items with
  | [] -> []
  | S stmt :: rest ->
      S (resolve_labels_in_stmt stmt map) :: resolve_labels_in_block rest map
  | D decl :: rest ->
      D decl :: resolve_labels_in_block rest map

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
  let resolved_body = resolve_vars body_with_resolved_labels StringMap.empty [] in
  Function (name, resolved_body)

let resolve_program (Program func_def) =
  Program (resolve_function func_def)
