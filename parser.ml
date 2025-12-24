open Ast
open Lexer
open Printf

exception ParseError of string

let expect expected tokens =
  match tokens with
  | first :: rest when first = expected -> rest
  | _ -> raise (ParseError "Unexpected token")

let parse_identifier tokens =
  match tokens with
  | Ident name :: rest -> (name, rest)
  | _ -> raise (ParseError "Expected identifier")

let parse_storage_class = function
  | StaticKw -> Ast.Static
  | ExternKw -> Ast.Extern
  | _ -> failwith "Invalid storage class"

let parse_specifiers tokens =
  let rec loop types storage_classes toks =
    match toks with
    | IntKw :: rest -> loop (IntKw :: types) storage_classes rest
    | StaticKw :: rest -> loop types (StaticKw :: storage_classes) rest
    | ExternKw :: rest -> loop types (ExternKw :: storage_classes) rest
    | _ -> (List.rev types, List.rev storage_classes, toks)
  in
  let types, storage_classes, rest = loop [] [] tokens in
  if List.length types != 1 then raise (ParseError "Invalid type specifier");
  if List.length storage_classes > 1 then raise (ParseError "Multiple storage class specifiers");
  let storage_class = match storage_classes with
    | [sc] -> Some (parse_storage_class sc)
    | [] -> None
    | _ -> failwith "Impossible"
  in
  ((), storage_class, rest)

let parse_params tokens =
  match tokens with
  | RParen :: rest -> ([], rest)
  | VoidKw :: RParen :: rest -> ([], rest)
  | _ ->
      let rec loop acc toks =
        let _, _, toks = parse_specifiers toks in
        let name, toks = parse_identifier toks in
        match toks with
        | Comma :: rest -> loop (name :: acc) rest
        | _ -> (List.rev (name :: acc), toks)
      in
      let args, rest = loop [] tokens in
      let rest = expect RParen rest in
      (args, rest)

let get_precedence = function
  | Star | Slash | Percent -> 50 
  | Plus | Hyphen -> 45 
  | ShiftLeft | ShiftRight -> 40
  | LessThan | LessOrEqual | GreaterThan | GreaterOrEqual -> 35 
  | Equal | NotEqual -> 30
  | Ampersand -> 25 | Caret -> 20 | Pipe -> 15 
  | And -> 10 | Or -> 5 | Question -> 3
  | Assign | PlusAssign | MinusAssign | MultAssign | DivAssign | ModAssign 
  | AndAssign | OrAssign | XorAssign | LeftShiftAssign | RightShiftAssign -> 1
  | _ -> -1

let rec parse_primary tokens =
  match tokens with
  | IntConst i :: rest -> (Constant i, rest)
  | Ident name :: LParen :: rest ->
      let args, rest = parse_args rest in
      (FunctionCall (name, args), rest)
  | Ident name :: rest -> (Var name, rest)
  | LParen :: rest ->
      let (exp, rest) = parse_exp 0 rest in
      let rest = expect RParen rest in
      (exp, rest)
  | _ -> raise (ParseError "Expected primary expression")

and parse_args tokens =
  match tokens with
  | RParen :: rest -> ([], rest)
  | _ ->
      let rec loop acc toks =
        let arg, toks = parse_exp 0 toks in
        match toks with
        | Comma :: rest -> loop (arg :: acc) rest
        | _ -> (List.rev (arg :: acc), toks)
      in
      let args, rest = loop [] tokens in
      let rest = expect RParen rest in
      (args, rest)

and parse_exp min_prec tokens =
  let rec loop lhs tokens =
    match tokens with
    | op :: rest ->
        let prec = get_precedence op in
        if prec >= min_prec then
          match op with
          | Question ->
              let true_exp, rest = parse_exp 0 rest in
              let rest = expect Colon rest in
              let false_exp, rest = parse_exp prec rest in
              loop (Conditional (lhs, true_exp, false_exp)) rest
          | Assign | PlusAssign | MinusAssign | MultAssign | DivAssign | ModAssign | AndAssign | OrAssign | XorAssign | LeftShiftAssign | RightShiftAssign ->
              let rhs, rest = parse_exp prec rest in
              let new_lhs = match op with
                | Assign -> Assignment (lhs, rhs)
                | PlusAssign -> CompoundAssignment (Add, lhs, rhs)
                | MinusAssign -> CompoundAssignment (Subtract, lhs, rhs)
                | MultAssign -> CompoundAssignment (Multiply, lhs, rhs)
                | DivAssign -> CompoundAssignment (Divide, lhs, rhs)
                | ModAssign -> CompoundAssignment (Remainder, lhs, rhs)
                | AndAssign -> CompoundAssignment (BitAnd, lhs, rhs)
                | OrAssign -> CompoundAssignment (BitOr, lhs, rhs)
                | XorAssign -> CompoundAssignment (Xor, lhs, rhs)
                | LeftShiftAssign -> CompoundAssignment (ShiftLeft, lhs, rhs)
                | RightShiftAssign -> CompoundAssignment (ShiftRight, lhs, rhs)
                | _ -> failwith "Impossible"
              in
              loop new_lhs rest
          | _ ->
              let rhs, rest = parse_exp (prec + 1) rest in
              let bin_op = match op with
                | Plus -> Add | Hyphen -> Subtract | Star -> Multiply | Slash -> Divide
                | Percent -> Remainder | Ampersand -> BitAnd | Pipe -> BitOr | Caret -> Xor
                | ShiftLeft -> ShiftLeft | ShiftRight -> ShiftRight | And -> And | Or -> Or
                | Equal -> Equal | NotEqual -> NotEqual | LessThan -> LessThan
                | LessOrEqual -> LessOrEqual | GreaterThan -> GreaterThan
                | GreaterOrEqual -> GreaterOrEqual | _ -> failwith "Invalid binary operator"
              in
              loop (Binary (bin_op, lhs, rhs)) rest
        else (lhs, tokens)
    | _ -> (lhs, tokens)
  in
  let factor, rest = match tokens with
    | Tilde :: rest -> let e, r = parse_exp 50 rest in (Unary (Complement, e), r)
    | Hyphen :: rest -> let e, r = parse_exp 50 rest in (Unary (Negate, e), r)
    | Bang :: rest -> let e, r = parse_exp 50 rest in (Unary (Not, e), r)
    | Increment :: rest -> let e, r = parse_exp 50 rest in (PrefixIncrement e, r)
    | Decrement :: rest -> let e, r = parse_exp 50 rest in (PrefixDecrement e, r)
    | _ ->
        let p, r = parse_primary tokens in
        match r with
        | Increment :: rest -> (PostfixIncrement p, rest)
        | Decrement :: rest -> (PostfixDecrement p, rest)
        | _ -> (p, r)
  in
  loop factor rest

let rec parse_statement tokens =
  match tokens with
  | ReturnKw :: rest ->
      let exp, tokens = parse_exp 0 rest in
      let tokens = expect Semicolon tokens in
      (Return exp, tokens)
  | IfKw :: rest ->
      let rest = expect LParen rest in
      let cond, rest = parse_exp 0 rest in
      let rest = expect RParen rest in
      let then_stmt, rest = parse_statement rest in
      (match rest with
       | ElseKw :: rest -> let else_stmt, rest = parse_statement rest in (If (cond, then_stmt, Some else_stmt), rest)
       | _ -> (If (cond, then_stmt, None), rest))
  | LBrace :: rest ->
      let block, rest = parse_block rest in
      (Compound block, rest)
  | IntKw :: _ | StaticKw :: _ | ExternKw :: _ -> raise (ParseError "Declaration not allowed here")
  | GotoKw :: rest ->
      let label, rest = parse_identifier rest in
      let rest = expect Semicolon rest in
      (Goto label, rest)
  | Ident label :: Colon :: rest ->
      let stmt, rest = parse_statement rest in
      (Label (label, stmt), rest)
  | WhileKw :: rest ->
      let rest = expect LParen rest in
      let cond, rest = parse_exp 0 rest in
      let rest = expect RParen rest in
      let body, rest = parse_statement rest in
      (While (cond, body, None), rest)
  | DoKw :: rest ->
      let body, rest = parse_statement rest in
      let rest = expect WhileKw rest in
      let rest = expect LParen rest in
      let cond, rest = parse_exp 0 rest in
      let rest = expect RParen rest in
      let rest = expect Semicolon rest in
      (DoWhile (body, cond, None), rest)
  | ForKw :: rest ->
      let rest = expect LParen rest in
      let init, rest = match rest with
        | IntKw :: _ | StaticKw :: _ | ExternKw :: _ -> let d, r = parse_declaration rest in (match d with VarDecl v -> (InitDecl v, r) | _ -> raise (ParseError "Invalid for loop init"))
        | Semicolon :: r -> (InitExp None, r)
        | _ -> let e, r = parse_exp 0 rest in let r = expect Semicolon r in (InitExp (Some e), r)
      in
      let cond, rest = match rest with Semicolon :: _ -> (None, rest) | _ -> let e, r = parse_exp 0 rest in (Some e, r) in
      let rest = expect Semicolon rest in
      let post, rest = match rest with RParen :: _ -> (None, rest) | _ -> let e, r = parse_exp 0 rest in (Some e, r) in
      let rest = expect RParen rest in let body, rest = parse_statement rest in
      (For (init, cond, post, body, None), rest)
  | BreakKw :: rest -> let rest = expect Semicolon rest in (Break None, rest)
  | ContinueKw :: rest -> let rest = expect Semicolon rest in (Continue None, rest)
  | SwitchKw :: rest ->
      let rest = expect LParen rest in let cond, rest = parse_exp 0 rest in
      let rest = expect RParen rest in let body, rest = parse_statement rest in
      (Switch (cond, body, None, None), rest)
  | CaseKw :: rest -> let exp, rest = parse_exp 0 rest in let rest = expect Colon rest in let stmt, rest = parse_statement rest in (Case (exp, stmt, None), rest)
  | DefaultKw :: rest -> let rest = expect Colon rest in let stmt, rest = parse_statement rest in (Default (stmt, None), rest)
  | Semicolon :: rest -> (Null, rest)
  | _ -> let exp, tokens = parse_exp 0 tokens in let tokens = expect Semicolon tokens in (Expression exp, tokens)

and parse_block_item tokens =
  match tokens with
  | IntKw :: _ | StaticKw :: _ | ExternKw :: _ ->
      let decl, rest = parse_declaration tokens in
      (D decl, rest)
  | _ ->
      let stmt, rest = parse_statement tokens in
      (S stmt, rest)

and parse_block tokens =
  let rec loop items toks =
    match toks with
    | RBrace :: rest -> (Block (List.rev items), rest)
    | _ ->
        let item, rest = parse_block_item toks in
        loop (item :: items) rest
  in
  loop [] tokens

and parse_declaration tokens =
  let _, storage_class, rest = parse_specifiers tokens in
  let name, rest = parse_identifier rest in
  match rest with
  | LParen :: _ ->
      let args, rest = parse_params rest in
      let body, rest = match rest with
        | Semicolon :: r -> (None, r)
        | LBrace :: _ -> let b, r = parse_block rest in (Some b, r)
        | _ -> raise (ParseError "Expected function body or semicolon")
      in
      (FunDecl { fd_name = name; fd_params = args; fd_body = body; fd_storage_class = storage_class }, rest)
  | Assign :: rest ->
      let exp, rest = parse_exp 0 rest in
      let rest = expect Semicolon rest in
      (VarDecl { vd_name = name; vd_init = Some exp; vd_storage_class = storage_class }, rest)
  | Semicolon :: rest ->
      (VarDecl { vd_name = name; vd_init = None; vd_storage_class = storage_class }, rest)
  | _ -> raise (ParseError "Expected variable or function declaration")

let parse_function tokens =
  let decl, tokens = parse_declaration tokens in
  match decl with
  | FunDecl f -> (f, tokens)
  | _ -> raise (ParseError "Expected function declaration at top level")

let parse tokens =
  let rec loop items toks =
    if toks = [] then items
    else
      let item, rest = parse_declaration toks in
      loop (item :: items) rest
  in
  Program (List.rev (loop [] tokens))