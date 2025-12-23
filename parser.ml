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

let get_precedence = function
  | Star | Slash | Percent -> 50 | Plus | Hyphen -> 45 | ShiftLeft | ShiftRight -> 40
  | LessThan | LessOrEqual | GreaterThan | GreaterOrEqual -> 35 | Equal | NotEqual -> 30
  | Ampersand -> 25 | Caret -> 20 | Pipe -> 15 | And -> 10 | Or -> 5 | Question -> 3
  | Assign | PlusAssign | MinusAssign | MultAssign | DivAssign | ModAssign | AndAssign | OrAssign | XorAssign | LeftShiftAssign | RightShiftAssign -> 1
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
        let acc = arg :: acc in
        match toks with
        | Comma :: t -> loop acc t
        | RParen :: t -> (List.rev acc, t)
        | _ -> raise (ParseError "Expected , or ) in argument list")
      in
      loop [] tokens

and parse_postfix tokens =
  let (exp, rest) = parse_primary tokens in
  let rec loop e toks =
    match toks with
    | Increment :: t -> loop (PostfixIncrement e) t
    | Decrement :: t -> loop (PostfixDecrement e) t
    | _ -> (e, toks)
  in
  loop exp rest

and parse_unary tokens =
  match tokens with
  | Tilde :: rest -> let (inner, rest) = parse_unary rest in (Unary (Complement, inner), rest)
  | Hyphen :: rest -> let (inner, rest) = parse_unary rest in (Unary (Negate, inner), rest)
  | Bang :: rest -> let (inner, rest) = parse_unary rest in (Unary (Not, inner), rest)
  | Increment :: rest -> let (inner, rest) = parse_unary rest in (PrefixIncrement inner, rest)
  | Decrement :: rest -> let (inner, rest) = parse_unary rest in (PrefixDecrement inner, rest)
  | _ -> parse_postfix tokens

and parse_exp min_prec tokens =
  let (left, rest) = parse_unary tokens in
  let rec loop left tokens =
    let prec = match tokens with op :: _ -> get_precedence op | _ -> -1 in
    if prec < min_prec then (left, tokens)
    else
      match tokens with
      | Question :: rest ->
          let then_exp, rest = parse_exp 0 rest in
          let rest = expect Colon rest in
          let else_exp, rest = parse_exp 0 rest in
          loop (Conditional (left, then_exp, else_exp)) rest
      | Assign :: rest -> let (right, rest) = parse_exp prec rest in loop (Assignment (left, right)) rest
      | PlusAssign :: rest -> let (right, rest) = parse_exp prec rest in loop (CompoundAssignment (Add, left, right)) rest
      | MinusAssign :: rest -> let (right, rest) = parse_exp prec rest in loop (CompoundAssignment (Subtract, left, right)) rest
      | MultAssign :: rest -> let (right, rest) = parse_exp prec rest in loop (CompoundAssignment (Multiply, left, right)) rest
      | DivAssign :: rest -> let (right, rest) = parse_exp prec rest in loop (CompoundAssignment (Divide, left, right)) rest
      | ModAssign :: rest -> let (right, rest) = parse_exp prec rest in loop (CompoundAssignment (Remainder, left, right)) rest
      | AndAssign :: rest -> let (right, rest) = parse_exp prec rest in loop (CompoundAssignment (BitAnd, left, right)) rest
      | OrAssign :: rest -> let (right, rest) = parse_exp prec rest in loop (CompoundAssignment (BitOr, left, right)) rest
      | XorAssign :: rest -> let (right, rest) = parse_exp prec rest in loop (CompoundAssignment (Xor, left, right)) rest
      | LeftShiftAssign :: rest -> let (right, rest) = parse_exp prec rest in loop (CompoundAssignment (ShiftLeft, left, right)) rest
      | RightShiftAssign :: rest -> let (right, rest) = parse_exp prec rest in loop (CompoundAssignment (ShiftRight, left, right)) rest
      | op :: rest ->
          let (right, rest) = parse_exp (prec + 1) rest in
          let op = match op with
            | Plus -> Add | Hyphen -> Subtract | Star -> Multiply | Slash -> Divide | Percent -> Remainder
            | Ampersand -> BitAnd | Pipe -> BitOr | Caret -> Xor | ShiftLeft -> ShiftLeft | ShiftRight -> ShiftRight
            | And -> And | Or -> Or | Equal -> Equal | NotEqual -> NotEqual | LessThan -> LessThan
            | LessOrEqual -> LessOrEqual | GreaterThan -> GreaterThan | GreaterOrEqual -> GreaterOrEqual
            | _ -> raise (ParseError "Invalid binary operator")
          in
          loop (Binary (op, left, right)) rest
      | _ -> (left, tokens)
  in
  loop left rest

let parse_declaration tokens =
  let tokens = expect IntKw tokens in
  let name, tokens = parse_identifier tokens in
  match tokens with
  | Assign :: rest ->
      let exp, rest = parse_exp 0 rest in
      let rest = expect Semicolon rest in
      (Declaration (name, Some exp), rest)
  | Semicolon :: rest -> (Declaration (name, None), rest)
  | _ -> raise (ParseError "Expected ; or = in declaration")

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
      let then_s, rest = parse_statement rest in
      let else_s, rest = match rest with ElseKw :: rest -> let s, rest = parse_statement rest in (Some s, rest) | _ -> (None, rest) in
      (If (cond, then_s, else_s), rest)
  | LBrace :: _ -> let block, rest = parse_block tokens in (Compound block, rest)
  | GotoKw :: rest -> let name, rest = parse_identifier rest in let rest = expect Semicolon rest in (Goto name, rest)
  | Ident name :: Colon :: rest -> let stmt, rest = parse_statement rest in (Label (name, stmt), rest)
  | WhileKw :: rest ->
      let rest = expect LParen rest in let cond, rest = parse_exp 0 rest in
      let rest = expect RParen rest in let body, rest = parse_statement rest in
      (While (cond, body, None), rest)
  | DoKw :: rest ->
      let body, rest = parse_statement rest in let rest = expect WhileKw rest in
      let rest = expect LParen rest in let cond, rest = parse_exp 0 rest in
      let rest = expect RParen rest in let rest = expect Semicolon rest in
      (DoWhile (body, cond, None), rest)
  | ForKw :: rest ->
      let rest = expect LParen rest in
      let init, rest = match rest with IntKw :: _ -> let d, r = parse_declaration rest in (InitDecl d, r) | Semicolon :: r -> (InitExp None, r) | _ -> let e, r = parse_exp 0 rest in let r = expect Semicolon r in (InitExp (Some e), r) in
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

and parse_block tokens =
  let tokens = expect LBrace tokens in
  let rec loop acc toks =
    match toks with
    | RBrace :: rest -> (Block (List.rev acc), rest)
    | IntKw :: _ -> let d, rest = parse_declaration toks in loop (D d :: acc) rest
    | _ -> let s, rest = parse_statement toks in loop (S s :: acc) rest
  in loop [] tokens

let parse_params tokens =
  let tokens = expect LParen tokens in
  match tokens with
  | VoidKw :: RParen :: rest -> ([], rest)
  | _ ->
      let rec loop acc toks =
        let toks = expect IntKw toks in
        let name, toks = parse_identifier toks in
        let acc = name :: acc in
        match toks with
        | Comma :: t -> loop acc t
        | RParen :: t -> (List.rev acc, t)
        | _ -> raise (ParseError "Expected , or ) in parameter list")
      in loop [] tokens

let parse_function tokens =
  let tokens = expect IntKw tokens in
  let name, tokens = parse_identifier tokens in
  let params, tokens = parse_params tokens in
  match tokens with
  | LBrace :: _ -> let block, tokens = parse_block tokens in (Function { name; params; body = Some block }, tokens)
  | Semicolon :: tokens -> (Function { name; params; body = None }, tokens)
  | _ -> raise (ParseError "Expected function body or ;")

let parse tokens =
  let rec loop acc toks =
    match toks with [] -> Program (List.rev acc) | _ -> let f, rest = parse_function toks in loop (f :: acc) rest
  in loop [] tokens
