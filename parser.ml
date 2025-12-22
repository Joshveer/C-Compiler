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
  | Star | Slash | Percent -> 50
  | Plus | Hyphen -> 45
  | ShiftLeft | ShiftRight -> 40
  | LessThan | LessOrEqual | GreaterThan | GreaterOrEqual -> 35
  | Equal | NotEqual -> 30
  | Ampersand -> 25
  | Caret -> 20
  | Pipe -> 15
  | And -> 10
  | Or -> 5
  | Assign | PlusAssign | MinusAssign | MultAssign | DivAssign
  | ModAssign | AndAssign | OrAssign | XorAssign
  | LeftShiftAssign | RightShiftAssign -> 1
  | _ -> -1

let rec parse_primary tokens =
  match tokens with
  | IntConst i :: rest -> (Constant i, rest)
  | Ident name :: rest -> (Var name, rest)
  | LParen :: rest ->
      let (exp, rest) = parse_exp 0 rest in
      let rest = expect RParen rest in
      (exp, rest)
  | _ -> raise (ParseError "Expected primary expression")

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
  | Tilde :: rest ->
      let (inner, rest) = parse_unary rest in
      (Unary (Complement, inner), rest)
  | Hyphen :: rest ->
      let (inner, rest) = parse_unary rest in
      (Unary (Negate, inner), rest)
  | Bang :: rest ->
      let (inner, rest) = parse_unary rest in
      (Unary (Not, inner), rest)
  | Increment :: rest ->
      let (inner, rest) = parse_unary rest in
      (PrefixIncrement inner, rest)
  | Decrement :: rest ->
      let (inner, rest) = parse_unary rest in
      (PrefixDecrement inner, rest)
  | _ -> parse_postfix tokens

and parse_exp min_prec tokens =
  let (left, rest) = parse_unary tokens in
  let rec loop left tokens =
    match tokens with
    | op_token :: rest_tokens ->
        let op_prec = get_precedence op_token in
        if op_prec < min_prec then (left, tokens)
        else
          let handle_assignment constr =
            let (right, rest) = parse_exp op_prec rest_tokens in
            let new_left = constr (left, right) in
            loop new_left rest
          in
          let handle_compound_assignment bin_op =
             let (right, rest) = parse_exp op_prec rest_tokens in
             let new_left = CompoundAssignment (bin_op, left, right) in
             loop new_left rest
          in
          begin
            match op_token with
            | Assign -> handle_assignment (fun (l, r) -> Assignment (l, r))
            | PlusAssign -> handle_compound_assignment Add
            | MinusAssign -> handle_compound_assignment Subtract
            | MultAssign -> handle_compound_assignment Multiply
            | DivAssign -> handle_compound_assignment Divide
            | ModAssign -> handle_compound_assignment Remainder
            | AndAssign -> handle_compound_assignment BitAnd
            | OrAssign -> handle_compound_assignment BitOr
            | XorAssign -> handle_compound_assignment Xor
            | LeftShiftAssign -> handle_compound_assignment ShiftLeft
            | RightShiftAssign -> handle_compound_assignment ShiftRight
            | _ ->
              let op =
                match op_token with
                | Plus -> Add
                | Hyphen -> Subtract
                | Star -> Multiply
                | Slash -> Divide
                | Percent -> Remainder
                | Ampersand -> BitAnd
                | Pipe -> BitOr
                | Caret -> Xor
                | ShiftLeft -> ShiftLeft
                | ShiftRight -> ShiftRight
                | And -> And
                | Or -> Or
                | Equal -> Equal
                | NotEqual -> NotEqual
                | LessThan -> LessThan
                | LessOrEqual -> LessOrEqual
                | GreaterThan -> GreaterThan
                | GreaterOrEqual -> GreaterOrEqual
                | _ -> failwith "Invalid binary operator"
              in
              let next_min_prec = op_prec + 1 in
              let (right, rest) = parse_exp next_min_prec rest_tokens in
              let new_left = Binary (op, left, right) in
              loop new_left rest
          end
    | [] -> (left, tokens)
  in
  loop left rest

let parse_declaration tokens =
  let tokens = expect IntKw tokens in
  let name, tokens = parse_identifier tokens in
  match tokens with
  | Semicolon :: rest ->
      (Declaration (name, None), rest)
  | Assign :: rest ->
      let init, tokens = parse_exp 0 rest in
      let tokens = expect Semicolon tokens in
      (Declaration (name, Some init), tokens)
  | _ -> raise (ParseError "Expected ; or = in declaration")

let parse_statement tokens =
  match tokens with
  | ReturnKw :: rest ->
      let exp, tokens = parse_exp 0 rest in
      let tokens = expect Semicolon tokens in
      (Return exp, tokens)
  | Semicolon :: rest ->
      (Null, rest)
  | _ ->
      let exp, tokens = parse_exp 0 tokens in
      let tokens = expect Semicolon tokens in
      (Expression exp, tokens)

let parse_block_item tokens =
  match tokens with
  | IntKw :: _ ->
      let decl, tokens = parse_declaration tokens in
      (D decl, tokens)
  | _ ->
      let stmt, tokens = parse_statement tokens in
      (S stmt, tokens)

let parse_function tokens =
  let tokens = expect IntKw tokens in
  let name, tokens = parse_identifier tokens in
  let tokens = expect LParen tokens in
  let tokens = expect VoidKw tokens in
  let tokens = expect RParen tokens in
  let tokens = expect LBrace tokens in
  
  let rec parse_items acc tokens =
    match tokens with
    | RBrace :: rest -> (List.rev acc, rest)
    | _ ->
        let item, rest = parse_block_item tokens in
        parse_items (item :: acc) rest
  in
  let body, tokens = parse_items [] tokens in
  (Function (name, body), tokens)

let parse tokens =
  let (func_def, rest) = parse_function tokens in
  match rest with
  | [] -> Program func_def
  | _ -> raise (ParseError "Unexpected token at end of file")
