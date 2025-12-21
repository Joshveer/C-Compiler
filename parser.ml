open Ast
open Lexer

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
  | Ampersand -> 25 (* BitAnd precedence usually between Eq and Shift, but based on your text I'll keep your table values or standardize. C standard: & is below ==. Text says table has gaps. Using standard C ordering adjusted for your table gaps. *)
  | Caret -> 20     (* BitXor *)
  | Pipe -> 15      (* BitOr *)
  | And -> 10       (* Logical && *)
  | Or -> 5         (* Logical || *)
  | _ -> -1

let rec parse_factor tokens =
  match tokens with
  | IntConst i :: rest -> 
      (Constant i, rest)
  | Tilde :: rest ->
      let (inner_exp, rest) = parse_factor rest in
      (Unary (Complement, inner_exp), rest)
  | Hyphen :: rest ->
      let (inner_exp, rest) = parse_factor rest in
      (Unary (Negate, inner_exp), rest)
  | Bang :: rest ->
      let (inner_exp, rest) = parse_factor rest in
      (Unary (Not, inner_exp), rest)
  | LParen :: rest ->
      let (inner_exp, rest) = parse_exp 0 rest in
      let rest = expect RParen rest in
      (inner_exp, rest)
  | _ -> raise (ParseError "Expected factor")

and parse_exp min_prec tokens =
  let (left, rest) = parse_factor tokens in
  let rec loop left tokens =
    match tokens with
    | op_token :: rest_tokens when get_precedence op_token >= min_prec ->
        let op = match op_token with
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
        let next_min_prec = get_precedence op_token + 1 in
        let (right, rest) = parse_exp next_min_prec rest_tokens in
        let new_left = Binary (op, left, right) in
        loop new_left rest
    | _ -> (left, tokens)
  in
  loop left rest

let parse_statement tokens =
  let tokens = expect ReturnKw tokens in
  let exp, tokens = parse_exp 0 tokens in
  let tokens = expect Semicolon tokens in
  (Return exp, tokens)

let parse_function tokens =
  let tokens = expect IntKw tokens in
  let name, tokens = parse_identifier tokens in
  let tokens = expect LParen tokens in
  let tokens = expect VoidKw tokens in
  let tokens = expect RParen tokens in
  let tokens = expect LBrace tokens in
  let body, tokens = parse_statement tokens in
  let tokens = expect RBrace tokens in
  (Function (name, body), tokens)

let parse tokens =
  let (func_def, rest) = parse_function tokens in
  if rest <> [] then raise (ParseError "Unexpected tokens after program") else
  Program func_def
