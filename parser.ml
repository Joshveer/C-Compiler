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
  let func, tokens = parse_function tokens in
  if tokens <> [] then
    raise (ParseError "Extra tokens at end of file")
  else
    Program func
