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

let parse_exp tokens =
  match tokens with
  | IntConst i :: rest -> (Constant i, rest)
  | _ -> raise (ParseError "Expected integer constant")

let parse_statement tokens =
  let tokens = expect ReturnKw tokens in
  let exp, tokens = parse_exp tokens in
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
