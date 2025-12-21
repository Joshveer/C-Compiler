type token =
  | IntKw
  | VoidKw
  | ReturnKw
  | Ident of string
  | IntConst of int
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Semicolon

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_word_char c =
  is_letter c || is_digit c

let keyword_of_ident = function
  | "int" -> Some IntKw
  | "void" -> Some VoidKw
  | "return" -> Some ReturnKw
  | _ -> None

exception LexError of string

let lex (input : string) : token list =
  let len = String.length input in

  let rec skip_whitespace i =
    if i < len && is_whitespace input.[i] then
      skip_whitespace (i + 1)
    else
      i
  in

  let rec lex_at i acc =
    let i = skip_whitespace i in
    if i >= len then
      List.rev acc
    else
      match input.[i] with
      | '(' -> lex_at (i + 1) (LParen :: acc)
      | ')' -> lex_at (i + 1) (RParen :: acc)
      | '{' -> lex_at (i + 1) (LBrace :: acc)
      | '}' -> lex_at (i + 1) (RBrace :: acc)
      | ';' -> lex_at (i + 1) (Semicolon :: acc)

      | c when is_letter c ->
          let j = ref (i + 1) in
          while !j < len && is_word_char input.[!j] do
            incr j
          done;
          let name = String.sub input i (!j - i) in
          let tok =
            match keyword_of_ident name with
            | Some kw -> kw
            | None -> Ident name
          in
          lex_at !j (tok :: acc)

      | c when is_digit c ->
          let j = ref (i + 1) in
          while !j < len && is_digit input.[!j] do
            incr j
          done;
          if !j < len && is_letter input.[!j] then
            raise (LexError ("invalid integer literal"))
          else
            let value =
              int_of_string (String.sub input i (!j - i))
            in
            lex_at !j (IntConst value :: acc)

      | _ ->
          raise (LexError ("unexpected character"))
  in
  lex_at 0 []
