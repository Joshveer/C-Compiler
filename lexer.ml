open Printf

type token =
  | IntKw
  | VoidKw
  | ReturnKw
  | IfKw
  | ElseKw
  | GotoKw
  | Ident of string
  | IntConst of int
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Semicolon
  | Tilde
  | Hyphen
  | Decrement
  | Plus
  | Increment
  | Star
  | Slash
  | Percent
  | Ampersand
  | Pipe
  | Caret
  | ShiftLeft
  | ShiftRight
  | Bang
  | And
  | Or
  | Equal
  | NotEqual
  | LessThan
  | GreaterThan
  | LessOrEqual
  | GreaterOrEqual
  | Assign
  | PlusAssign
  | MinusAssign
  | MultAssign
  | DivAssign
  | ModAssign
  | AndAssign
  | OrAssign
  | XorAssign
  | LeftShiftAssign
  | RightShiftAssign
  | Question
  | Colon

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
  | "if" -> Some IfKw
  | "else" -> Some ElseKw
  | "goto" -> Some GotoKw
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
    if i >= len then
      List.rev acc
    else
      match input.[i] with
      | c when is_whitespace c ->
          lex_at (skip_whitespace i) acc
      | '(' -> lex_at (i + 1) (LParen :: acc)
      | ')' -> lex_at (i + 1) (RParen :: acc)
      | '{' -> lex_at (i + 1) (LBrace :: acc)
      | '}' -> lex_at (i + 1) (RBrace :: acc)
      | ';' -> lex_at (i + 1) (Semicolon :: acc)
      | ':' -> lex_at (i + 1) (Colon :: acc)
      | '?' -> lex_at (i + 1) (Question :: acc)
      | '~' -> lex_at (i + 1) (Tilde :: acc)
      | '*' -> 
          if i + 1 < len && input.[i + 1] = '=' then
            lex_at (i + 2) (MultAssign :: acc)
          else
            lex_at (i + 1) (Star :: acc)
      | '/' -> 
          if i + 1 < len && input.[i + 1] = '=' then
            lex_at (i + 2) (DivAssign :: acc)
          else
            lex_at (i + 1) (Slash :: acc)
      | '%' -> 
          if i + 1 < len && input.[i + 1] = '=' then
            lex_at (i + 2) (ModAssign :: acc)
          else
            lex_at (i + 1) (Percent :: acc)
      | '&' ->
          if i + 1 < len then
            match input.[i + 1] with
            | '&' -> lex_at (i + 2) (And :: acc)
            | '=' -> lex_at (i + 2) (AndAssign :: acc)
            | _ -> lex_at (i + 1) (Ampersand :: acc)
          else
            lex_at (i + 1) (Ampersand :: acc)
      | '|' ->
          if i + 1 < len then
            match input.[i + 1] with
            | '|' -> lex_at (i + 2) (Or :: acc)
            | '=' -> lex_at (i + 2) (OrAssign :: acc)
            | _ -> lex_at (i + 1) (Pipe :: acc)
          else
            lex_at (i + 1) (Pipe :: acc)
      | '^' -> 
          if i + 1 < len && input.[i + 1] = '=' then
            lex_at (i + 2) (XorAssign :: acc)
          else
            lex_at (i + 1) (Caret :: acc)
      | '!' ->
          if i + 1 < len && input.[i + 1] = '=' then
            lex_at (i + 2) (NotEqual :: acc)
          else
            lex_at (i + 1) (Bang :: acc)
      | '<' ->
          if i + 1 < len then
            match input.[i + 1] with
            | '<' -> 
                if i + 2 < len && input.[i + 2] = '=' then
                  lex_at (i + 3) (LeftShiftAssign :: acc)
                else
                  lex_at (i + 2) (ShiftLeft :: acc)
            | '=' -> lex_at (i + 2) (LessOrEqual :: acc)
            | _ -> lex_at (i + 1) (LessThan :: acc)
          else
            lex_at (i + 1) (LessThan :: acc)
      | '>' ->
          if i + 1 < len then
            match input.[i + 1] with
            | '>' -> 
                if i + 2 < len && input.[i + 2] = '=' then
                  lex_at (i + 3) (RightShiftAssign :: acc)
                else
                  lex_at (i + 2) (ShiftRight :: acc)
            | '=' -> lex_at (i + 2) (GreaterOrEqual :: acc)
            | _ -> lex_at (i + 1) (GreaterThan :: acc)
          else
            lex_at (i + 1) (GreaterThan :: acc)
      | '-' ->
          if i + 1 < len then
            match input.[i + 1] with
            | '-' -> lex_at (i + 2) (Decrement :: acc)
            | '=' -> lex_at (i + 2) (MinusAssign :: acc)
            | _ -> lex_at (i + 1) (Hyphen :: acc)
          else
            lex_at (i + 1) (Hyphen :: acc)
      | '+' -> 
          if i + 1 < len then
            match input.[i + 1] with
            | '+' -> lex_at (i + 2) (Increment :: acc)
            | '=' -> lex_at (i + 2) (PlusAssign :: acc)
            | _ -> lex_at (i + 1) (Plus :: acc)
          else
            lex_at (i + 1) (Plus :: acc)
      | '=' ->
          if i + 1 < len && input.[i + 1] = '=' then
            lex_at (i + 2) (Equal :: acc)
          else
            lex_at (i + 1) (Assign :: acc)
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
            raise (LexError "Invalid identifier starting with digit")
          else
            let num_str = String.sub input i (!j - i) in
            lex_at !j (IntConst (int_of_string num_str) :: acc)
      | c -> raise (LexError (sprintf "Unexpected character: %c" c))
  in
  lex_at 0 []
