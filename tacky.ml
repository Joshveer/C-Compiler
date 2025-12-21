open Printf

type unary_operator =
  | Complement
  | Negate

type v =
  | Constant of int
  | Var of string

type instruction =
  | Return of v
  | Unary of unary_operator * v * v

type function_def =
  { name : string
  ; body : instruction list
  }

type program =
  | Program of function_def

let pp_unop = function
  | Complement -> "Complement"
  | Negate -> "Negate"

let pp_val = function
  | Constant i -> sprintf "Constant(%d)" i
  | Var s -> sprintf "Var(%s)" s

let pp_instruction = function
  | Return v -> 
      sprintf "Return(%s)" (pp_val v)
  | Unary (op, src, dst) -> 
      sprintf "Unary(%s, %s, %s)" (pp_unop op) (pp_val src) (pp_val dst)

let pp_program (Program f) =
  let body_str = 
    List.map pp_instruction f.body 
    |> String.concat "\n    " 
  in
  sprintf "Function %s:\n    %s\n" f.name body_str
