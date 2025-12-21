open Printf

type unary_op = 
  | Complement 
  | Negate

type binary_op = 
  | Add 
  | Subtract 
  | Multiply 
  | Divide 
  | Remainder

type identifier = string

type exp = 
  | Constant of int
  | Unary of unary_op * exp
  | Binary of binary_op * exp * exp

type statement = 
  | Return of exp

type function_def = 
  | Function of identifier * statement

type program = 
  | Program of function_def

let pp_binop = function
  | Add -> "Add"
  | Subtract -> "Subtract"
  | Multiply -> "Multiply"
  | Divide -> "Divide"
  | Remainder -> "Remainder"

let rec pp_exp = function
  | Constant i -> sprintf "Constant(%d)" i
  | Unary (op, e) -> 
      let op_str = match op with Complement -> "Complement" | Negate -> "Negate" in
      sprintf "Unary(%s, %s)" op_str (pp_exp e)
  | Binary (op, e1, e2) ->
      sprintf "Binary(%s, %s, %s)" (pp_binop op) (pp_exp e1) (pp_exp e2)

let pp_statement = function
  | Return e -> sprintf "Return(\n    %s\n  )" (pp_exp e)

let pp_function_def (Function (name, body)) =
  sprintf "Function(\n  name=\"%s\",\n  body=%s\n)" name (pp_statement body)

let pp_program (Program f) =
  sprintf "Program(\n  %s\n)\n" (pp_function_def f)
