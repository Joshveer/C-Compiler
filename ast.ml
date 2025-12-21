open Printf

type unary_op = 
  | Complement 
  | Negate
  | Not

type binary_op = 
  | Add 
  | Subtract 
  | Multiply 
  | Divide 
  | Remainder
  | BitAnd 
  | BitOr 
  | Xor 
  | ShiftLeft 
  | ShiftRight
  | And
  | Or
  | Equal
  | NotEqual
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual

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
  | BitAnd -> "BitAnd"
  | BitOr -> "BitOr"
  | Xor -> "Xor"
  | ShiftLeft -> "ShiftLeft"
  | ShiftRight -> "ShiftRight"
  | And -> "And"
  | Or -> "Or"
  | Equal -> "Equal"
  | NotEqual -> "NotEqual"
  | LessThan -> "LessThan"
  | LessOrEqual -> "LessOrEqual"
  | GreaterThan -> "GreaterThan"
  | GreaterOrEqual -> "GreaterOrEqual"

let rec pp_exp = function
  | Constant i -> sprintf "Constant(%d)" i
  | Unary (op, e) -> 
      let op_str = match op with 
        | Complement -> "Complement" 
        | Negate -> "Negate" 
        | Not -> "Not" 
      in
      sprintf "Unary(%s, %s)" op_str (pp_exp e)
  | Binary (op, e1, e2) ->
      sprintf "Binary(%s, %s, %s)" (pp_binop op) (pp_exp e1) (pp_exp e2)

let pp_statement = function
  | Return e -> sprintf "Return(\n    %s\n  )" (pp_exp e)

let pp_function_def (Function (name, body)) =
  sprintf "Function(\n  name=\"%s\",\n  body=%s\n)" name (pp_statement body)

let pp_program (Program f) = 
  sprintf "Program(%s)" (pp_function_def f)
