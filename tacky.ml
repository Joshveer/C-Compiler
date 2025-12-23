open Printf

type unary_operator = Complement | Negate | Not

type binary_operator =
  | Add | Subtract | Multiply | Divide | Remainder | BitAnd | BitOr | Xor
  | ShiftLeft | ShiftRight | Equal | NotEqual | LessThan | LessOrEqual | GreaterThan | GreaterOrEqual
  | And | Or

type v = Constant of int | Var of string

type instruction =
  | Return of v
  | Unary of unary_operator * v * v
  | Binary of binary_operator * v * v * v
  | Jump of string
  | JumpIfZero of v * string
  | JumpIfNotZero of v * string
  | Label of string
  | Copy of v * v
  | FunCall of string * v list * v

type top_level =
  | Function of string * bool * string list * instruction list
  | StaticVariable of string * bool * int

type program = Program of top_level list

let pp_unop = function Complement -> "Complement" | Negate -> "Negate" | Not -> "Not"

let pp_binop = function
  | Add -> "Add" | Subtract -> "Subtract" | Multiply -> "Multiply" | Divide -> "Divide"
  | Remainder -> "Remainder" | BitAnd -> "BitAnd" | BitOr -> "BitOr" | Xor -> "Xor"
  | ShiftLeft -> "ShiftLeft" | ShiftRight -> "ShiftRight" | Equal -> "Equal" | NotEqual -> "NotEqual"
  | LessThan -> "LessThan" | LessOrEqual -> "LessOrEqual" | GreaterThan -> "GreaterThan" | GreaterOrEqual -> "GreaterOrEqual"
  | And -> "And" | Or -> "Or"

let pp_val = function Constant i -> sprintf "Constant(%d)" i | Var s -> sprintf "Var(%s)" s

let pp_instruction = function
  | Return v -> sprintf "Return(%s)" (pp_val v)
  | Unary (op, src, dst) -> sprintf "Unary(%s, %s, %s)" (pp_unop op) (pp_val src) (pp_val dst)
  | Binary (op, src1, src2, dst) -> sprintf "Binary(%s, %s, %s, %s)" (pp_binop op) (pp_val src1) (pp_val src2) (pp_val dst)
  | Jump target -> sprintf "Jump(%s)" target
  | JumpIfZero (v, target) -> sprintf "JumpIfZero(%s, %s)" (pp_val v) target
  | JumpIfNotZero (v, target) -> sprintf "JumpIfNotZero(%s, %s)" (pp_val v) target
  | Label target -> sprintf "Label(%s)" target
  | Copy (src, dst) -> sprintf "Copy(%s, %s)" (pp_val src) (pp_val dst)
  | FunCall (name, args, dst) -> sprintf "FunCall(%s, [%s], %s)" name (String.concat ", " (List.map pp_val args)) (pp_val dst)

let pp_top_level = function
  | Function (name, global, params, body) ->
      sprintf "Function(%s, %b, [%s], [%s])" name global (String.concat ", " params) (String.concat "; " (List.map pp_instruction body))
  | StaticVariable (name, global, init) ->
      sprintf "StaticVariable(%s, %b, %d)" name global init

let pp_program (Program tops) = String.concat "\n" (List.map pp_top_level tops)
