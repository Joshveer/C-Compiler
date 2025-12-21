type register =
  | EAX

type operand =
  | Imm of int
  | Reg of register

type unary_op =
  | Neg
  | Not

type instruction =
  | Mov of operand * operand
  | Unary of unary_op * operand
  | Ret

type function_def =
  { name : string
  ; instructions : instruction list
  }

type program =
  | Program of function_def
