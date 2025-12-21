type reg =
  | AX
  | R10

type operand =
  | Imm of int
  | Reg of reg
  | Pseudo of string
  | Stack of int

type unary_op =
  | Neg
  | Not

type instruction =
  | Mov of operand * operand
  | Unary of unary_op * operand
  | AllocateStack of int
  | Ret

type function_def =
  { name : string
  ; instructions : instruction list
  }

type program =
  | Program of function_def
