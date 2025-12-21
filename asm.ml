type reg =
  | AX
  | DX
  | CX
  | R10
  | R11

type operand =
  | Imm of int
  | Reg of reg
  | Pseudo of string
  | Stack of int

type unary_op =
  | Neg
  | Not

type binary_op =
  | Add
  | Sub
  | Mult
  | And
  | Or
  | Xor
  | Shl
  | Shr

type instruction =
  | Mov of operand * operand
  | Unary of unary_op * operand
  | Binary of binary_op * operand * operand
  | Idiv of operand
  | Cdq
  | AllocateStack of int
  | Ret

type function_def =
  { name : string
  ; instructions : instruction list
  }

type program =
  | Program of function_def
