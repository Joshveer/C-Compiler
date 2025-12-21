type register =
  | EAX

type operand =
  | Imm of int
  | Reg of register

type instruction =
  | Mov of operand * operand
  | Ret

type function_def =
  { name : string
  ; instructions : instruction list
  }

type program =
  | Program of function_def
