type reg = AX | CX | DX | DI | SI | R8 | R9 | R10 | R11

type operand = Imm of int | Reg of reg | Pseudo of string | Stack of int | Data of string

type unary_op = Neg | Not
type binary_op = Add | Sub | Mult | And | Or | Xor | Shl | Shr

type cond_code = E | NE | G | GE | L | LE

type instruction =
  | Mov of operand * operand
  | Unary of unary_op * operand
  | Binary of binary_op * operand * operand
  | Cmp of operand * operand
  | Idiv of operand
  | Cdq
  | Jmp of string
  | JmpCC of cond_code * string
  | SetCC of cond_code * operand
  | Label of string
  | AllocateStack of int
  | DeallocateStack of int
  | Push of operand
  | Call of string
  | Ret

type top_level =
  | Function of string * bool * instruction list
  | StaticVariable of string * bool * int

type program = Program of top_level list
