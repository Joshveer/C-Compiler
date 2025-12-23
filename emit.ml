open Asm
open Printf

let emit_reg = function AX -> "%eax" | CX -> "%ecx" | DX -> "%edx" | DI -> "%edi" | SI -> "%esi" | R8 -> "%r8d" | R9 -> "%r9d" | R10 -> "%r10d" | R11 -> "%r11d"
let emit_reg64 = function AX -> "%rax" | CX -> "%rcx" | DX -> "%rdx" | DI -> "%rdi" | SI -> "%rsi" | R8 -> "%r8" | R9 -> "%r9" | R10 -> "%r10" | R11 -> "%r11"
let emit_reg8 = function AX -> "%al" | CX -> "%cl" | DX -> "%dl" | DI -> "%dil" | SI -> "%sil" | R8 -> "%r8b" | R9 -> "%r9b" | R10 -> "%r10b" | R11 -> "%r11b"

let emit_operand = function Imm i -> sprintf "$%d" i | Reg r -> emit_reg r | Stack i -> sprintf "%d(%%rbp)" i | Pseudo _ -> failwith "Pseudo error"
let emit_operand64 = function Imm i -> sprintf "$%d" i | Reg r -> emit_reg64 r | Stack i -> sprintf "%d(%%rbp)" i | Pseudo _ -> failwith "Pseudo error"

let emit_unary_op = function Neg -> "negl" | Not -> "notl"
let emit_binop = function Add -> "addl" | Sub -> "subl" | Mult -> "imull" | And -> "andl" | Or -> "orl" | Xor -> "xorl" | Shl -> "sall" | Shr -> "sarl"
let emit_cc = function E -> "e" | NE -> "ne" | G -> "g" | GE -> "ge" | L -> "l" | LE -> "le"
let emit_local_label l = sprintf "L%s" l

let emit_instruction = function
  | Mov (src, dst) -> sprintf "    movl %s, %s\n" (emit_operand src) (emit_operand dst)
  | Unary (op, dst) -> sprintf "    %s %s\n" (emit_unary_op op) (emit_operand dst)
  | Binary (op, src, dst) ->
      let src_str = match op, src with (Shl, Reg CX) | (Shr, Reg CX) -> "%cl" | _ -> emit_operand src in
      sprintf "    %s %s, %s\n" (emit_binop op) src_str (emit_operand dst)
  | Cmp (op1, op2) -> sprintf "    cmpl %s, %s\n" (emit_operand op1) (emit_operand op2)
  | Idiv src -> sprintf "    idivl %s\n" (emit_operand src)
  | Cdq -> "    cdq\n"
  | Jmp target -> sprintf "    jmp %s\n" (emit_local_label target)
  | JmpCC (cc, target) -> sprintf "    j%s %s\n" (emit_cc cc) (emit_local_label target)
  | SetCC (cc, op) -> sprintf "    set%s %s\n" (emit_cc cc) (match op with Reg r -> emit_reg8 r | _ -> emit_operand op)
  | Label l -> sprintf "%s:\n" (emit_local_label l)
  | AllocateStack i -> sprintf "    subq $%d, %%rsp\n" i
  | DeallocateStack i -> sprintf "    addq $%d, %%rsp\n" i
  | Push op -> sprintf "    pushq %s\n" (emit_operand64 op)
  | Call name -> let target = if Sys.os_type = "Unix" then name ^ "@PLT" else "_" ^ name in sprintf "    call %s\n" target
  | Ret -> "    movq %rbp, %rsp\n    popq %rbp\n    ret\n"

let emit_function { name; instructions } =
  let prefix = if Sys.os_type = "Unix" then "" else "_" in
  let name = prefix ^ name in
  sprintf "    .globl %s\n%s:\n    pushq %%rbp\n    movq %%rsp, %%rbp\n%s" name name (String.concat "" (List.map emit_instruction instructions))

let emit_program (Program funs) =
  let text = String.concat "\n" (List.map emit_function funs) in
  if Sys.os_type = "Unix" then text ^ "\n    .section .note.GNU-stack,\"\",@progbits\n" else text
