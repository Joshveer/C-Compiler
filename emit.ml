open Asm
open Printf

let emit_reg = function AX -> "%eax" | CX -> "%ecx" | DX -> "%edx" | DI -> "%edi" | SI -> "%esi" | R8 -> "%r8d" | R9 -> "%r9d" | R10 -> "%r10d" | R11 -> "%r11d"
let emit_reg64 = function AX -> "%rax" | CX -> "%rcx" | DX -> "%rdx" | DI -> "%rdi" | SI -> "%rsi" | R8 -> "%r8" | R9 -> "%r9" | R10 -> "%r10" | R11 -> "%r11"
let emit_reg8 = function AX -> "%al" | CX -> "%cl" | DX -> "%dl" | DI -> "%dil" | SI -> "%sil" | R8 -> "%r8b" | R9 -> "%r9b" | R10 -> "%r10b" | R11 -> "%r11b"

let emit_operand = function Imm i -> sprintf "$%d" i | Reg r -> emit_reg r | Stack i -> sprintf "%d(%%rbp)" i | Data s -> let prefix = if Sys.os_type = "Unix" then "_" else "" in sprintf "%s%s(%%rip)" prefix s | Pseudo _ -> failwith "Pseudo error"
let emit_operand64 = function Imm i -> sprintf "$%d" i | Reg r -> emit_reg64 r | Stack i -> sprintf "%d(%%rbp)" i | Data s -> let prefix = if Sys.os_type = "Unix" then "_" else "" in sprintf "%s%s(%%rip)" prefix s | Pseudo _ -> failwith "Pseudo error"

let emit_cond_code = function E -> "e" | NE -> "ne" | G -> "g" | GE -> "ge" | L -> "l" | LE -> "le"

let emit_local_label l = sprintf ".%s" l

let emit_instruction = function
  | Mov (src, dst) ->
      (match (src, dst) with
      | (Reg _, Reg _) | (Imm _, Reg _) | (Stack _, Reg _) | (Data _, Reg _) -> sprintf "    movl %s, %s\n" (emit_operand src) (emit_operand dst)
      | (Reg _, Stack _) | (Imm _, Stack _) | (Reg _, Data _) -> sprintf "    movl %s, %s\n" (emit_operand src) (emit_operand dst)
      | _ -> failwith "Invalid operand combination")
  | Unary (op, dst) ->
      let instr = match op with Neg -> "neg" | Not -> "not" in
      sprintf "    %sl %s\n" instr (emit_operand dst)
  | Binary (op, src, dst) ->
      let instr = match op with Add -> "add" | Sub -> "sub" | Mult -> "imul" | And -> "and" | Or -> "or" | Xor -> "xor" | Shl -> "shl" | Shr -> "sar" in
      sprintf "    %sl %s, %s\n" instr (emit_operand src) (emit_operand dst)
  | Cmp (src, dst) -> sprintf "    cmpl %s, %s\n" (emit_operand src) (emit_operand dst)
  | Idiv op -> sprintf "    idivl %s\n" (emit_operand op)
  | Cdq -> "    cdq\n"
  | Jmp target -> sprintf "    jmp %s\n" (emit_local_label target)
  | JmpCC (cc, target) -> sprintf "    j%s %s\n" (emit_cond_code cc) (emit_local_label target)
  | SetCC (cc, dst) ->
      let r = match dst with Reg r -> r | _ -> failwith "SetCC only supports register" in
      sprintf "    set%s %s\n    movzbl %s, %s\n" (emit_cond_code cc) (emit_reg8 r) (emit_reg8 r) (emit_reg r)
  | Label l -> sprintf "%s:\n" (emit_local_label l)
  | AllocateStack i -> sprintf "    subq $%d, %%rsp\n" i
  | DeallocateStack i -> sprintf "    addq $%d, %%rsp\n" i
  | Push op -> sprintf "    pushq %s\n" (emit_operand64 op)
  | Call name -> let target = "_" ^ name in sprintf "    call %s\n" target
  | Ret -> "    movq %rbp, %rsp\n    popq %rbp\n    ret\n"

let emit_top_level = function
  | Function (name, global, instructions) ->
      let prefix = if Sys.os_type = "Unix" then "_" else "" in
      let name = prefix ^ name in
      let global_directive = if global then sprintf "    .globl %s\n" name else "" in
      sprintf "%s%s:\n    pushq %%rbp\n    movq %%rsp, %%rbp\n%s" global_directive name (String.concat "" (List.map emit_instruction instructions))
  | StaticVariable (name, global, init) ->
      let prefix = if Sys.os_type = "Unix" then "_" else "" in
      let name = prefix ^ name in
      let global_directive = if global then sprintf "    .globl %s\n" name else "" in
      let section = if init = 0 then ".bss" else ".data" in
      let alignment = if Sys.os_type = "Unix" then "    .balign 4\n" else "    .align 4\n" in
      let value = if init = 0 then sprintf "    .zero 4\n" else sprintf "    .long %d\n" init in
      sprintf "%s    %s\n%s%s:\n%s%s" section alignment global_directive name alignment value

let emit_program (Program tops) =
  let code = String.concat "\n" (List.map emit_top_level tops) in
  sprintf "    .section .note.GNU-stack,\"\",@progbits\n%s\n" code
