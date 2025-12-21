open Asm
open Printf

let emit_reg = function
  | AX -> "%eax"
  | CX -> "%ecx"
  | DX -> "%edx"
  | R10 -> "%r10d"
  | R11 -> "%r11d"

let emit_reg8 = function
  | AX -> "%al"
  | CX -> "%cl"
  | DX -> "%dl"
  | R10 -> "%r10b"
  | R11 -> "%r11b"

let emit_operand = function
  | Imm i -> sprintf "$%d" i
  | Reg r -> emit_reg r
  | Stack i -> sprintf "%d(%%rbp)" i
  | Pseudo _ -> failwith "Pseudo operand should have been replaced"

let emit_unary_op = function
  | Neg -> "negl"
  | Not -> "notl"

let emit_binop = function
  | Add -> "addl"
  | Sub -> "subl"
  | Mult -> "imull"
  | And -> "andl"
  | Or -> "orl"
  | Xor -> "xorl"
  | Shl -> "sall"
  | Shr -> "sarl"

let emit_cc = function
  | E -> "e"
  | NE -> "ne"
  | G -> "g"
  | GE -> "ge"
  | L -> "l"
  | LE -> "le"

let emit_local_label l =
  sprintf "L%s" l

let emit_instruction = function
  | Mov (src, dst) ->
      sprintf "    movl %s, %s\n" (emit_operand src) (emit_operand dst)
  | Unary (op, dst) ->
      sprintf "    %s %s\n" (emit_unary_op op) (emit_operand dst)
  | Binary (op, src, dst) ->
      let src_str = 
        match op, src with
        | (Shl, Reg CX) | (Shr, Reg CX) -> "%cl"
        | _ -> emit_operand src 
      in
      sprintf "    %s %s, %s\n" (emit_binop op) src_str (emit_operand dst)
  | Cmp (op1, op2) ->
      sprintf "    cmpl %s, %s\n" (emit_operand op1) (emit_operand op2)
  | Idiv src ->
      sprintf "    idivl %s\n" (emit_operand src)
  | Cdq ->
      "    cdq\n"
  | Jmp target ->
      sprintf "    jmp %s\n" (emit_local_label target)
  | JmpCC (cc, target) ->
      sprintf "    j%s %s\n" (emit_cc cc) (emit_local_label target)
  | SetCC (cc, op) ->
      let op_str = match op with
        | Reg r -> emit_reg8 r
        | _ -> emit_operand op
      in
      sprintf "    set%s %s\n" (emit_cc cc) op_str
  | Label l ->
      sprintf "%s:\n" (emit_local_label l)
  | AllocateStack i ->
      if i = 0 then "" else sprintf "    subq $%d, %%rsp\n" i
  | Ret ->
      "    movq %rbp, %rsp\n    popq %rbp\n    ret\n"

let emit_function f =
  let name = "_" ^ f.name in
  let buf = Buffer.create 1024 in
  bprintf buf "    .globl %s\n" name;
  bprintf buf "%s:\n" name;
  bprintf buf "    pushq %%rbp\n";
  bprintf buf "    movq %%rsp, %%rbp\n";
  List.iter (fun i -> Buffer.add_string buf (emit_instruction i)) f.instructions;
  Buffer.contents buf

let emit_program (Program f) =
  emit_function f
