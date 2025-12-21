open Asm
open Printf

let emit_reg = function
  | AX -> "%eax"
  | DX -> "%edx"
  | R10 -> "%r10d"

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

let emit_instruction = function
  | Mov (src, dst) ->
      sprintf "    movl %s, %s\n" (emit_operand src) (emit_operand dst)
  | Unary (op, dst) ->
      sprintf "    %s %s\n" (emit_unary_op op) (emit_operand dst)
  | Binary (op, src, dst) ->
      sprintf "    %s %s, %s\n" (emit_binop op) (emit_operand src) (emit_operand dst)
  | Idiv src ->
      sprintf "    idivl %s\n" (emit_operand src)
  | Cdq ->
      "    cdq\n"
  | AllocateStack i ->
      if i = 0 then "" else sprintf "    subq $%d, %%rsp\n" i
  | Ret ->
      "    movq %rbp, %rsp\n    popq %rbp\n    ret\n"
  | _ -> failwith "Instruction not yet supported in emit"

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
