open Asm
open Printf

let is_macos = true

let emit_reg = function
  | AX -> "%eax"
  | R10 -> "%r10d"

let emit_operand = function
  | Imm i -> sprintf "$%d" i
  | Reg r -> emit_reg r
  | Stack i -> sprintf "%d(%%rbp)" i
  | Pseudo _ -> failwith "Pseudo operand should have been replaced"

let emit_unary_op = function
  | Neg -> "neg"
  | Not -> "not"

let emit_instruction = function
  | Mov (src, dst) ->
      sprintf "    movl %s, %s\n" (emit_operand src) (emit_operand dst)
  | Unary (op, dst) ->
      sprintf "    %s %s\n" (emit_unary_op op) (emit_operand dst)
  | AllocateStack i ->
      if i = 0 then "" else sprintf "    subq $%d, %%rsp\n" i
  | Ret ->
      "    movq %rbp, %rsp\n    popq %rbp\n    ret\n"

let emit_function f =
  let name = if is_macos then "_" ^ f.name else f.name in
  let buf = Buffer.create 1024 in
  bprintf buf "    .globl %s\n" name;
  bprintf buf "%s:\n" name;
  bprintf buf "    pushq %%rbp\n";
  bprintf buf "    movq %%rsp, %%rbp\n";
  List.iter (fun i -> Buffer.add_string buf (emit_instruction i)) f.instructions;
  Buffer.contents buf

let emit_program (Program f) =
  let asm = emit_function f in
  if not is_macos then
    asm ^ "\n    .section .note.GNU-stack,\"\",@progbits\n"
  else
    asm
