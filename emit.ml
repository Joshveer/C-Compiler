open Asm
open Printf

let emit_register = function
  | EAX -> "%eax"

let emit_operand = function
  | Imm n -> sprintf "$%d" n
  | Reg r -> emit_register r

let emit_instruction = function
  | Mov (src, dst) ->
      sprintf "  mov %s, %s\n"
        (emit_operand src)
        (emit_operand dst)
  | Ret ->
      "  ret\n"

let emit_function f =
  let buf = Buffer.create 64 in
  bprintf buf ".globl _%s\n" f.name;
  bprintf buf "_%s:\n" f.name;
  List.iter (fun i -> Buffer.add_string buf (emit_instruction i)) f.instructions;
  Buffer.contents buf

let emit_program = function
  | Program f -> emit_function f
