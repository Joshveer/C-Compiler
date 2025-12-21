open Asm
open Tacky

let convert_op = function
  | Tacky.Complement -> Asm.Not
  | Tacky.Negate -> Asm.Neg

let convert_val = function
  | Tacky.Constant i -> Asm.Imm i
  | Tacky.Var v -> Asm.Pseudo v

let convert_instruction = function
  | Tacky.Return v ->
      [ Asm.Mov (convert_val v, Reg AX); Asm.Ret ]
  | Tacky.Unary (op, src, dst) ->
      let asm_op = convert_op op in
      let src_op = convert_val src in
      let dst_op = convert_val dst in
      [ Asm.Mov (src_op, dst_op); Asm.Unary (asm_op, dst_op) ]

let rec convert_instrs = function
  | [] -> []
  | i :: rest -> convert_instruction i @ convert_instrs rest

let replace_pseudos instrs =
  let offset = ref 0 in
  let mapping = Hashtbl.create 10 in
  
  let get_stack_loc name =
    if Hashtbl.mem mapping name then
      Hashtbl.find mapping name
    else begin
      offset := !offset - 4;
      Hashtbl.add mapping name !offset;
      !offset
    end
  in

  let replace_operand = function
    | Asm.Pseudo name -> Asm.Stack (get_stack_loc name)
    | other -> other
  in

  let rec replace_in_instrs = function
    | [] -> []
    | Asm.Mov (src, dst) :: rest ->
        Asm.Mov (replace_operand src, replace_operand dst) :: replace_in_instrs rest
    | Asm.Unary (op, dst) :: rest ->
        Asm.Unary (op, replace_operand dst) :: replace_in_instrs rest
    | Asm.Ret :: rest ->
        Asm.Ret :: replace_in_instrs rest
    | Asm.AllocateStack i :: rest ->
        Asm.AllocateStack i :: replace_in_instrs rest
  in
  
  (replace_in_instrs instrs, -(!offset))

let fixup_program stack_size instrs =
  let rec fixup = function
    | [] -> []
    | Asm.Mov (Asm.Stack s1, Asm.Stack s2) :: rest ->
        Asm.Mov (Asm.Stack s1, Asm.Reg R10) ::
        Asm.Mov (Asm.Reg R10, Asm.Stack s2) ::
        fixup rest
    | i :: rest -> i :: fixup rest
  in
  Asm.AllocateStack stack_size :: fixup instrs

let gen_function func_def =
  let name = func_def.Tacky.name in
  let body = func_def.Tacky.body in
  let initial_instrs = convert_instrs body in
  let instrs_with_stack, stack_size = replace_pseudos initial_instrs in
  let final_instrs = fixup_program stack_size instrs_with_stack in
  { Asm.name = name; Asm.instructions = final_instrs }

let gen_program (Tacky.Program f) =
  Asm.Program (gen_function f)
