open Asm
open Tacky

let convert_op = function
  | Tacky.Complement -> Asm.Not
  | Tacky.Negate -> Asm.Neg

let convert_binop = function
  | Tacky.Add -> Asm.Add
  | Tacky.Subtract -> Asm.Sub
  | Tacky.Multiply -> Asm.Mult
  | _ -> failwith "Div/Rem handled separately"

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
  | Tacky.Binary (op, src1, src2, dst) ->
      match op with
      | Tacky.Divide | Tacky.Remainder ->
          let mov_eax = Asm.Mov (convert_val src1, Reg AX) in
          let cdq = Asm.Cdq in
          let src2_asm = convert_val src2 in
          let setup_divisor, divisor_op =
            match src2_asm with
            | Imm _ -> ([ Asm.Mov (src2_asm, Reg R10) ], Reg R10)
            | _ -> ([], src2_asm)
          in
          let idiv = Asm.Idiv divisor_op in
          let result_reg = if op = Tacky.Divide then Reg AX else Reg DX in
          let mov_res = Asm.Mov (result_reg, convert_val dst) in
          [ mov_eax; cdq ] @ setup_divisor @ [ idiv; mov_res ]
      | _ ->
          let asm_op = convert_binop op in
          [
            Asm.Mov (convert_val src1, convert_val dst);
            Asm.Binary (asm_op, convert_val src2, convert_val dst);
          ]

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
        Asm.Mov (replace_operand src, replace_operand dst)
        :: replace_in_instrs rest
    | Asm.Unary (op, dst) :: rest ->
        Asm.Unary (op, replace_operand dst)
        :: replace_in_instrs rest
    | Asm.Binary (op, src, dst) :: rest ->
        Asm.Binary (op, replace_operand src, replace_operand dst)
        :: replace_in_instrs rest
    | Asm.Idiv op :: rest ->
        Asm.Idiv (replace_operand op) :: replace_in_instrs rest
    | Asm.Cdq :: rest -> Asm.Cdq :: replace_in_instrs rest
    | Asm.Ret :: rest -> Asm.Ret :: replace_in_instrs rest
    | Asm.AllocateStack i :: rest ->
        Asm.AllocateStack i :: replace_in_instrs rest
    | other :: rest -> other :: replace_in_instrs rest
  in
  (replace_in_instrs instrs, -(!offset))

let fixup_program stack_size instrs =
  let rec fixup = function
    | [] -> []
    | Asm.Binary (Asm.Mult, src, Asm.Stack s) :: rest ->
        Asm.Mov (Asm.Stack s, Asm.Reg R11)
        :: Asm.Binary (Asm.Mult, src, Asm.Reg R11)
        :: Asm.Mov (Asm.Reg R11, Asm.Stack s)
        :: fixup rest
    | Asm.Idiv (Asm.Imm i) :: rest ->
        Asm.Mov (Asm.Imm i, Asm.Reg R10)
        :: Asm.Idiv (Asm.Reg R10)
        :: fixup rest
    | Asm.Mov (Asm.Stack s1, Asm.Stack s2) :: rest ->
        Asm.Mov (Asm.Stack s1, Asm.Reg R10)
        :: Asm.Mov (Asm.Reg R10, Asm.Stack s2)
        :: fixup rest
    | Asm.Binary (op, Asm.Stack s1, Asm.Stack s2) :: rest ->
        Asm.Mov (Asm.Stack s1, Asm.Reg R10)
        :: Asm.Binary (op, Asm.Reg R10, Asm.Stack s2)
        :: fixup rest
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
