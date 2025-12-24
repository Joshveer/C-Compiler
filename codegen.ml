open Asm
open Tacky
open Semanticanalysis

let convert_op = function Tacky.Complement -> Asm.Not | Tacky.Negate -> Asm.Neg | _ -> failwith "Internal Error"
let convert_binop = function
  | Tacky.Add -> Asm.Add | Tacky.Subtract -> Asm.Sub | Tacky.Multiply -> Asm.Mult
  | Tacky.BitAnd -> Asm.And | Tacky.BitOr -> Asm.Or | Tacky.Xor -> Asm.Xor
  | Tacky.ShiftLeft -> Asm.Shl | Tacky.ShiftRight -> Asm.Shr | _ -> failwith "Internal Error"

let convert_cc = function
  | Tacky.Equal -> Asm.E | Tacky.NotEqual -> Asm.NE | Tacky.LessThan -> Asm.L
  | Tacky.LessOrEqual -> Asm.LE | Tacky.GreaterThan -> Asm.G | Tacky.GreaterOrEqual -> Asm.GE
  | _ -> failwith "Internal Error"

let convert_val = function Tacky.Constant i -> Asm.Imm i | Tacky.Var v -> Asm.Pseudo v

let arg_registers = [ DI; SI; DX; CX; R8; R9 ]

let convert_function_call fun_name args dst =
  let register_args = List.mapi (fun i a -> (i, a)) args |> List.filter (fun (i, _) -> i < 6) |> List.map snd in
  let stack_args = List.mapi (fun i a -> (i, a)) args |> List.filter (fun (i, _) -> i >= 6) |> List.map snd in
  let stack_padding = if List.length stack_args mod 2 = 1 then 8 else 0 in
  let instrs = ref [] in
  if stack_padding <> 0 then instrs := !instrs @ [ AllocateStack stack_padding ];
  List.iteri (fun i arg -> instrs := !instrs @ [ Mov (convert_val arg, Reg (List.nth arg_registers i)) ]) register_args;
  List.iter (fun arg ->
      let asm_arg = convert_val arg in
      match asm_arg with Reg _ | Imm _ -> instrs := !instrs @ [ Push asm_arg ] | _ -> instrs := !instrs @ [ Mov (asm_arg, Reg AX); Push (Reg AX) ]
    ) (List.rev stack_args);
  instrs := !instrs @ [ Call fun_name ];
  let bytes_to_remove = (8 * List.length stack_args) + stack_padding in
  if bytes_to_remove <> 0 then instrs := !instrs @ [ DeallocateStack bytes_to_remove ];
  instrs := !instrs @ [ Mov (Reg AX, convert_val dst) ];
  !instrs

let convert_instruction = function
  | Tacky.Return v -> [ Mov (convert_val v, Reg AX); Ret ]
  | Tacky.Unary (op, src, dst) -> [ Mov (convert_val src, convert_val dst); Unary (convert_op op, convert_val dst) ]
  | Tacky.Binary (op, src1, src2, dst) ->
      (match op with
      | Tacky.Divide | Tacky.Remainder ->
          [ Mov (convert_val src1, Reg AX); Cdq; Idiv (convert_val src2);
            Mov (Reg (if op = Tacky.Divide then AX else DX), convert_val dst) ]
      | _ ->
          [ Mov (convert_val src1, convert_val dst); Binary (convert_binop op, convert_val src2, convert_val dst) ])
  | Tacky.Jump target -> [ Jmp target ]
  | Tacky.JumpIfZero (v, target) -> [ Cmp (Imm 0, convert_val v); JmpCC (E, target) ]
  | Tacky.JumpIfNotZero (v, target) -> [ Cmp (Imm 0, convert_val v); JmpCC (NE, target) ]
  | Tacky.Label target -> [ Label target ]
  | Tacky.Copy (src, dst) -> [ Mov (convert_val src, convert_val dst) ]
  | Tacky.FunCall (name, args, dst) -> convert_function_call name args dst

let rec replace_pseudo_instr instr map count symbols =
  let get_offset pseudo =
    if Hashtbl.mem map pseudo then Hashtbl.find map pseudo
    else
      match StringMap.find_opt pseudo symbols with
      | Some { Semanticanalysis.sym_attrs = Semanticanalysis.StaticAttr _ } ->
          Asm.Data pseudo
      | _ ->
          let offset = !count in count := !count - 4;
          let operand = Asm.Stack offset in
          Hashtbl.add map pseudo operand;
          operand
  in
  let replace_operand = function
    | Pseudo p -> get_offset p
    | other -> other
  in
  match instr with
  | Mov (src, dst) -> Mov (replace_operand src, replace_operand dst)
  | Unary (op, dst) -> Unary (op, replace_operand dst)
  | Binary (op, src, dst) -> Binary (op, replace_operand src, replace_operand dst)
  | Cmp (src, dst) -> Cmp (replace_operand src, replace_operand dst)
  | Idiv op -> Idiv (replace_operand op)
  | SetCC (cc, dst) -> SetCC (cc, replace_operand dst)
  | Push op -> Push (replace_operand op)
  | other -> other

let fixup instrs =
  let rec loop acc = function
    | [] -> List.rev acc
    | Mov (Stack s1, Stack s2) :: rest ->
        loop (Mov (Reg R10, Stack s2) :: Mov (Stack s1, Reg R10) :: acc) rest
    | Mov (Data d1, Data d2) :: rest ->
        loop (Mov (Reg R10, Data d2) :: Mov (Data d1, Reg R10) :: acc) rest
    | Mov (Data d, Stack s) :: rest ->
        loop (Mov (Reg R10, Stack s) :: Mov (Data d, Reg R10) :: acc) rest
    | Mov (Stack s, Data d) :: rest ->
        loop (Mov (Reg R10, Data d) :: Mov (Stack s, Reg R10) :: acc) rest
    | Binary (op, Stack s1, Stack s2) :: rest ->
        loop (Binary (op, Reg R10, Stack s2) :: Mov (Stack s1, Reg R10) :: acc) rest
    | Binary (op, Data d1, Data d2) :: rest ->
        loop (Binary (op, Reg R10, Data d2) :: Mov (Data d1, Reg R10) :: acc) rest
    | Binary (op, Data d, Stack s) :: rest ->
        loop (Binary (op, Reg R10, Stack s) :: Mov (Data d, Reg R10) :: acc) rest
    | Binary (op, Stack s, Data d) :: rest ->
        loop (Binary (op, Reg R10, Data d) :: Mov (Stack s, Reg R10) :: acc) rest
    | Binary (Mult, Stack s, Reg r) :: rest ->
        loop (Binary (Mult, Reg R11, Reg r) :: Mov (Stack s, Reg R11) :: acc) rest
    | Binary (Mult, Data d, Reg r) :: rest ->
        loop (Binary (Mult, Reg R11, Reg r) :: Mov (Data d, Reg R11) :: acc) rest
    | Idiv (Imm i) :: rest ->
        loop (Idiv (Reg R10) :: Mov (Imm i, Reg R10) :: acc) rest
    | Cmp (Stack s1, Stack s2) :: rest ->
        loop (Cmp (Reg R10, Stack s2) :: Mov (Stack s1, Reg R10) :: acc) rest
    | Cmp (Data d1, Data d2) :: rest ->
        loop (Cmp (Reg R10, Data d2) :: Mov (Data d1, Reg R10) :: acc) rest
    | Cmp (Data d, Stack s) :: rest ->
        loop (Cmp (Reg R10, Stack s) :: Mov (Data d, Reg R10) :: acc) rest
    | Cmp (Stack s, Data d) :: rest ->
        loop (Cmp (Reg R10, Data d) :: Mov (Stack s, Reg R10) :: acc) rest
    | Cmp (Imm i, Stack s) :: rest ->
        loop (Cmp (Reg R10, Stack s) :: Mov (Imm i, Reg R10) :: acc) rest
    | Cmp (Imm i, Data d) :: rest ->
        loop (Cmp (Reg R10, Data d) :: Mov (Imm i, Reg R10) :: acc) rest
    | i :: rest -> loop (i :: acc) rest
  in
  loop [] instrs

let gen_function top symbols =
  match top with
  | Tacky.Function (name, global, params, body) ->
      let param_instrs = ref [] in
      List.iteri (fun i p -> if i < 6 then param_instrs := !param_instrs @ [ Mov (Reg (List.nth arg_registers i), Pseudo p) ] else let offset = 16 + (8 * (i - 6)) in param_instrs := !param_instrs @ [ Mov (Stack offset, Pseudo p) ]) params;
      let body_instrs = List.concat (List.map convert_instruction body) in
      let instrs = !param_instrs @ body_instrs in
      let map = Hashtbl.create 10 in let count = ref (-4) in
      let replaced = List.map (fun i -> replace_pseudo_instr i map count symbols) instrs in
      let stack_size = - !count - 4 in
      let aligned_size = (stack_size + 15) / 16 * 16 in
      let fixed = fixup replaced in
      Asm.Function (name, global, AllocateStack aligned_size :: fixed)
  | Tacky.StaticVariable (name, global, init) ->
      Asm.StaticVariable (name, global, init)

let gen_program (Tacky.Program tops) symbols = Asm.Program (List.map (fun top -> gen_function top symbols) tops)