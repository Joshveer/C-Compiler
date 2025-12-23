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
  | Tacky.Return v -> [ Asm.Mov (convert_val v, Reg AX); Asm.Ret ]
  | Tacky.Unary (Tacky.Not, src, dst) -> [ Asm.Cmp (Asm.Imm 0, convert_val src); Asm.Mov (Asm.Imm 0, convert_val dst); Asm.SetCC (Asm.E, convert_val dst) ]
  | Tacky.Unary (op, src, dst) -> let asm_op = convert_op op in [ Asm.Mov (convert_val src, convert_val dst); Asm.Unary (asm_op, convert_val dst) ]
  | Tacky.Binary (op, src1, src2, dst) -> (
      match op with
      | Tacky.Divide | Tacky.Remainder ->
          let setup_divisor, divisor_op = match convert_val src2 with Imm _ -> ([ Asm.Mov (convert_val src2, Reg R10) ], Reg R10) | op -> ([], op) in
          let result_reg = if op = Tacky.Divide then Reg AX else Reg DX in
          [ Asm.Mov (convert_val src1, Reg AX); Asm.Cdq ] @ setup_divisor @ [ Asm.Idiv divisor_op; Asm.Mov (result_reg, convert_val dst) ]
      | Tacky.Equal | Tacky.NotEqual | Tacky.LessThan | Tacky.LessOrEqual | Tacky.GreaterThan | Tacky.GreaterOrEqual ->
          [ Asm.Cmp (convert_val src2, convert_val src1); Asm.Mov (Asm.Imm 0, convert_val dst); Asm.SetCC (convert_cc op, convert_val dst) ]
      | _ -> [ Asm.Mov (convert_val src1, convert_val dst); Asm.Binary (convert_binop op, convert_val src2, convert_val dst) ])
  | Tacky.Copy (src, dst) -> [ Asm.Mov (convert_val src, convert_val dst) ]
  | Tacky.Label l -> [ Asm.Label l ]
  | Tacky.Jump l -> [ Asm.Jmp l ]
  | Tacky.JumpIfZero (v, l) -> [ Asm.Cmp (Asm.Imm 0, convert_val v); Asm.JmpCC (Asm.E, l) ]
  | Tacky.JumpIfNotZero (v, l) -> [ Asm.Cmp (Asm.Imm 0, convert_val v); Asm.JmpCC (Asm.NE, l) ]
  | Tacky.FunCall (name, args, dst) -> convert_function_call name args dst

let rec replace_pseudo_op op map count symbols =
  match op with
  | Pseudo name ->
      (match Hashtbl.find_opt map name with
       | Some s -> Stack s
       | None ->
           (match StringMap.find_opt name symbols with
            | Some entry when (match entry.sym_attrs with StaticAttr _ -> true | _ -> false) -> Data name
            | _ -> let offset = !count in count := !count - 4; Hashtbl.add map name offset; Stack offset))
  | _ -> op

let replace_pseudo_instr instr map count symbols =
  match instr with
  | Mov (src, dst) -> Mov (replace_pseudo_op src map count symbols, replace_pseudo_op dst map count symbols)
  | Unary (op, dst) -> Unary (op, replace_pseudo_op dst map count symbols)
  | Binary (op, src, dst) -> Binary (op, replace_pseudo_op src map count symbols, replace_pseudo_op dst map count symbols)
  | Cmp (op1, op2) -> Cmp (replace_pseudo_op op1 map count symbols, replace_pseudo_op op2 map count symbols)
  | SetCC (cc, op) -> SetCC (cc, replace_pseudo_op op map count symbols)
  | Idiv op -> Idiv (replace_pseudo_op op map count symbols)
  | Push op -> Push (replace_pseudo_op op map count symbols)
  | _ -> instr

let rec fixup instrs =
  match instrs with
  | [] -> []
  | Asm.Binary (Asm.Mult, src, Asm.Stack s) :: rest -> Asm.Mov (Asm.Stack s, Asm.Reg R11) :: Asm.Binary (Asm.Mult, src, Asm.Reg R11) :: Asm.Mov (Asm.Reg R11, Asm.Stack s) :: fixup rest
  | Asm.Idiv (Asm.Imm i) :: rest -> Asm.Mov (Asm.Imm i, Asm.Reg R10) :: Asm.Idiv (Asm.Reg R10) :: fixup rest
  | Asm.Cmp (Asm.Stack s1, Asm.Stack s2) :: rest -> Asm.Mov (Asm.Stack s1, Asm.Reg R10) :: Asm.Cmp (Asm.Reg R10, Asm.Stack s2) :: fixup rest
  | Asm.Cmp (src, Asm.Imm i) :: rest -> Asm.Mov (Asm.Imm i, Asm.Reg R11) :: Asm.Cmp (src, Asm.Reg R11) :: fixup rest
  | Asm.Mov (Asm.Stack s, Asm.Data d) :: rest -> Asm.Mov (Asm.Stack s, Asm.Reg R10) :: Asm.Mov (Asm.Reg R10, Asm.Data d) :: fixup rest
  | Asm.Mov (Asm.Data d, Asm.Stack s) :: rest -> Asm.Mov (Asm.Data d, Asm.Reg R10) :: Asm.Mov (Asm.Reg R10, Asm.Stack s) :: fixup rest
  | Asm.Mov (Asm.Data d1, Asm.Data d2) :: rest -> Asm.Mov (Asm.Data d1, Asm.Reg R10) :: Asm.Mov (Asm.Reg R10, Asm.Data d2) :: fixup rest
  | Asm.Mov (Asm.Stack s1, Asm.Stack s2) :: rest -> Asm.Mov (Asm.Stack s1, Asm.Reg R10) :: Asm.Mov (Asm.Reg R10, Asm.Stack s2) :: fixup rest
  | Asm.Binary (op, Asm.Data d, dst) :: rest -> Asm.Mov (Asm.Data d, Asm.Reg R10) :: Asm.Binary (op, Asm.Reg R10, dst) :: fixup rest
  | Asm.Binary (op, src, Asm.Data d) :: rest -> Asm.Mov (src, Asm.Reg R10) :: Asm.Binary (op, Asm.Reg R10, Asm.Data d) :: fixup rest
  | Asm.Binary (op, Asm.Stack s1, Asm.Stack s2) :: rest -> Asm.Mov (Asm.Stack s1, Asm.Reg R10) :: Asm.Binary (op, Asm.Reg R10, Asm.Stack s2) :: fixup rest
  | Asm.Push (Asm.Stack s) :: rest -> Asm.Mov (Asm.Stack s, Asm.Reg R10) :: Asm.Push (Asm.Reg R10) :: fixup rest
  | i :: rest -> i :: fixup rest

let gen_function top symbols = match top with
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
