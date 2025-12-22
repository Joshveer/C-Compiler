open Printf

type unary_op =
  | Complement
  | Negate
  | Not

type binary_op =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Remainder
  | BitAnd
  | BitOr
  | Xor
  | ShiftLeft
  | ShiftRight
  | And
  | Or
  | Equal
  | NotEqual
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual

type identifier = string

type exp =
  | Constant of int
  | Var of identifier
  | Unary of unary_op * exp
  | Binary of binary_op * exp * exp
  | Assignment of exp * exp
  | CompoundAssignment of binary_op * exp * exp
  | PrefixIncrement of exp
  | PostfixIncrement of exp
  | PrefixDecrement of exp
  | PostfixDecrement of exp
  | Conditional of exp * exp * exp

type declaration = Declaration of identifier * exp option

type statement =
  | Return of exp
  | Expression of exp
  | If of exp * statement * statement option
  | Compound of block
  | Goto of identifier
  | Label of identifier * statement
  | Null

and block_item =
  | S of statement
  | D of declaration

and block = Block of block_item list

type function_def =
  | Function of identifier * block_item list

type program =
  | Program of function_def

let pp_unop = function
  | Complement -> "~"
  | Negate -> "-"
  | Not -> "!"

let pp_binop = function
  | Add -> "+"
  | Subtract -> "-"
  | Multiply -> "*"
  | Divide -> "/"
  | Remainder -> "%"
  | BitAnd -> "&"
  | BitOr -> "|"
  | Xor -> "^"
  | ShiftLeft -> "<<"
  | ShiftRight -> ">>"
  | And -> "&&"
  | Or -> "||"
  | Equal -> "=="
  | NotEqual -> "!="
  | LessThan -> "<"
  | LessOrEqual -> "<="
  | GreaterThan -> ">"
  | GreaterOrEqual -> ">="

let rec pp_exp = function
  | Constant i -> sprintf "Constant(%d)" i
  | Var id -> sprintf "Var(%s)" id
  | Unary (op, e) ->
      sprintf "Unary(%s, %s)" (pp_unop op) (pp_exp e)
  | Binary (op, e1, e2) ->
      sprintf "Binary(%s, %s, %s)" (pp_binop op) (pp_exp e1) (pp_exp e2)
  | Assignment (e1, e2) ->
      sprintf "Assignment(%s, %s)" (pp_exp e1) (pp_exp e2)
  | CompoundAssignment (op, e1, e2) ->
      sprintf "CompoundAssignment(%s, %s, %s)" (pp_binop op) (pp_exp e1) (pp_exp e2)
  | PrefixIncrement e -> sprintf "PrefixIncrement(%s)" (pp_exp e)
  | PostfixIncrement e -> sprintf "PostfixIncrement(%s)" (pp_exp e)
  | PrefixDecrement e -> sprintf "PrefixDecrement(%s)" (pp_exp e)
  | PostfixDecrement e -> sprintf "PostfixDecrement(%s)" (pp_exp e)
  | Conditional (e1, e2, e3) ->
      sprintf "Conditional(%s, %s, %s)" (pp_exp e1) (pp_exp e2) (pp_exp e3)

let pp_declaration = function
  | Declaration (id, None) -> sprintf "Declaration(%s)" id
  | Declaration (id, Some e) -> sprintf "Declaration(%s, %s)" id (pp_exp e)

let rec pp_statement = function
  | Return e -> sprintf "Return(%s)" (pp_exp e)
  | Expression e -> sprintf "Expression(%s)" (pp_exp e)
  | If (cond, then_s, Some else_s) ->
      sprintf "If(%s, %s, %s)" (pp_exp cond) (pp_statement then_s) (pp_statement else_s)
  | If (cond, then_s, None) ->
      sprintf "If(%s, %s, Null)" (pp_exp cond) (pp_statement then_s)
  | Compound (Block items) ->
      let items_str = List.map pp_block_item items |> String.concat ", " in
      sprintf "Compound([%s])" items_str
  | Goto label -> sprintf "Goto(%s)" label
  | Label (label, s) -> sprintf "Label(%s, %s)" label (pp_statement s)
  | Null -> "Null"

and pp_block_item = function
  | S s -> pp_statement s
  | D d -> pp_declaration d

let pp_program (Program (Function (name, body))) =
  let body_str =
    body
    |> List.map (fun item -> "  " ^ pp_block_item item)
    |> String.concat "\n"
  in
  sprintf "Program(Function(%s, [\n%s\n]))" name body_str
