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

type for_init =
  | InitDecl of declaration
  | InitExp of exp option

type switch_cases = {
  case_list : (exp * identifier) list;
  default_label : identifier option;
}

type statement =
  | Return of exp
  | Expression of exp
  | If of exp * statement * statement option
  | Compound of block
  | Goto of identifier
  | Label of identifier * statement
  | While of exp * statement * identifier option
  | DoWhile of statement * exp * identifier option
  | For of for_init * exp option * exp option * statement * identifier option
  | Break of identifier option
  | Continue of identifier option
  | Switch of exp * statement * identifier option * switch_cases option
  | Case of exp * statement * identifier option
  | Default of statement * identifier option
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

let pp_for_init = function
  | InitDecl d -> pp_declaration d
  | InitExp (Some e) -> pp_exp e
  | InitExp None -> "None"

let pp_opt_exp = function
  | Some e -> pp_exp e
  | None -> "None"

let pp_label = function
  | Some l -> sprintf "Label(%s)" l
  | None -> "NoLabel"

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
  | While (cond, body, lbl) ->
      sprintf "While(%s, %s, %s)" (pp_exp cond) (pp_statement body) (pp_label lbl)
  | DoWhile (body, cond, lbl) ->
      sprintf "DoWhile(%s, %s, %s)" (pp_statement body) (pp_exp cond) (pp_label lbl)
  | For (init, cond, post, body, lbl) ->
      sprintf "For(%s, %s, %s, %s, %s)" 
        (pp_for_init init) (pp_opt_exp cond) (pp_opt_exp post) (pp_statement body) (pp_label lbl)
  | Break lbl -> sprintf "Break(%s)" (pp_label lbl)
  | Continue lbl -> sprintf "Continue(%s)" (pp_label lbl)
  | Switch (cond, body, lbl, _) ->
      sprintf "Switch(%s, %s, %s)" (pp_exp cond) (pp_statement body) (pp_label lbl)
  | Case (exp, stmt, lbl) ->
      sprintf "Case(%s, %s, %s)" (pp_exp exp) (pp_statement stmt) (pp_label lbl)
  | Default (stmt, lbl) ->
      sprintf "Default(%s, %s)" (pp_statement stmt) (pp_label lbl)
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
