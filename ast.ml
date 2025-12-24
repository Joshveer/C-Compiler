open Printf

type unary_op = Complement | Negate | Not

type binary_op =
  | Add | Subtract | Multiply | Divide | Remainder | BitAnd | BitOr | Xor
  | ShiftLeft | ShiftRight | And | Or | Equal | NotEqual
  | LessThan | LessOrEqual | GreaterThan | GreaterOrEqual

type identifier = string

type storage_class = Static | Extern

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
  | FunctionCall of identifier * exp list

type function_declaration = {
  fd_name : identifier;
  fd_params : identifier list;
  fd_body : block option;
  fd_storage_class : storage_class option;
}

and variable_declaration = {
  vd_name : identifier;
  vd_init : exp option;
  vd_storage_class : storage_class option;
}

and declaration =
  | FunDecl of function_declaration
  | VarDecl of variable_declaration

and for_init = InitDecl of variable_declaration | InitExp of exp option

and switch_cases = {
  case_list : (exp * identifier) list;
  default_label : identifier option;
}

and statement =
  | Return of exp
  | Expression of exp
  | If of exp * statement * statement option
  | Compound of block
  | Break of string option
  | Continue of string option
  | While of exp * statement * string option
  | DoWhile of statement * exp * string option
  | For of for_init * exp option * exp option * statement * string option
  | Null
  | Switch of exp * statement * string option * switch_cases option
  | Case of exp * statement * string option
  | Default of statement * string option
  | Goto of string
  | Label of string * statement

and block_item =
  | S of statement
  | D of declaration

and block = Block of block_item list

type program = Program of declaration list

let rec pp_exp = function
  | Constant i -> sprintf "Constant(%d)" i
  | Var i -> sprintf "Var(%s)" i
  | Unary _ -> "Unary(...)"
  | Binary _ -> "Binary(...)"
  | Assignment _ -> "Assignment(...)"
  | CompoundAssignment _ -> "CompoundAssignment(...)"
  | PrefixIncrement _ -> "PrefixIncrement(...)"
  | PostfixIncrement _ -> "PostfixIncrement(...)"
  | PrefixDecrement _ -> "PrefixDecrement(...)"
  | PostfixDecrement _ -> "PostfixDecrement(...)"
  | Conditional _ -> "Conditional(...)"
  | FunctionCall _ -> "FunctionCall(...)"

and pp_variable_declaration d = sprintf "VarDecl(%s)" d.vd_name

and pp_block_item = function
  | S s -> pp_statement s
  | D d -> match d with VarDecl vd -> pp_variable_declaration vd | FunDecl fd -> fd.fd_name

and pp_for_init = function InitDecl d -> pp_variable_declaration d | InitExp (Some e) -> pp_exp e | InitExp None -> "None"
and pp_opt_exp = function Some e -> pp_exp e | None -> "None"
and pp_label = function Some l -> l | None -> "None"

and pp_statement = function
  | Return e -> sprintf "Return(%s)" (pp_exp e)
  | Expression e -> sprintf "Expression(%s)" (pp_exp e)
  | If (c, t, e) -> sprintf "If(%s, %s, %s)" (pp_exp c) (pp_statement t) (match e with Some s -> pp_statement s | None -> "Null")
  | Compound (Block items) -> sprintf "Compound([%s])" (String.concat ", " (List.map pp_block_item items))
  | Goto l -> sprintf "Goto(%s)" l
  | Label (l, s) -> sprintf "Label(%s, %s)" l (pp_statement s)
  | While (c, b, l) -> sprintf "While(%s, %s, %s)" (pp_exp c) (pp_statement b) (pp_label l)
  | DoWhile (b, c, l) -> sprintf "DoWhile(%s, %s, %s)" (pp_statement b) (pp_exp c) (pp_label l)
  | For (i, c, p, b, l) -> sprintf "For(%s, %s, %s, %s, %s)" (pp_for_init i) (pp_opt_exp c) (pp_opt_exp p) (pp_statement b) (pp_label l)
  | Null -> "Null"
  | Break l -> sprintf "Break(%s)" (pp_label l)
  | Continue l -> sprintf "Continue(%s)" (pp_label l)
  | Switch (e, s, l, _) -> sprintf "Switch(%s, %s, %s)" (pp_exp e) (pp_statement s) (pp_label l)
  | Case (e, s, l) -> sprintf "Case(%s, %s, %s)" (pp_exp e) (pp_statement s) (pp_label l)
  | Default (s, l) -> sprintf "Default(%s, %s)" (pp_statement s) (pp_label l)
