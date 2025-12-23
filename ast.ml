open Printf

type unary_op = Complement | Negate | Not

type binary_op =
  | Add | Subtract | Multiply | Divide | Remainder | BitAnd | BitOr | Xor
  | ShiftLeft | ShiftRight | And | Or | Equal | NotEqual
  | LessThan | LessOrEqual | GreaterThan | GreaterOrEqual

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
  | FunctionCall of identifier * exp list

type declaration = Declaration of identifier * exp option

type for_init = InitDecl of declaration | InitExp of exp option

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

and block_item = S of statement | D of declaration

and block = Block of block_item list

type function_definition =
  | Function of {
      name : identifier;
      params : identifier list;
      body : block option;
    }

type program = Program of function_definition list

let pp_unary_op = function
  | Complement -> "~" | Negate -> "-" | Not -> "!"

let pp_binary_op = function
  | Add -> "+" | Subtract -> "-" | Multiply -> "*" | Divide -> "/" | Remainder -> "%"
  | BitAnd -> "&" | BitOr -> "|" | Xor -> "^" | ShiftLeft -> "<<" | ShiftRight -> ">>"
  | And -> "&&" | Or -> "||" | Equal -> "==" | NotEqual -> "!="
  | LessThan -> "<" | LessOrEqual -> "<=" | GreaterThan -> ">" | GreaterOrEqual -> ">="

let rec pp_exp = function
  | Constant i -> sprintf "Constant(%d)" i
  | Var name -> sprintf "Var(%s)" name
  | Unary (op, e) -> sprintf "Unary(%s, %s)" (pp_unary_op op) (pp_exp e)
  | Binary (op, e1, e2) -> sprintf "Binary(%s, %s, %s)" (pp_binary_op op) (pp_exp e1) (pp_exp e2)
  | Assignment (e1, e2) -> sprintf "Assignment(%s, %s)" (pp_exp e1) (pp_exp e2)
  | CompoundAssignment (op, e1, e2) -> sprintf "CompoundAssignment(%s, %s, %s)" (pp_binary_op op) (pp_exp e1) (pp_exp e2)
  | PrefixIncrement e -> sprintf "PrefixIncrement(%s)" (pp_exp e)
  | PostfixIncrement e -> sprintf "PostfixIncrement(%s)" (pp_exp e)
  | PrefixDecrement e -> sprintf "PrefixDecrement(%s)" (pp_exp e)
  | PostfixDecrement e -> sprintf "PostfixDecrement(%s)" (pp_exp e)
  | Conditional (c, t, f) -> sprintf "Conditional(%s, %s, %s)" (pp_exp c) (pp_exp t) (pp_exp f)
  | FunctionCall (name, args) -> sprintf "Call(%s, [%s])" name (String.concat ", " (List.map pp_exp args))

let pp_declaration (Declaration (name, init)) =
  match init with Some e -> sprintf "Decl(%s, %s)" name (pp_exp e) | None -> sprintf "Decl(%s)" name

let pp_for_init = function InitDecl d -> pp_declaration d | InitExp (Some e) -> pp_exp e | InitExp None -> "None"
let pp_opt_exp = function Some e -> pp_exp e | None -> "None"
let pp_label = function Some l -> l | None -> "None"

let rec pp_statement = function
  | Return e -> sprintf "Return(%s)" (pp_exp e)
  | Expression e -> sprintf "Expression(%s)" (pp_exp e)
  | If (c, t, e) -> sprintf "If(%s, %s, %s)" (pp_exp c) (pp_statement t) (match e with Some s -> pp_statement s | None -> "Null")
  | Compound (Block items) -> sprintf "Compound([%s])" (String.concat ", " (List.map pp_block_item items))
  | Goto l -> sprintf "Goto(%s)" l
  | Label (l, s) -> sprintf "Label(%s, %s)" l (pp_statement s)
  | While (c, b, l) -> sprintf "While(%s, %s, %s)" (pp_exp c) (pp_statement b) (pp_label l)
  | DoWhile (b, c, l) -> sprintf "DoWhile(%s, %s, %s)" (pp_statement b) (pp_exp c) (pp_label l)
  | For (i, c, p, b, l) -> sprintf "For(%s, %s, %s, %s, %s)" (pp_for_init i) (pp_opt_exp c) (pp_opt_exp p) (pp_statement b) (pp_label l)
  | Break l -> sprintf "Break(%s)" (pp_label l)
  | Continue l -> sprintf "Continue(%s)" (pp_label l)
  | Switch (c, b, l, _) -> sprintf "Switch(%s, %s, %s)" (pp_exp c) (pp_statement b) (pp_label l)
  | Case (e, s, l) -> sprintf "Case(%s, %s, %s)" (pp_exp e) (pp_statement s) (pp_label l)
  | Default (s, l) -> sprintf "Default(%s, %s)" (pp_statement s) (pp_label l)
  | Null -> "Null"

and pp_block_item = function S s -> pp_statement s | D d -> pp_declaration d

let pp_function (Function { name; params; body }) =
  let params_str = String.concat ", " params in
  match body with
  | Some (Block items) -> sprintf "Function(%s, [%s], [%s])" name params_str (String.concat "; " (List.map pp_block_item items))
  | None -> sprintf "Declare(%s, [%s])" name params_str

let pp_program (Program funs) = String.concat "\n" (List.map pp_function funs)
