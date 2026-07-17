type t =
  | Lit of lit
  | Unary of unary
  | Grouping of grouping
  | Binary of binary
[@@deriving show]

and num =
  | Int of int
  | Float of float
[@@deriving show]

and lit =
  | Num of num
  | Str of string
  | True
  | False
  | Nil
[@@deriving show]

and grouping = Expr of t [@@deriving show]

and unary =
  | Not of t
  | Minus of t
[@@deriving show]

and binary =
  { op : operator
  ; left : t
  ; right : t
  }
[@@deriving show]

and operator =
  | Eq
  | NEq
  | LT
  | LEq
  | GT
  | GEq
  | Plus
  | Minus
  | Times
  | Div
[@@deriving show]

let rec to_string = function
  | Lit l -> lit_to_string l
  | Grouping (Expr e) -> Printf.sprintf "(group %s)" (to_string e)
  | Unary (Not e) -> Printf.sprintf "(! %s)" (to_string e)
  | Unary (Minus e) -> Printf.sprintf "(- %s)" (to_string e)
  | Binary { op; left; right } ->
    Printf.sprintf "(%s %s %s)" (op_to_string op) (to_string left) (to_string right)

and lit_to_string expr =
  match expr with
  | Num (Int n) -> string_of_int n
  | Num (Float n) -> string_of_float n
  | True -> "true"
  | False -> "false"
  | Nil -> "nil"
  | Str s -> s

and op_to_string expr =
  match expr with
  | Eq -> "=="
  | NEq -> "!="
  | LT -> "<"
  | LEq -> "<="
  | GT -> ">"
  | GEq -> ">="
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
;;
