type num =
  | Int of int
  | Float of float
[@@deriving show]

type lit =
  | Num of num
  | Str of string
  | Bool of bool
  | Nil
[@@deriving show]

type unop =
  | Neg
  | Not
[@@deriving show]

type binop =
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | Add
  | Sub
  | Mul
  | Div
[@@deriving show]

type t =
  | Lit of lit
  | Grouping of t
  | Unary of unop * t
  | Binary of
      { op : binop
      ; left : t
      ; right : t
      }
    (* the comma operator: a, b *)
  | Sequence of
      { left : t
      ; right : t
      }
  | Ternary of
      { cond : t
      ; then_ : t
      ; else_ : t
      }
[@@deriving show]

let rec to_string = function
  | Lit l -> lit_to_string l
  | Grouping e -> Printf.sprintf "(group %s)" (to_string e)
  | Unary (Not, e) -> Printf.sprintf "(! %s)" (to_string e)
  | Unary (Neg, e) -> Printf.sprintf "(- %s)" (to_string e)
  | Binary { op; left; right } ->
    Printf.sprintf "(%s %s %s)" (op_to_string op) (to_string left) (to_string right)
  | Sequence { left; right } ->
    Printf.sprintf "(, %s %s)" (to_string left) (to_string right)
  | Ternary { cond; then_; else_ } ->
    Printf.sprintf
      "(? %s %s %s)"
      (to_string cond)
      (to_string then_)
      (to_string else_)

and lit_to_string expr =
  match expr with
  | Num (Int n) -> string_of_int n
  | Num (Float n) -> string_of_float n
  | Bool true -> "true"
  | Bool false -> "false"
  | Nil -> "nil"
  | Str s -> s

and op_to_string expr =
  match expr with
  | Eq -> "=="
  | Ne -> "!="
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
;;
