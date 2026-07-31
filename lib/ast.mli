type num =
  | Int of int
  | Float of float

type lit =
  | Num of num
  | Str of string
  | Bool of bool
  | Nil

type unop =
  | Neg
  | Not

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

type t =
  | Lit of lit
  | Grouping of t
  | Unary of unop * t
  | Binary of
      { op : binop
      ; left : t
      ; right : t
      }

val to_string : t -> string
