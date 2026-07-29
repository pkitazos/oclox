type t =
  | Lit of lit
  | Unary of unary
  | Grouping of t
  | Binary of binary

and num =
  | Int of int
  | Float of float

and lit =
  | Num of num
  | Str of string
  | True
  | False
  | Nil

and unary =
  | Not of t
  | Minus of t

and binary =
  { op : operator
  ; left : t
  ; right : t
  }

and operator =
  | Eq
  | NEq
  | LT
  | LEq
  | GT
  | GEq
  | Add
  | Sub
  | Mul
  | Div

val to_string : t -> string
