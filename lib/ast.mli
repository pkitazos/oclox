type t =
  | Lit of lit
  | Unary of unary
  | Grouping of grouping
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

and grouping = Expr of t

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
  | Plus
  | Minus
  | Times
  | Div

val to_string : t -> string
