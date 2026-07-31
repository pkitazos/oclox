type t =
  { typ : token_typ
  ; lexeme : string
  ; literal : lit option
  ; line : int
  ; column : int
  }

and num =
  | Int of int
  | Float of float
[@@deriving show]

and lit =
  | Num of num
  | Str of string
[@@deriving show]

and token_typ =
  | (* Single-character tokens. *)
    LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  | (* One or two character tokens. *)
    BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  | (* Literals. *)
    IDENTIFIER
  | STRING
  | NUMBER
  | (* Keywords. *)
    AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | EOF
[@@deriving show]

let make ~typ ~lexeme ~literal ~line ~column = { typ; lexeme; literal; line; column }

let to_string token =
  Printf.sprintf
    "%s %s %s"
    (show_token_typ token.typ)
    token.lexeme
    (match token.literal with
     | None -> ""
     | Some lit -> show_lit lit)
;;
