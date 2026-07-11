type num =
  | Int of int
  | Float of float
[@@deriving show]

type lit =
  | Num of num
  | Str of string
[@@deriving show]

type token_type =
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

type t =
  { typ : token_type
  ; lexeme : string
  ; literal : lit option
  ; line : int
  }

let show_token token =
  print_endline
    (show_token_type token.typ
     ^ " "
     ^ token.lexeme
     ^ " "
     ^
     match token.literal with
     | None -> ""
     | Some lit -> show_lit lit)
;;
