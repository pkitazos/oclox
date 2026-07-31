let ( let* ) = Result.bind

let err_at (tok : Token.t) msg =
  Error.make ~kind:Parse ~line:tok.line ~column:tok.column msg
;;

let peek (tokens : Token.t list) =
  match tokens with
  | tok :: rest -> Some (tok, rest)
  | [] -> None
;;

let expect typ msg tokens =
  match peek tokens with
  | Some (tok, rest) when tok.typ = typ -> Ok rest
  | Some (tok, _) -> Error (err_at tok msg)
  (* Invariant: token lists always end with a single EOF token
     this is guaranteed by [Scanner.scan_tokens]
     Rules may therefore assume [peek] never returns None on well-formed input
     a None means the invariant was broken. *)
  | None -> failwith "unreachable: token list must end with EOF"
;;

let ast_lit_of_token_lit lit =
  match lit with
  | Token.Num (Token.Int n) -> Ast.Num (Ast.Int n)
  | Token.Num (Token.Float n) -> Ast.Num (Ast.Float n)
  | Token.Str str -> Ast.Str str
;;

let equality_ops = [ Token.EQUAL_EQUAL, Ast.Eq; Token.BANG_EQUAL, Ast.Ne ]

let comparison_ops =
  [ Token.LESS, Ast.Lt
  ; Token.LESS_EQUAL, Ast.Le
  ; Token.GREATER, Ast.Gt
  ; Token.GREATER_EQUAL, Ast.Ge
  ]
;;

let term_ops = [ Token.PLUS, Ast.Add; Token.MINUS, Ast.Sub ]
let factor_ops = [ Token.SLASH, Ast.Div; Token.STAR, Ast.Mul ]
let unary_ops = [ Token.BANG, Ast.Not; Token.MINUS, Ast.Neg ]

(* peek the next token and, if its type is in [ops],
   return the mapped value along with the remaining tokens *)
let peek_op ops tokens =
  match peek tokens with
  | Some (tok, rest) -> Option.map (fun op -> op, rest) (List.assoc_opt tok.typ ops)
  | None -> None
;;

(* [left_assoc ~ops ~next] parses  `next ( op next )*`  folding LEFT, so
   1 - 2 - 3 becomes ((1 - 2) - 3).
   Right-associative operators would need a different combinator. *)

let left_assoc ~ops ~next tokens =
  let rec fold tokens acc =
    let parser tokens acc = fold tokens acc in
    match peek_op ops tokens with
    | Some (op, rest) ->
      let* expr, rest = next rest in
      parser rest (Ast.Binary { left = acc; op; right = expr })
    | None -> Ok (acc, tokens)
  in
  let* expr, rest = next tokens in
  fold rest expr
;;

(* Each rule consumes a prefix of [tokens]
   and returns its AST node plus the unconsumed remainder.

   Rules never consume tokens they don't recognize. *)

type parsed = (Ast.t * Token.t list, Error.t) result

(*
    expression     → equality ;
    equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    term           → factor ( ( "-" | "+" ) factor )* ;
    factor         → unary ( ( "/" | "*" ) unary )* ;
    unary          → ( "!" | "-" ) unary
                   | primary ;
    primary        → NUMBER | STRING | "true" | "false" | "nil"
                   | "(" expression ")" ;
*)

let rec parse_expression tokens : parsed = parse_equality tokens

and parse_equality tokens : parsed =
  left_assoc ~ops:equality_ops ~next:parse_comparison tokens

and parse_comparison tokens : parsed =
  left_assoc ~ops:comparison_ops ~next:parse_term tokens

and parse_term tokens : parsed = left_assoc ~ops:term_ops ~next:parse_factor tokens
and parse_factor tokens : parsed = left_assoc ~ops:factor_ops ~next:parse_unary tokens

and parse_unary tokens =
  match peek_op unary_ops tokens with
  | Some (op, rest) ->
    let* expr, rest = parse_unary rest in
    Ok (Ast.Unary (op, expr), rest)
  | None -> parse_primary tokens

and parse_primary tokens : parsed =
  match peek tokens with
  | Some ({ typ = Token.TRUE; _ }, rest) -> Ok (Ast.Lit (Ast.Bool true), rest)
  | Some ({ typ = Token.FALSE; _ }, rest) -> Ok (Ast.Lit (Ast.Bool false), rest)
  | Some ({ typ = Token.NIL; _ }, rest) -> Ok (Ast.Lit Ast.Nil, rest)
  | Some ({ typ; literal = Some lit; _ }, rest)
    when List.mem typ [ Token.STRING; Token.NUMBER ] ->
    Ok (Ast.Lit (ast_lit_of_token_lit lit), rest)
  | Some ({ typ = Token.LEFT_PAREN; _ }, rest) ->
    let* expr, rest = parse_expression rest in
    let* rest = expect Token.RIGHT_PAREN "expected ')' after expression" rest in
    Ok (Ast.Grouping expr, rest)
  | Some (tok, _) -> Error (err_at tok "not the expected token")
  | None -> failwith "unreachable: token list must end with EOF"
;;

let parse tokens : (Ast.t, Error.t) result =
  let* expr, rest = parse_expression tokens in
  match peek rest with
  | Some ({ typ = Token.EOF; _ }, []) -> Ok expr
  | Some (tok, _) -> Error (err_at tok "unexpected token after expression")
  | None -> failwith "unreachable: token list must end with EOF"
;;
