let ( let* ) = Result.bind
let err_at line column msg = Error.make ~kind:Parse ~line ~column msg

let peek (tokens : Token.t list) =
  match tokens with
  | tok :: rest -> Some (tok, rest)
  | [] -> None
;;

let equality_ops = [ Token.EQUAL_EQUAL, Ast.Eq; Token.BANG_EQUAL, Ast.NEq ]

let comparison_ops =
  [ Token.LESS, Ast.LT
  ; Token.LESS_EQUAL, Ast.LEq
  ; Token.GREATER, Ast.GT
  ; Token.GREATER_EQUAL, Ast.GEq
  ]
;;

let term_ops = [ Token.PLUS, Ast.Add; Token.MINUS, Ast.Sub ]
let factor_ops = [ Token.SLASH, Ast.Div; Token.STAR, Ast.Mul ]

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

let rec parse tokens : (Ast.t, Error.t) result =
  let* expr, rest = parse_equality tokens in
  match rest with
  | [] -> Ok expr
  | _ -> Error (err_at 1 1 "still tokens left?")

and fold_left_if tokens acc parser' assoc_opt =
  let parser tokens acc = fold_left_if tokens acc parser' assoc_opt in
  match peek tokens with
  | Some ({ typ }, rest) ->
    (match List.assoc_opt typ assoc_opt with
     | Some op ->
       let* expr, rest = parser' rest in
       parser rest (Ast.Binary { left = acc; op; right = expr })
     | _ -> Ok (acc, tokens))
  | _ -> Ok (acc, tokens)

and parse_equality tokens : (Ast.t * Token.t list, Error.t) result =
  let* expr, rest = parse_comparison tokens in
  fold_left_if rest expr parse_comparison equality_ops

and parse_comparison tokens : (Ast.t * Token.t list, Error.t) result =
  let* expr, rest = parse_term tokens in
  fold_left_if rest expr parse_term comparison_ops

and parse_term tokens : (Ast.t * Token.t list, Error.t) result =
  let* expr, rest = parse_factor tokens in
  fold_left_if rest expr parse_factor term_ops

and parse_factor tokens : (Ast.t * Token.t list, Error.t) result =
  let* expr, rest = parse_unary tokens in
  fold_left_if rest expr parse_unary factor_ops

and parse_unary tokens : (Ast.t * Token.t list, Error.t) result =
  match peek tokens with
  | Some ({ typ = Token.MINUS }, rest) ->
    let* expr, rest = parse_unary rest in
    Ok (Ast.Unary (Ast.Minus expr), rest)
  | Some ({ typ = Token.BANG }, rest) ->
    let* expr, rest = parse_unary rest in
    Ok (Ast.Unary (Ast.Not expr), rest)
  | _ -> parse_primary tokens

and parse_primary tokens : (Ast.t * Token.t list, Error.t) result =
  match peek tokens with
  | Some ({ typ = Token.TRUE }, rest) -> Ok (Ast.Lit Ast.True, rest)
  | Some ({ typ = Token.FALSE }, rest) -> Ok (Ast.Lit Ast.False, rest)
  | Some ({ typ = Token.NIL }, rest) -> Ok (Ast.Lit Ast.Nil, rest)
  | Some ({ typ; literal = Some lit; _ }, rest)
    when List.mem typ [ Token.STRING; Token.NUMBER ] ->
    let expr =
      match lit with
      | Token.Num (Token.Int n) -> Ast.Num (Ast.Int n)
      | Token.Num (Token.Float n) -> Ast.Num (Ast.Float n)
      | Token.Str str -> Ast.Str str
    in
    Ok (Ast.Lit expr, rest)
  | Some (tok, _) -> Error (err_at tok.line tok.line "not the expected token")
  | None -> Error (err_at 1 1 "out of tokens?")
;;
