let ( let* ) = Result.bind

exception Invalid_token_stream

let err_at (tok : Token.t) msg =
  Error.make ~kind:Parse ~line:tok.line ~column:tok.column msg
;;

(* [Lexer.lex] always emits a trailing [EOF]
   so a well-formed token list is never empty.
   peek relies on that invariant and is therefore total
   an empty list means the invariant was broken upstream, which is a bug. *)
let peek : Token.t list -> Token.t * Token.t list = function
  | tok :: rest -> tok, rest
  | [] -> raise Invalid_token_stream
;;

let is_stmt_start = function
  | Token.CLASS
  | Token.FUN
  | Token.VAR
  | Token.FOR
  | Token.IF
  | Token.WHILE
  | Token.PRINT
  | Token.RETURN -> true
  | _ -> false
;;

let synchronize tokens =
  (* Skip tokens until just past a ';' or just before a statement keyword,
     so the parser can resume at a plausible boundary.
     Always discards at least one token, which is what guarantees the caller's recovery loop
     terminates. The book does this before the loop, since we don't have a `prev` check we do it after. *)
  let rec loop tokens =
    match peek tokens with
    | { typ = Token.EOF; _ }, _ -> tokens
    | { typ = Token.SEMICOLON; _ }, rest -> rest
    | { typ; _ }, _ when is_stmt_start typ -> tokens
    | _, rest -> loop rest
  in
  let _, rest = peek tokens in
  loop rest
;;

let expect typ msg tokens =
  match peek tokens with
  | tok, rest when tok.typ = typ -> Ok rest
  | tok, _ -> Error (err_at tok msg)
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
  let tok, rest = peek tokens in
  Option.map (fun op -> op, rest) (List.assoc_opt tok.typ ops)
;;

(* [left_assoc ~ops ~next] parses  `next ( op next )*`  folding LEFT, so
   1 - 2 - 3 becomes ((1 - 2) - 3).
   Right-associative operators would need a different combinator. *)

let left_assoc
      ~(ops : (Token.token_typ * 'op) list)
      ~(mk : 'op -> Ast.t -> Ast.t -> Ast.t)
      ~next
      tokens
  =
  let rec fold tokens acc =
    match peek_op ops tokens with
    | Some (op, rest) ->
      let* expr, rest = next rest in
      fold rest (mk op acc expr)
    | None -> Ok (acc, tokens)
  in
  let* expr, rest = next tokens in
  fold rest expr
;;

let binary op left right = Ast.Binary { op; left; right }

let is_binary_op typ =
  List.exists (List.mem_assoc typ) [ equality_ops; comparison_ops; term_ops; factor_ops ]
;;

(* Each rule consumes a prefix of [tokens]
   and returns its AST node plus the unconsumed remainder.

   Rules never consume tokens they don't recognize. *)

type parsed = (Ast.t * Token.t list, Error.t) result

(*
    expression    -> comma ;
    comma         -> ternary ( "," ternary )* ;
    ternary       -> equality ( "?" expression ":" ternary )? ;
    equality      -> comparison ( ( "!=" | "==" ) comparison )* ;
    comparison    -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    term          -> factor ( ( "-" | "+" ) factor )* ;
    factor        -> unary ( ( "/" | "*" ) unary )* ;
    unary         -> ( "!" | "-" ) unary
                  | primary ;
    primary       -> NUMBER | STRING | "true" | "false" | "nil"
                  | "(" expression ")" ;
*)

let rec parse_expression tokens : parsed = parse_comma tokens

and parse_comma tokens =
  left_assoc
    ~ops:[ Token.COMMA, () ]
    ~mk:(fun () left right -> Ast.Sequence { left; right })
    ~next:parse_ternary
    tokens

and parse_ternary tokens : parsed =
  let* cond, rest = parse_equality tokens in
  match peek rest with
  | { typ = Token.QUESTION; _ }, rest ->
    (* delimited by '?' and ':', so any expression fits *)
    let* then_, rest = parse_expression rest in
    let* rest = expect Token.COLON "expected ':' in ternary" rest in
    (* and here is the right-associativity *)
    let* else_, rest = parse_ternary rest in
    Ok (Ast.Ternary { cond; then_; else_ }, rest)
  | _ -> Ok (cond, rest)

and parse_equality tokens : parsed =
  left_assoc ~ops:equality_ops ~mk:binary ~next:parse_comparison tokens

and parse_comparison tokens : parsed =
  left_assoc ~ops:comparison_ops ~mk:binary ~next:parse_term tokens

and parse_term tokens : parsed =
  left_assoc ~ops:term_ops ~mk:binary ~next:parse_factor tokens

and parse_factor tokens : parsed =
  left_assoc ~ops:factor_ops ~mk:binary ~next:parse_unary tokens

and parse_unary tokens =
  match peek_op unary_ops tokens with
  | Some (op, rest) ->
    let* expr, rest = parse_unary rest in
    Ok (Ast.Unary (op, expr), rest)
  | None -> parse_primary tokens

and parse_primary tokens : parsed =
  match peek tokens with
  | { typ = Token.TRUE; _ }, rest -> Ok (Ast.Lit (Ast.Bool true), rest)
  | { typ = Token.FALSE; _ }, rest -> Ok (Ast.Lit (Ast.Bool false), rest)
  | { typ = Token.NIL; _ }, rest -> Ok (Ast.Lit Ast.Nil, rest)
  | { typ; literal = Some lit; _ }, rest when List.mem typ [ Token.STRING; Token.NUMBER ]
    -> Ok (Ast.Lit (ast_lit_of_token_lit lit), rest)
  | { typ = Token.LEFT_PAREN; _ }, rest ->
    let* expr, rest = parse_expression rest in
    let* rest = expect Token.RIGHT_PAREN "expected ')' after expression" rest in
    Ok (Ast.Grouping expr, rest)
  | tok, _ when is_binary_op tok.typ ->
    Error (err_at tok (Printf.sprintf "missing left-hand operand for '%s'" tok.lexeme))
  | tok, _ -> Error (err_at tok "not the expected token")
;;

let is_trivia (tok : Token.t) =
  match tok.typ with
  | Token.LINE_COMMENT | Token.BLOCK_COMMENT -> true
  | _ -> false
;;

(* Trivia is only emitted by the lexer *)
let parse tokens : (Ast.t, Error.t) result =
  let tokens = List.filter (fun tok -> not (is_trivia tok)) tokens in
  let* expr, rest = parse_expression tokens in
  match peek rest with
  | { typ = Token.EOF; _ }, [] -> Ok expr
  | tok, _ -> Error (err_at tok "unexpected token after expression")
;;
