open Token
module StrMap = Map.Make (String)

let ( let* ) = Result.bind

let keywords =
  StrMap.of_seq
  @@ List.to_seq
       [ "and", AND
       ; "class", CLASS
       ; "else", ELSE
       ; "false", FALSE
       ; "for", FOR
       ; "fun", FUN
       ; "if", IF
       ; "nil", NIL
       ; "or", OR
       ; "print", PRINT
       ; "return", RETURN
       ; "super", SUPER
       ; "this", THIS
       ; "true", TRUE
       ; "var", VAR
       ; "while", WHILE
       ]
;;

(* some functional utils *)

let string_of_chars chars = List.rev chars |> List.to_seq |> String.of_seq

let num_of_chars chars is_float =
  let s = string_of_chars chars in
  if is_float then Float (float_of_string s) else Int (int_of_string s)
;;

(* char utils *)

let is_alpha (c : char) : bool =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
;;

let is_digit (c : char) : bool = c >= '0' && c <= '9'

(* lexer internals *)

type ctx =
  { source : string
  ; idx : int
  ; line : int
  ; column : int
  }

let peek (ctx : ctx) : char option =
  let str, i = ctx.source, ctx.idx in
  if i >= 0 && i < String.length str then Some str.[i] else None
;;

(* advance [n] characters within the current line *)
let advance ?(n = 1) (ctx : ctx) : ctx =
  { ctx with idx = ctx.idx + n; column = ctx.column + n }
;;

(* advance past a newline *)
let advance_line (ctx : ctx) : ctx =
  { ctx with idx = ctx.idx + 1; column = 1; line = ctx.line + 1 }
;;

let matches (ctx : ctx) (c : char) : ctx option =
  if peek ctx = Some c then Some (advance ctx) else None
;;

let matches_both (ctx : ctx) (c1 : char) (c2 : char) : ctx option =
  if peek ctx = Some c1 && peek (advance ctx) = Some c2
  then Some (advance ~n:2 ctx)
  else None
;;

let err_at (ctx : ctx) msg = Error.make ~kind:Lex ~line:ctx.line ~column:ctx.column msg

let lex_string ctx : (ctx * Token.lit, Error.t) result =
  let rec lex_string_loop ctx chars =
    match peek ctx with
    | Some '"' -> Ok (advance ctx, string_of_chars chars)
    | Some '\n' -> lex_string_loop (advance_line ctx) ('\n' :: chars)
    | Some '\\' ->
      (match peek (advance ctx) with
       | Some 'n' -> lex_string_loop (advance ~n:2 ctx) ('\n' :: chars)
       | Some 't' -> lex_string_loop (advance ~n:2 ctx) ('\t' :: chars)
       | Some 'r' -> lex_string_loop (advance ~n:2 ctx) ('\r' :: chars)
       | Some '"' -> lex_string_loop (advance ~n:2 ctx) ('"' :: chars)
       | Some '\\' -> lex_string_loop (advance ~n:2 ctx) ('\\' :: chars)
       | Some c ->
         (* js semantics where slash is ignored and next character survives *)
         lex_string_loop (advance ~n:2 ctx) (c :: chars)
       | _ -> lex_string_loop (advance ctx) ('\\' :: chars))
    | Some char -> lex_string_loop (advance ctx) (char :: chars)
    | None -> Error (err_at ctx "unterminated string")
  in
  let* ctx, str = lex_string_loop ctx [] in
  Ok (ctx, Str str)
;;

let lex_number (ctx : ctx) : (ctx * Token.lit, Error.t) result =
  let rec lex_number_loop ctx chars in_float =
    match peek ctx with
    | Some '.' when not in_float -> lex_number_loop (advance ctx) ('.' :: chars) true
    (* the book allows this to pass the lexer at least *)
    | Some '.' -> Error (err_at ctx "already in a float")
    | Some c when is_digit c -> lex_number_loop (advance ctx) (c :: chars) in_float
    (* same here *)
    | Some c when is_alpha c -> Error (err_at ctx "not a valid number")
    | _ -> Ok (ctx, num_of_chars chars in_float)
  in
  let* ctx, num = lex_number_loop ctx [] false in
  Ok (ctx, Num num)
;;

let lex_identifier ctx : ctx * string =
  let rec lex_identifier_loop ctx chars =
    match peek ctx with
    | Some c when is_alpha c || is_digit c ->
      lex_identifier_loop (advance ctx) (c :: chars)
    | _ -> ctx, string_of_chars chars
  in
  lex_identifier_loop ctx []
;;

type step =
  | Emit of Token.t * ctx
  | Skip of ctx

let lex_line_comment ctx =
  let rec lex_line_comment_loop (ctx : ctx) (chars : char list)
    : (ctx * string, Error.t) result
    =
    match peek ctx with
    | Some '\n' -> Ok (ctx, string_of_chars chars)
    | Some char -> lex_line_comment_loop (advance ctx) (char :: chars)
    | None -> Ok (ctx, string_of_chars chars)
  in
  lex_line_comment_loop ctx []
;;

let lex_block_comment ctx =
  let rec lex_block_comment_loop (ctx : ctx) (chars : char list) (depth : int)
    : (ctx * string, Error.t) result
    =
    match matches_both ctx '*' '/' with
    | Some ctx' when depth = 0 -> Ok (ctx', string_of_chars chars)
    | Some ctx' -> lex_block_comment_loop ctx' ('/' :: '*' :: chars) (depth - 1)
    | None ->
      (match matches_both ctx '/' '*' with
       | Some ctx' -> lex_block_comment_loop ctx' ('*' :: '/' :: chars) (depth + 1)
       | None ->
         (match peek ctx with
          | Some '\n' -> lex_block_comment_loop (advance_line ctx) ('\n' :: chars) depth
          | Some char -> lex_block_comment_loop (advance ctx) (char :: chars) depth
          | None -> Error (err_at ctx "unterminated multiline comment")))
  in
  lex_block_comment_loop ctx [] 0
;;

let rec lex_loop (ctx : ctx) (tokens : Token.t list) ~emit_trivia
  : (Token.t list, Error.t) result
  =
  let mk_tok ctx ~typ ~lexeme ~literal =
    Token.make ~typ ~lexeme ~literal ~line:ctx.line ~column:ctx.column
  in
  let emit_tok ctx' ?lit typ : step =
    (* Tokens carry the line where their lexeme *starts*.
     NB: deviation from book, where a multi-line string
     carries the line of its closing quote. *)
    let tok =
      mk_tok
        ctx
        ~typ
        ~lexeme:(String.sub ctx.source ctx.idx (ctx'.idx - ctx.idx))
        ~literal:lit
    in
    Emit (tok, ctx')
  in
  let emit_lit lexer typ ctx' =
    lexer ctx' |> Result.map (fun (ctx'', lit) -> emit_tok ctx'' typ ~lit)
  in
  let emit typ = Ok (emit_tok (advance ctx) typ) in
  let skip ctx' = Ok (Skip ctx') in
  let emit_or_skip res typ =
    match res with
    | Ok (ctx, str) when emit_trivia -> Ok (emit_tok ctx typ ~lit:(Str str))
    | Ok (ctx, _) -> skip ctx
    | Error e -> Error e
  in
  let emit_op c ~default ~matched =
    match matches (advance ctx) c with
    | Some ctx' -> Ok (emit_tok ctx' matched)
    | None -> Ok (emit_tok (advance ctx) default)
  in
  let typ_of_identifier id =
    StrMap.find_opt id keywords |> Option.value ~default:IDENTIFIER
  in
  match peek ctx with
  | None -> Ok (List.rev (mk_tok ctx ~typ:EOF ~lexeme:"" ~literal:None :: tokens))
  | Some c ->
    let res =
      match c with
      (* whitespace & friends *)
      | ' ' | '\r' | '\t' -> skip (advance ctx)
      | '\n' -> skip (advance_line ctx)
      (* single char symbols *)
      | '(' -> emit LEFT_PAREN
      | ')' -> emit RIGHT_PAREN
      | '{' -> emit LEFT_BRACE
      | '}' -> emit RIGHT_BRACE
      | ',' -> emit COMMA
      | '.' -> emit DOT
      | '-' -> emit MINUS
      | '+' -> emit PLUS
      | ';' -> emit SEMICOLON
      | '*' -> emit STAR
      (* maybe two-char operators *)
      | '!' -> emit_op '=' ~default:BANG ~matched:BANG_EQUAL
      | '=' -> emit_op '=' ~default:EQUAL ~matched:EQUAL_EQUAL
      | '<' -> emit_op '=' ~default:LESS ~matched:LESS_EQUAL
      | '>' -> emit_op '=' ~default:GREATER ~matched:GREATER_EQUAL
      (* maybe comments *)
      | '/' ->
        (match matches_both ctx '/' '/' with
         | Some ctx' -> emit_or_skip (lex_line_comment ctx') LINE_COMMENT
         | None ->
           (match matches_both ctx '/' '*' with
            | Some ctx' -> emit_or_skip (lex_block_comment ctx') BLOCK_COMMENT
            | None -> emit SLASH))
      (* strings *)
      | '"' -> emit_lit lex_string STRING (advance ctx)
      (* numbers *)
      | c when is_digit c -> emit_lit lex_number NUMBER ctx
      (* keywords & identifiers *)
      | c when is_alpha c ->
        let ctx', id = lex_identifier ctx in
        Ok (emit_tok ctx' (typ_of_identifier id))
      (* otherwise *)
      | c -> Error (err_at ctx ("unexpected character: " ^ String.make 1 c))
    in
    Result.bind res (function
      | Emit (tok, ctx') -> lex_loop ctx' (tok :: tokens) ~emit_trivia
      | Skip ctx' -> lex_loop ctx' tokens ~emit_trivia)
;;

let lex source ~emit_trivia =
  lex_loop { source; line = 1; column = 1; idx = 0 } [] ~emit_trivia
;;
