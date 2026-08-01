open Token
module StrMap = Map.Make (String)

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

let rec lex_string (ctx : ctx) (chars : char list) : (ctx * string, Error.t) result =
  match peek ctx with
  | Some '"' -> Ok (advance ctx, string_of_chars chars)
  | Some '\n' -> lex_string (advance_line ctx) ('\n' :: chars)
  | Some '\\' ->
    (match peek (advance ctx) with
     | Some 'n' -> lex_string (advance ~n:2 ctx) ('\n' :: chars)
     | Some 't' -> lex_string (advance ~n:2 ctx) ('\t' :: chars)
     | Some 'r' -> lex_string (advance ~n:2 ctx) ('\r' :: chars)
     | Some '"' -> lex_string (advance ~n:2 ctx) ('"' :: chars)
     | Some '\\' -> lex_string (advance ~n:2 ctx) ('\\' :: chars)
     | Some c ->
       (* js semantics where slash is ignored and next character survives *)
       lex_string (advance ~n:2 ctx) (c :: chars)
     | _ -> lex_string (advance ctx) ('\\' :: chars))
  | Some char -> lex_string (advance ctx) (char :: chars)
  | None -> Error (err_at ctx "unterminated string")
;;

let rec lex_number (ctx : ctx) (chars : char list) (in_float : bool)
  : (ctx * num, Error.t) result
  =
  match peek ctx with
  | Some '.' when not in_float -> lex_number (advance ctx) ('.' :: chars) true
  (* the book allows this to pass the lexer at least *)
  | Some '.' -> Error (err_at ctx "already in a float")
  | Some c when is_digit c -> lex_number (advance ctx) (c :: chars) in_float
  (* same here *)
  | Some c when is_alpha c -> Error (err_at ctx "not a valid number")
  | _ -> Ok (ctx, num_of_chars chars in_float)
;;

let rec lex_identifier (ctx : ctx) (chars : char list) : ctx * string =
  match peek ctx with
  | Some c when is_alpha c || is_digit c -> lex_identifier (advance ctx) (c :: chars)
  | _ -> ctx, string_of_chars chars
;;

type step =
  | Emit of Token.t * ctx
  | Skip of ctx

(* for lossy parser *)
let rec consume_comment (ctx : ctx) : ctx =
  match peek ctx with
  | None -> ctx
  | Some '\n' -> advance_line ctx
  | Some _ -> consume_comment (advance ctx)
;;

(* for lossless parser *)
let rec lex_comment (ctx : ctx) (chars : char list) : (ctx * string, Error.t) result =
  match peek ctx with
  | Some '\n' -> Ok (advance_line ctx, string_of_chars chars)
  | Some char -> lex_comment (advance ctx) (char :: chars)
  | None -> Ok (ctx, string_of_chars chars)
;;

let rec lex_multiline_comment (ctx : ctx) (chars : char list) (depth : int)
  : (ctx * string, Error.t) result
  =
  match matches_both ctx '*' '/' with
  | Some ctx' when depth = 0 -> Ok (ctx', string_of_chars chars)
  | Some ctx' -> lex_multiline_comment ctx' ('/' :: '*' :: chars) (depth - 1)
  | None ->
    (match matches_both ctx '/' '*' with
     | Some ctx' -> lex_multiline_comment ctx' ('*' :: '/' :: chars) (depth + 1)
     | None ->
       (match peek ctx with
        | Some '\n' -> lex_multiline_comment (advance_line ctx) ('\n' :: chars) depth
        | Some char -> lex_multiline_comment (advance ctx) (char :: chars) depth
        | None -> Error (err_at ctx "unterminated multiline comment")))
;;

let rec lex_loop (ctx : ctx) (tokens : Token.t list) : (Token.t list, Error.t) result =
  let lexeme_until idx' = String.sub ctx.source ctx.idx (idx' - ctx.idx) in
  let emit_tok ctx' ?lit typ : step =
    (* Tokens carry the line where their lexeme *starts*.
     NB: deviation from book, where a multi-line string
     carries the line of its closing quote. *)
    let lexeme = lexeme_until ctx'.idx in
    let tok = Token.make ~typ ~lexeme ~literal:lit ~line:ctx.line ~column:ctx.column in
    Emit (tok, ctx')
  in
  let emit_str lexer typ ctx' =
    lexer ctx' |> Result.map (fun (ctx'', s) -> emit_tok ctx'' typ ~lit:(Str s))
  in
  let emit typ = Ok (emit_tok (advance ctx) typ) in
  let skip ctx = Ok (Skip ctx) in
  let op2 c ~default ~matched =
    match matches (advance ctx) c with
    | Some ctx' -> Ok (emit_tok ctx' matched)
    | None -> Ok (emit_tok (advance ctx) default)
  in
  match peek ctx with
  | None ->
    let tok =
      Token.make ~typ:EOF ~lexeme:"" ~literal:None ~line:ctx.line ~column:ctx.column
    in
    Ok (List.rev (tok :: tokens))
  | Some c ->
    let res : (step, Error.t) result =
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
      | '!' -> op2 '=' ~default:BANG ~matched:BANG_EQUAL
      | '=' -> op2 '=' ~default:EQUAL ~matched:EQUAL_EQUAL
      | '<' -> op2 '=' ~default:LESS ~matched:LESS_EQUAL
      | '>' -> op2 '=' ~default:GREATER ~matched:GREATER_EQUAL
      (* maybe comments *)
      | '/' ->
        (match matches (advance ctx) '/' with
         | Some ctx' -> emit_str (fun c -> lex_comment c []) COMMENT ctx'
         | None ->
           (match matches (advance ctx) '*' with
            | Some ctx' ->
              emit_str (fun c -> lex_multiline_comment c [] 0) MULTILINE_COMMENT ctx'
            | None -> emit SLASH))
        (* strings *)
      | '"' -> emit_str (fun c -> lex_string c []) STRING (advance ctx)
      (* numbers *)
      | c when is_digit c ->
        lex_number ctx [] false
        |> Result.map (fun (ctx', num) -> emit_tok ctx' NUMBER ~lit:(Num num))
      (* keywords & identifiers *)
      | c when is_alpha c ->
        let ctx', id = lex_identifier ctx [] in
        let typ =
          match StrMap.find_opt id keywords with
          | Some keyword -> keyword
          | None -> IDENTIFIER
        in
        Ok (emit_tok ctx' typ)
      (* otherwise *)
      | c -> Error (err_at ctx ("unexpected character: " ^ String.make 1 c))
    in
    (match res with
     | Ok (Emit (tok, ctx')) -> lex_loop ctx' (tok :: tokens)
     | Ok (Skip ctx') -> lex_loop ctx' tokens
     | Error err -> Error err)
;;

let lex source = lex_loop { source; line = 1; column = 1; idx = 0 } []
