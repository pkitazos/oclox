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

(* scanner internals *)

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

let err_at (ctx : ctx) msg = Error.make ~line:ctx.line ~column:ctx.column msg

let rec scan_string (ctx : ctx) (chars : char list) : (ctx * string, Error.t) result =
  match peek ctx with
  | Some '"' -> Ok (advance ctx, string_of_chars chars)
  | Some '\n' -> scan_string (advance_line ctx) ('\n' :: chars)
  | Some '\\' ->
    (match peek (advance ctx) with
     | Some 'n' -> scan_string (advance ~n:2 ctx) ('\n' :: chars)
     | Some 't' -> scan_string (advance ~n:2 ctx) ('\t' :: chars)
     | Some 'r' -> scan_string (advance ~n:2 ctx) ('\r' :: chars)
     | Some '"' -> scan_string (advance ~n:2 ctx) ('"' :: chars)
     | Some '\\' -> scan_string (advance ~n:2 ctx) ('\\' :: chars)
     | Some c ->
       (* js semantics where slash is ignored and next character survives *)
       scan_string (advance ~n:2 ctx) (c :: chars)
     | _ -> scan_string (advance ctx) ('\\' :: chars))
  | Some char -> scan_string (advance ctx) (char :: chars)
  | None -> Error (err_at ctx "unterminated string")
;;

let rec scan_number (ctx : ctx) (chars : char list) (in_float : bool)
  : (ctx * num, Error.t) result
  =
  match peek ctx with
  | Some '.' when not in_float -> scan_number (advance ctx) ('.' :: chars) true
  (* the book allows this to pass the scanner at least *)
  | Some '.' -> Error (err_at ctx "already in a float")
  | Some c when is_digit c -> scan_number (advance ctx) (c :: chars) in_float
  (* same here *)
  | Some c when is_alpha c -> Error (err_at ctx "not a valid number")
  | _ -> Ok (ctx, num_of_chars chars in_float)
;;

let rec scan_identifier (ctx : ctx) (chars : char list) : ctx * string =
  match peek ctx with
  | Some c when is_alpha c || is_digit c -> scan_identifier (advance ctx) (c :: chars)
  | _ -> ctx, string_of_chars chars
;;

type step =
  | Emit of Token.t * ctx
  | Skip of ctx

let rec consume_comment (ctx : ctx) : ctx =
  match peek ctx with
  | None -> ctx
  | Some '\n' -> advance_line ctx
  | Some _ -> consume_comment (advance ctx)
;;

let rec scan_loop (ctx : ctx) (tokens : Token.t list) : (Token.t list, Error.t) result =
  let lexeme_until idx' = String.sub ctx.source ctx.idx (idx' - ctx.idx) in
  let emit_tok ctx' ?lit typ : step =
    (* Tokens carry the line where their lexeme *starts*.
     NB: deviation from book, where a multi-line string
     carries the line of its closing quote. *)
    let lexeme = lexeme_until ctx'.idx in
    let tok = Token.make ~typ ~lexeme ~literal:lit ~line:ctx.line in
    Emit (tok, ctx')
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
    let tok = Token.make ~typ:EOF ~lexeme:"" ~literal:None ~line:ctx.line in
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
         | Some ctx' -> skip (consume_comment ctx')
         | None -> emit SLASH)
      (* strings *)
      | '"' ->
        scan_string (advance ctx) []
        |> Result.map (fun (ctx', str) -> emit_tok ctx' STRING ~lit:(Str str))
      (* numbers *)
      | c when is_digit c ->
        scan_number ctx [] false
        |> Result.map (fun (ctx', num) -> emit_tok ctx' NUMBER ~lit:(Num num))
      (* keywords & identifiers *)
      | c when is_alpha c ->
        let ctx', id = scan_identifier ctx [] in
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
     | Ok (Emit (tok, ctx')) -> scan_loop ctx' (tok :: tokens)
     | Ok (Skip ctx') -> scan_loop ctx' tokens
     | Error err -> Error err)
;;

let scan_tokens source = scan_loop { source; line = 1; column = 1; idx = 0 } []
