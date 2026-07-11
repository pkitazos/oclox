open Token
module StrMap = Map.Make (String)

let keywords =
  StrMap.of_seq
  @@ List.to_seq
       [
         ("and", AND);
         ("class", CLASS);
         ("else", ELSE);
         ("false", FALSE);
         ("for", FOR);
         ("fun", FUN);
         ("if", IF);
         ("nil", NIL);
         ("or", OR);
         ("print", PRINT);
         ("return", RETURN);
         ("super", SUPER);
         ("this", THIS);
         ("true", TRUE);
         ("var", VAR);
         ("while", WHILE);
       ]

(* some functional utils *)

let to_str = String.make 1

let chars_to_str (chars : char list) =
  List.map to_str chars |> List.rev |> String.concat ""

let maybe_get (str : string) (i : int) : char option =
  if i >= 0 && i < String.length str then Some str.[i] else None

let num_of_chars chars is_float =
  if is_float then Float (float_of_string (chars_to_str chars))
  else Int (int_of_string (chars_to_str chars))

(* char utils *)

let is_alpha (c : char) : bool =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let is_digit (c : char) : bool = c >= '0' && c <= '9'

(* scanner internals *)

type ctx = { source : string; idx : int; line : int; column : int }

let get_from_src (ctx : ctx) : char option = maybe_get ctx.source ctx.idx

(* advance [n] characters within the current line *)
let advance ?(n = 1) (ctx : ctx) : ctx =
  { ctx with idx = ctx.idx + n; column = ctx.column + n }

(* advance past a newline *)
let advance_line (ctx : ctx) : ctx =
  { ctx with idx = ctx.idx + 1; column = 1; line = ctx.line + 1 }

let rec scan_string (ctx : ctx) (chars : char list) :
    (ctx * string, Error.t) result =
  match get_from_src ctx with
  | Some '"' -> Ok (advance ctx, chars_to_str chars)
  | Some '\n' -> scan_string (advance_line ctx) ('\n' :: chars)
  | Some '\\' -> (
      match maybe_get ctx.source (ctx.idx + 1) with
      | Some '"' -> scan_string (advance ~n:2 ctx) ('"' :: chars)
      | _ -> scan_string (advance ctx) ('\\' :: chars))
  | Some char -> scan_string (advance ctx) (char :: chars)
  | None ->
      Error
        { line = ctx.line; column = ctx.column; msg = "unterminated string" }

let rec scan_number (ctx : ctx) (chars : char list) (in_float : bool) :
    (ctx * num, Error.t) result =
  match get_from_src ctx with
  | Some '.' when not in_float -> scan_number (advance ctx) ('.' :: chars) true
  | Some '.' ->
      Error { line = ctx.line; column = ctx.column; msg = "already in a float" }
  | Some c when is_digit c -> scan_number (advance ctx) (c :: chars) in_float
  | Some c when is_alpha c ->
      Error { line = ctx.line; column = ctx.column; msg = "not a valid number" }
  | _ -> Ok (ctx, num_of_chars chars in_float)

let rec scan_identifier (ctx : ctx) (chars : char list) : ctx * string =
  match get_from_src ctx with
  | Some c when is_alpha c || is_digit c ->
      scan_identifier (advance ctx) (c :: chars)
  | _ -> (ctx, chars_to_str chars)

type scan_res =
  | Tok of { tok : token; ctx : ctx }
  | Err of Error.t
  | Skip of ctx

let rec consume_comment (ctx : ctx) : ctx =
  match get_from_src ctx with
  | None -> ctx
  | Some '\n' -> advance_line ctx
  | Some _ -> consume_comment (advance ctx)

let rec scan_loop (ctx : ctx) (tokens : token list) :
    (token list, Error.t) result =
  let make_tok (typ : token_type) (lexeme : string) : token =
    { typ; lexeme; literal = None; line = ctx.line }
  in

  let match_next (match_char : char) : bool =
    match get_from_src (advance ctx) with
    | None -> false
    | Some c -> c = match_char
  in

  let lexeme_until idx' = String.sub ctx.source ctx.idx (idx' - ctx.idx) in

  let lift_tok (tok : token) : scan_res =
    let lexeme_len = String.length tok.lexeme in
    Tok { tok; ctx = advance ~n:lexeme_len ctx }
  in

  let lift_lit_tok (ctx' : ctx) (typ : token_type) (literal : lit option) :
      scan_res =
    Tok
      {
        tok = { typ; lexeme = lexeme_until ctx'.idx; literal; line = ctx'.line };
        ctx = ctx';
      }
  in

  match get_from_src ctx with
  | None ->
      Ok ({ typ = EOF; lexeme = ""; literal = None; line = ctx.line } :: tokens)
  | Some c -> (
      let res : scan_res =
        match c with
        | '(' -> lift_tok (make_tok LEFT_PAREN "(")
        | ')' -> lift_tok (make_tok RIGHT_PAREN ")")
        | '{' -> lift_tok (make_tok LEFT_BRACE "{")
        | '}' -> lift_tok (make_tok RIGHT_BRACE "}")
        | ',' -> lift_tok (make_tok COMMA ",")
        | '.' -> lift_tok (make_tok DOT ".")
        | '-' -> lift_tok (make_tok MINUS "-")
        | '+' -> lift_tok (make_tok PLUS "+")
        | ';' -> lift_tok (make_tok SEMICOLON ";")
        | '*' -> lift_tok (make_tok STAR "*")
        (* maybe two-char operators *)
        | '!' ->
            lift_tok
              (if match_next '=' then make_tok BANG_EQUAL "!="
               else make_tok BANG "!")
        | '=' ->
            lift_tok
              (if match_next '=' then make_tok EQUAL_EQUAL "=="
               else make_tok EQUAL "=")
        | '<' ->
            lift_tok
              (if match_next '=' then make_tok LESS_EQUAL "<="
               else make_tok LESS "<")
        | '>' ->
            lift_tok
              (if match_next '=' then make_tok GREATER_EQUAL ">="
               else make_tok GREATER ">")
        (* maybe comments *)
        | '/' ->
            if match_next '/' then Skip (consume_comment ctx)
            else lift_tok (make_tok SLASH "/")
        (* whitespace & friends *)
        | ' ' | '\r' | '\t' -> Skip (advance ctx)
        | '\n' -> Skip (advance_line ctx)
        (* strings *)
        | '"' -> (
            match scan_string (advance ctx) [] with
            | Ok (ctx', str) -> lift_lit_tok ctx' STRING (Some (Str str))
            | Error err -> Err err)
        (* numbers *)
        | c when is_digit c -> (
            match scan_number ctx [] false with
            | Ok (ctx', num) -> lift_lit_tok ctx' NUMBER (Some (Num num))
            | Error err -> Err err)
        (* keywords & identifiers *)
        | c when is_alpha c ->
            let ctx', id = scan_identifier ctx [] in
            let typ =
              match StrMap.find_opt id keywords with
              | Some keyword -> keyword
              | None -> IDENTIFIER
            in
            lift_lit_tok ctx' typ None
        | c ->
            Err
              {
                line = ctx.line;
                column = ctx.column;
                msg = "unexpected character: " ^ to_str c;
              }
      in
      match res with
      | Tok { tok; ctx = ctx' } -> scan_loop ctx' (tok :: tokens)
      | Skip ctx' -> scan_loop ctx' tokens
      | Err err -> Error err)

(* public API *)

let scan_tokens source = scan_loop { source; line = 1; column = 1; idx = 0 } []
