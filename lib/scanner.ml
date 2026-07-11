open Token
module StrMap = Map.Make (String)

let keywords_map () =
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

type scan_res = Tok of token | Skip | Error of Error.my_error

let to_str = String.make 1

let chars_to_str (chars : char list) =
  List.map to_str chars |> List.rev |> String.concat ""

let maybe_get (str : string) (i : int) : char option =
  try Some (String.get str i) with _ -> None

let is_alpha (c : char) : bool =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

let is_digit (c : char) : bool = c >= '0' && c <= '9'

let rec consume_until_newline (lexemes : char list) : char list =
  match lexemes with
  | [] -> lexemes
  | '\n' :: rest -> rest
  | _ :: rest -> consume_until_newline rest

let rec scan_string (lexemes : char list) (str : string) (line : int) :
    (string * char list * int, Error.my_error) result =
  match lexemes with
  | '"' :: rest -> Ok (str, rest, line)
  | '\n' :: rest -> scan_string rest str (line + 1)
  | '\\' :: '\"' :: rest -> scan_string rest (str ^ "\"") line
  | char :: rest -> scan_string rest (str ^ to_str char) line
  | [] -> Error { line; column = 0; msg = "unterminated string" }

type ctx = { source : string; current : int; line : int }

let get_from_src (ctx : ctx) : char option = maybe_get ctx.source ctx.current

let rec scan_string_2 (ctx : ctx) (chars : char list) :
    (ctx * string, Error.my_error) result =
  try
    match String.get ctx.source ctx.current with
    | '"' ->
        Ok
          ( { ctx with current = ctx.current + 1 },
            List.map to_str chars |> List.rev |> String.concat "" )
    | '\n' ->
        scan_string_2
          {
            source = ctx.source;
            current = ctx.current + 1;
            line = ctx.line + 1;
          }
          ('\n' :: chars)
    | '\\' ->
        if String.get ctx.source (ctx.current + 1) == '\"' then
          scan_string_2 { ctx with current = ctx.current + 1 } ('\"' :: chars)
        else scan_string_2 { ctx with current = ctx.current + 1 } ('\\' :: chars)
    | char ->
        scan_string_2 { ctx with current = ctx.current + 1 } (char :: chars)
  with Invalid_argument _ ->
    Error { line = ctx.line; column = 0; msg = "unterminated string" }

let rec scan_number (lexemes : char list) (str : string) (line : int)
    (is_float : bool) : (string * char list * int * bool, Error.my_error) result
    =
  match lexemes with
  | [] -> Ok (str, [], line, is_float)
  | c :: rest when is_digit c -> scan_number rest (str ^ to_str c) line is_float
  | '.' :: rest when not is_float -> scan_number rest (str ^ ".") line true
  | c :: rest when is_alpha c || c == '.' || c == '_' ->
      Error { line; column = 0; msg = "not a valid number" }
  | c :: rest -> Ok (str, rest, line, is_float)

let rec scan_number_2 (ctx : ctx) (chars : char list) (in_float : bool) :
    (ctx * num, Error.my_error) result =
  try
    match String.get ctx.source ctx.current with
    | '.' when not in_float ->
        scan_number_2 { ctx with current = ctx.current + 1 } ('.' :: chars) true
    | '.' when in_float ->
        Error { line = ctx.line; column = 0; msg = "already in a float" }
    | c when is_digit c ->
        scan_number_2 { ctx with current = ctx.current + 1 } (c :: chars) true
    | c when is_alpha c || c == '_' ->
        Error { line = ctx.line; column = 0; msg = "not a valid number" }
    | c ->
        let num_string =
          List.map to_str chars |> List.rev |> String.concat ""
        in
        Ok
          ( { ctx with current = ctx.current + 1 },
            if in_float then Float (float_of_string num_string)
            else Int (int_of_string num_string) )
  with Invalid_argument _ ->
    Error { line = ctx.line; column = 0; msg = "unterminated string" }

let rec scan_identifier (lexemes : char list) (str : string) :
    string * char list =
  match lexemes with
  | [] -> (str, [])
  | c :: rest when is_alpha c || is_digit c ->
      scan_identifier rest (str ^ to_str c)
  | rest -> (str, rest)

let rec scan_identifier2 (ctx : ctx) (chars : char list) : ctx * string =
  match get_from_src ctx with
  | Some c when is_alpha c || is_digit c ->
      scan_identifier2 { ctx with current = ctx.current + 1 } (c :: chars)
  | Some c -> (ctx, chars_to_str chars)
  | None -> (ctx, chars_to_str chars)

let scan (source : string) : (token list, Error.my_error) result = Ok []

type scan_res2 =
  | Tok2 of { tok : token; current' : int; line' : int }
  | Err2 of Error.my_error
  | Skip2 of { current' : int; line' : int }

type pos = { current : int; line : int }

let rec consume_comment (source : string) (current : int) : int =
  match String.get source current with
  | '\n' -> current + 1
  | c -> consume_comment source (current + 1)

let rec scan_loop (source : string) (tokens : token list) (line : int)
    (column : int) (start : int) (current : int) :
    (token list, Error.my_error) result =
  let ctx = { source; current; line } in

  let lift_tok (tok : token) : scan_res2 =
    Tok2 { tok; current' = current; line' = line }
  in

  let make_tok (typ : token_type) (lexeme : string) : token =
    { typ; lexeme; literal = None; line }
  in

  let match_next (match_char : char) : bool =
    String.get source (current + 1) == match_char
  in

  match get_from_src ctx with
  | None -> Ok tokens
  | Some c -> (
      let res : scan_res2 =
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
            if match_next '=' then
              Tok2
                {
                  tok = make_tok BANG_EQUAL "!=";
                  current' = current + 2;
                  line' = line;
                }
            else lift_tok (make_tok BANG "!")
        | '=' ->
            if match_next '=' then
              Tok2
                {
                  tok = make_tok EQUAL_EQUAL "==";
                  current' = current + 2;
                  line' = line;
                }
            else lift_tok (make_tok EQUAL "=")
        | '<' ->
            if match_next '=' then
              Tok2
                {
                  tok = make_tok LESS_EQUAL "<=";
                  current' = current + 2;
                  line' = line;
                }
            else lift_tok (make_tok LESS "<")
        | '>' ->
            if match_next '=' then
              Tok2
                {
                  tok = make_tok GREATER_EQUAL ">=";
                  current' = current + 2;
                  line' = line;
                }
            else lift_tok (make_tok GREATER ">")
        (* maybe comments *)
        | '/' ->
            if match_next '/' then
              Skip2
                { current' = consume_comment source current; line' = line + 1 }
            else lift_tok (make_tok SLASH "/")
        (* whitespace & friends *)
        | ' ' | '\r' | '\t' -> Skip2 { current' = current + 1; line' = line }
        | '\n' -> Skip2 { current' = current + 1; line' = line + 1 }
        (* strings *)
        | '"' -> (
            match scan_string_2 { source; current; line } [] with
            | Ok (ctx, str) ->
                Tok2
                  {
                    tok =
                      {
                        typ = STRING;
                        lexeme = String.sub source current ctx.current;
                        literal = Some (Str str);
                        line = ctx.line;
                      };
                    current' = ctx.current;
                    line' = ctx.line;
                  }
            | Error err -> Err2 err)
        | c when is_digit c -> (
            match scan_number_2 { source; current; line } [] false with
            | Ok (ctx, num) ->
                Tok2
                  {
                    tok =
                      {
                        typ = NUMBER;
                        lexeme = String.sub source current ctx.current;
                        literal = Some (Num num);
                        line = ctx.line;
                      };
                    current' = ctx.current;
                    line' = ctx.line;
                  }
            | Error err -> Err2 err)
        | c when is_alpha c || c == '_' ->
            let ctx, id = scan_identifier2 { source; current; line } [] in
            Tok2
              {
                tok =
                  {
                    typ = IDENTIFIER;
                    lexeme = String.sub source current ctx.current;
                    literal = Some (Str id);
                    line = ctx.line;
                  };
                current' = ctx.current;
                line' = ctx.line;
              }
        | _ -> Err2 { line; column = 0; msg = "unexpected character" }
      in
      match res with
      | Tok2 { tok; current'; line' } ->
          scan_loop source (tok :: tokens) line' column current' 0
      | Skip2 { current'; line' } ->
          scan_loop source tokens line' column current' 0
      | Err2 err -> Error err)

let rec scan_tokens (lexemes : char list) (tokens : token list) (start : int)
    (current : int) (line : int) (at_end : int -> int) :
    (token list, Error.my_error) result =
  let keywords = keywords_map () in

  let make_tok (typ : token_type) (lexeme : string) =
    Tok { typ; lexeme; literal = None; line }
  in

  let match_next (rest : char list) (expected_char : char)
      (match_typ : token_type) (single_typ : token_type) (match_str : string)
      (single_str : string) : scan_res * char list =
    match rest with
    | c :: rest when c = expected_char -> (make_tok match_typ match_str, rest)
    | _ -> (make_tok single_typ single_str, rest)
  in

  let with_line (t, r) = (t, r, line) in

  match lexemes with
  | [] -> Ok tokens
  | char :: rest -> (
      let next_token, rest, line =
        match char with
        (* Single-char tokens *)
        | '(' -> (make_tok LEFT_PAREN "(", rest, line)
        | ')' -> (make_tok RIGHT_PAREN ")", rest, line)
        | '{' -> (make_tok LEFT_BRACE "{", rest, line)
        | '}' -> (make_tok RIGHT_BRACE "}", rest, line)
        | ',' -> (make_tok COMMA ",", rest, line)
        | '.' -> (make_tok DOT ".", rest, line)
        | '-' -> (make_tok MINUS "-", rest, line)
        | '+' -> (make_tok PLUS "+", rest, line)
        | ';' -> (make_tok SEMICOLON ";", rest, line)
        | '*' -> (make_tok STAR "*", rest, line)
        (* Operators that can be one or two characters *)
        | '!' -> with_line (match_next rest '=' BANG_EQUAL BANG "!=" "!")
        | '=' -> with_line (match_next rest '=' EQUAL_EQUAL EQUAL "==" "=")
        | '<' -> with_line (match_next rest '=' LESS_EQUAL LESS "<=" "<")
        | '>' -> with_line (match_next rest '=' GREATER_EQUAL GREATER ">=" ">")
        | '/' -> (
            match rest with
            | '/' :: rest -> (Skip, consume_until_newline rest, line)
            | _ -> (make_tok SLASH "/", rest, line))
        | ' ' | '\r' | '\t' -> (Skip, rest, line)
        | '\n' -> (Skip, rest, line + 1)
        | '"' -> (
            (* enter string man *)
            match scan_string rest "" line with
            | Ok (str, rest, line) ->
                ( Tok
                    {
                      typ = STRING;
                      lexeme = "some string";
                      literal = Some (Str str);
                      line;
                    },
                  rest,
                  line )
            | Error err -> (Error err, rest, line))
        | c when is_digit c -> (
            match scan_number (c :: rest) "" line false with
            | Ok (str, rest, line, is_float) ->
                ( Tok
                    {
                      typ = NUMBER;
                      lexeme = "some num";
                      literal =
                        Some
                          (Num
                             (if is_float then Float (float_of_string str)
                              else Int (int_of_string str)));
                      line;
                    },
                  rest,
                  line )
            | Error err -> (Error err, rest, line))
        | c when is_alpha c -> (
            let str, rest = scan_identifier (c :: rest) "" in
            match StrMap.find_opt str keywords with
            | Some keyword -> (make_tok keyword str, rest, line)
            | None -> (make_tok IDENTIFIER str, rest, line))
        | c ->
            ( Error
                { line; column = 0; msg = "Unexpected character. " ^ to_str c },
              rest,
              line )
      in
      match next_token with
      | Tok token ->
          scan_tokens rest (token :: tokens) current (current + 1) line at_end
      | Skip -> scan_tokens rest tokens current (current + 1) line at_end
      | Error err -> Error err)
