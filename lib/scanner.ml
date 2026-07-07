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

type scan_res = Tok of token | Skip | Error of string

let to_str = String.make 1

let is_alpha (c : char) : bool =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

let is_digit (c : char) : bool = c >= '0' && c <= '9'

let rec consume_until_newline (lexemes : char list) : char list =
  match lexemes with
  | [] -> lexemes
  | '\n' :: rest -> rest
  | _ :: rest -> consume_until_newline rest

let rec scan_string (lexemes : char list) (str : string) (line : int) :
    (string * char list * int, unit) result =
  match lexemes with
  | '"' :: rest -> Ok (str, rest, line)
  | '\n' :: rest -> scan_string rest str (line + 1)
  | '\\' :: '\"' :: rest -> scan_string rest (str ^ "\"") line
  | char :: rest -> scan_string rest (str ^ to_str char) line
  | [] -> Error ()

let rec scan_number (lexemes : char list) (str : string) (line : int)
    (is_float : bool) : (string * char list * int * bool, unit) result =
  match lexemes with
  | [] -> Ok (str, [], line, is_float)
  | c :: rest when is_digit c -> scan_number rest (str ^ to_str c) line is_float
  | '.' :: rest when not is_float -> scan_number rest (str ^ ".") line true
  | c :: rest when is_alpha c || c == '.' || c == '_' ->
      Error () (* not a valid number *)
  | c :: rest -> Ok (str, rest, line, is_float)

let rec scan_identifier (lexemes : char list) (str : string) :
    string * char list =
  match lexemes with
  | [] -> (str, [])
  | c :: rest when is_alpha c || is_digit c ->
      scan_identifier rest (str ^ to_str c)
  | rest -> (str, rest)

let rec scan_tokens (lexemes : char list) (tokens : token list) (start : int)
    (current : int) (line : int) (at_end : int -> int) :
    (token list, string) result =
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
            | Error _ -> (Error "Unterminated string.", rest, line))
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
            | Error _ -> (Error "Bad number.", rest, line))
        | c when is_alpha c -> (
            let str, rest = scan_identifier (c :: rest) "" in
            match StrMap.find_opt str keywords with
            | Some keyword -> (make_tok keyword str, rest, line)
            | None -> (make_tok IDENTIFIER str, rest, line))
        | c -> (Error ("Unexpected character. " ^ to_str c), rest, line)
      in
      match next_token with
      | Tok token ->
          scan_tokens rest (token :: tokens) current (current + 1) line at_end
      | Skip -> scan_tokens rest tokens current (current + 1) line at_end
      | Error err -> Error ("Unexpected character. ++ " ^ err))
