let report line where msg =
  print_endline ("[line " ^ line ^ "] Error" ^ where ^ ": " ^ msg)

let error line msg = report line "" msg

type num = Int of int | Float of float [@@deriving show]
type lit = Num of num | Str of string [@@deriving show]

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

type token = {
  typ : token_type;
  lexeme : string;
  literal : lit option;
  line : int;
}

type scan_res = Tok of token | Skip | Error of string

let to_str = String.make 1

let show_token token =
  print_endline
    (show_token_type token.typ ^ " " ^ token.lexeme ^ " "
    ^ match token.literal with None -> "" | Some lit -> show_lit lit)

let add_token (typ : token_type) (lexeme : string) (literal : lit option)
    (line : int) (tokens : token list) : token list =
  { typ; lexeme; literal; line } :: tokens

let rec consume_till_end_of_line (lexemes : char list) : char list =
  match lexemes with
  | [] -> lexemes
  | '\n' :: rest -> rest
  | _ :: rest -> consume_till_end_of_line rest

let rec scan_string (lexemes : char list) (str : string) (line : int) :
    (string * char list * int, unit) result =
  match lexemes with
  | '"' :: rest -> Ok (str, rest, line)
  | '\n' :: rest -> scan_string rest str (line + 1)
  | '\\' :: '\"' :: rest -> scan_string rest (str ^ "\"") line
  | char :: rest -> scan_string rest (str ^ to_str char) line
  | [] -> Error ()

let is_alpha (c : char) : bool =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

let is_digit (c : char) : bool = c > '0' && c <= '9'

let rec scan_number (lexemes : char list) (str : string) (line : int)
    (is_float : bool) : (string * char list * int * bool, unit) result =
  match lexemes with
  | [] -> Ok (str, [], line, is_float)
  | c :: rest when is_digit c -> scan_number rest (str ^ to_str c) line is_float
  | '.' :: rest when not is_float -> scan_number rest (str ^ ".") line true
  | c :: rest when is_alpha c -> Error () (* not a valid number *)
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
        (* todo: how do i know whether I need to pass a reduced `rest` *)
        | '!' ->
            let t, r = match_next rest '=' BANG_EQUAL BANG "!=" "!" in
            (t, r, line)
        | '=' ->
            let t, r = match_next rest '=' EQUAL_EQUAL EQUAL "==" "=" in
            (t, r, line)
        | '<' ->
            let t, r = match_next rest '=' LESS_EQUAL LESS "<=" "<" in
            (t, r, line)
        | '>' ->
            let t, r = match_next rest '=' GREATER_EQUAL GREATER ">=" ">" in
            (t, r, line)
        | '/' -> (
            match rest with
            | '/' :: rest -> (Skip, consume_till_end_of_line rest, line)
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

let rec read_file ic acc =
  try read_file ic (input_line ic :: acc)
  with End_of_file ->
    close_in ic;
    String.concat "\n" acc

let run s =
  let lexemes = List.of_seq (String.to_seq s) in
  match scan_tokens lexemes [] 0 0 1 (fun _ -> 0) with
  | Ok tokens -> List.iter show_token (List.rev tokens)
  | Error msg -> error "?" msg

let run_prompt () =
  while true do
    print_string "oclox> ";
    let s = read_line () in
    run s
  done

let run_file path =
  let ic = open_in path in
  let s = read_file ic [] in
  run s

let () =
  match Array.to_list Sys.argv with
  | [ _ ] -> run_prompt ()
  | [ _; path ] -> run_file path
  | _ -> print_endline "Usage: oclox [script]"
