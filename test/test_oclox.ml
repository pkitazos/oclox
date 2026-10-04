open Oclox

let failures = ref 0

let parse_to_string ~emit_trivia source =
  match Lexer.lex source ~emit_trivia with
  | Error e -> Error e
  | Ok tokens -> Result.map Ast.to_string (Parser.parse tokens)
;;

let check name ?(emit_trivia = false) source expected =
  let actual =
    match parse_to_string ~emit_trivia source with
    | Ok s -> s
    | Error _ -> "error"
  in
  if actual <> expected
  then (
    incr failures;
    Printf.printf "FAIL %s\n  source:   %S\n  expected: %s\n  actual:   %s\n" name source expected actual)
;;

let () =
  check "literal" "1" "1";
  check "left associativity" "1 - 2 - 3" "(- (- 1 2) 3)";
  check "precedence" "1 + 2 * 3" "(+ 1 (* 2 3))";
  check "unary" "!true == false" "(== (! true) false)";
  check "missing paren" "(1 + 2" "error";
  check "trailing tokens" "1 2" "error";
  (* trivia is emitted by the lexer but must be invisible to the parser *)
  check "line comment" ~emit_trivia:true "1 + // hi\n 2" "(+ 1 2)";
  check "block comment" ~emit_trivia:true "1 /* x */ + 2" "(+ 1 2)";
  check "nested block comment" ~emit_trivia:true "/* a /* b */ c */ 1" "1";
  if !failures > 0 then exit 1 else print_endline "all tests passed"
;;
