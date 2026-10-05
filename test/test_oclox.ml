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
    | Error e -> "error: " ^ Error.msg e
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
  check "missing paren" "(1 + 2" "error: expected ')' after expression";
  check "trailing tokens" "1 2" "error: unexpected token after expression";
  (* comma and ternary *)
  check "comma is left-associative" "1, 2, 3" "(, (, 1 2) 3)";
  check "ternary is right-associative" "1 ? 2 : 3 ? 4 : 5" "(? 1 2 (? 3 4 5))";
  check "ternary in the middle" "1 ? 2 ? 3 : 4 : 5" "(? 1 (? 2 3 4) 5)";
  check "comma in the middle" "1 ? 2, 3 : 4" "(? 1 (, 2 3) 4)";
  check "comma binds looser than ternary" "1, 2 ? 3 : 4" "(, 1 (? 2 3 4))";
  check "ternary binds tighter than comma" "1 ? 2 : 3, 4" "(, (? 1 2 3) 4)";
  check "ternary binds looser than equality" "1 == 2 ? 3 + 4 : 5" "(? (== 1 2) (+ 3 4) 5)";
  check "ternary missing colon" "1 ? 2" "error: expected ':' in ternary";
  (* error productions: a binary operator where an operand was expected *)
  check "missing operand for ==" "== 1" "error: missing left-hand operand for '=='";
  check "missing operand for <" "< 1" "error: missing left-hand operand for '<'";
  check "missing operand for +" "+ 1" "error: missing left-hand operand for '+'";
  check "missing operand mid-expression" "1 + * 2" "error: missing left-hand operand for '*'";
  check "leading minus is unary, not a missing operand" "-1" "(- 1)";
  (* trivia is emitted by the lexer but must be invisible to the parser *)
  check "line comment" ~emit_trivia:true "1 + // hi\n 2" "(+ 1 2)";
  check "block comment" ~emit_trivia:true "1 /* x */ + 2" "(+ 1 2)";
  check "nested block comment" ~emit_trivia:true "/* a /* b */ c */ 1" "1";
  if !failures > 0 then exit 1 else print_endline "all tests passed"
;;
