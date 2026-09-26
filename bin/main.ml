open Oclox

let ( let* ) = Result.bind

type args = { emit_tokens : bool }

let pipeline source args =
  let* tokens = Lexer.lex source ~emit_trivia:args.emit_tokens in
  let* expr = Parser.parse tokens in
  Ok expr

let run source args =
  match pipeline source args with
  | Ok expr -> print_endline (Ast.to_string expr)
  | Error err -> Error.report source err

let run_prompt args =
  let rec loop () =
    print_string "oclox> ";
    flush stdout;
    match In_channel.input_line stdin with
    | None -> print_newline ()
    | Some line ->
      run line args;
      loop ()
  in
  loop ()

let run_file path args =
  run (In_channel.with_open_text path In_channel.input_all) args

let parse_argv () =
  let emit_tokens = ref false in
  let script = ref None in
  let specs = [("-t", Arg.Set emit_tokens, " Emit trivia tokens from the lexer")] in
  let anon path =
    match !script with
    | None -> script := Some path
    | Some _ -> raise (Arg.Bad "at most one script may be given")
  in
  Arg.parse specs anon "Usage: oclox [options] [script]";
  ({ emit_tokens = !emit_tokens }, !script)

let () =
  let args, script = parse_argv () in
  match script with
  | None -> run_prompt args
  | Some path -> run_file path args
