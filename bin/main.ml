open Oclox

let report line where msg =
  print_endline ("[line " ^ line ^ "] Error" ^ where ^ ": " ^ msg)

let error line msg = report line "" msg

let rec read_file ic acc =
  try read_file ic (input_line ic :: acc)
  with End_of_file ->
    close_in ic;
    String.concat "\n" acc

let run s =
  let lexemes = List.of_seq (String.to_seq s) in
  match Scanner.scan_tokens lexemes [] 0 0 1 (fun _ -> 0) with
  | Ok tokens -> List.iter Token.show_token (List.rev tokens)
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
