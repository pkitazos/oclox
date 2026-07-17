open Oclox

let run s =
  match Scanner.scan_tokens s with
  | Ok tokens -> List.iter Token.print tokens
  | Error err -> Error.report s err
;;

let run_prompt () =
  while true do
    print_string "oclox> ";
    run (read_line ())
  done
;;

let run_file path = run (In_channel.with_open_text path In_channel.input_all)

let () =
  match Array.to_list Sys.argv with
  | [ _ ] -> run_prompt ()
  | [ _; path ] -> run_file path
  | _ -> print_endline "Usage: oclox [script]"
;;
