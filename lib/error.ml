type t =
  { line : int
  ; column : int
  ; msg : string
  }

let report line where msg =
  print_endline ("[line " ^ line ^ "] Error" ^ where ^ ": " ^ msg)
;;

let error err = report (string_of_int err.line) "" err.msg
