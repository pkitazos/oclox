type t =
  { line : int
  ; column : int
  ; msg : string
  }

let rec report (source : string) (err : t) =
  let line = get_error_line source err.line in
  print_endline
    (Printf.sprintf
       "%s\n%s\nError: %s"
       line
       (annotate_error_line line err.column)
       err.msg)

and get_error_line source line_num =
  let lines = String.split_on_char '\n' source in
  List.nth lines (line_num - 1)

and annotate_error_line line col_num = String.make (col_num - 1) ' ' ^ "^"

let make ~line ~column msg = { line; column; msg }
