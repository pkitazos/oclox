type t

val error : t -> unit
val make : line:int -> column:int -> string -> t
