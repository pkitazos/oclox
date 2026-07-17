type t

val report : string -> t -> unit
val make : line:int -> column:int -> string -> t
