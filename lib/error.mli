type t

type kind = Lex | Parse

val report : string -> t -> unit
val make : kind:kind -> line:int -> column:int -> string -> t
val msg : t -> string
