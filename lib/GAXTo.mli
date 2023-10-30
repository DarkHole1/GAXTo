type t

val create : unit -> t
val run : t -> string -> t
val run' : string -> t
val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
