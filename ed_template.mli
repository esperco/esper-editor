(*
   Remove or rewrite unsupported markup from HTML snippet
*)
val sanitize : string -> string

(*
   Sanitize and replace variables with text
*)
val instantiate : (string * string) list -> string -> string

val tests : (string * (unit -> bool)) list
