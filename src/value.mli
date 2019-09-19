(** Module encapsulating the values inserted in the Patricia tree. *)

type t
(** Type of a value. *)

val of_string : string -> t
val to_string : t -> string
(** conversions between binary bytes *)

val length : t -> int 
(** in bytes *)
