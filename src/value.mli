(** Module encapsulating the values inserted in the Patricia tree. *)

type t
(** Type of a value. *)

val of_string : string -> t
val to_string : t -> string
(** conversions between binary bytes *)

val to_hex : t -> Hex.t
val to_hex_string : t -> string

val length : t -> int 
(** in bytes *)
