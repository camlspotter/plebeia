type t = private string
(** Type for the hash.  28 bytes *)

val to_string : t -> string
val of_string : string -> t (* may raise exn *)

val hash : string -> t
val hash_list : string list -> t

val reset_last_2bits : t -> t

val zero : t
