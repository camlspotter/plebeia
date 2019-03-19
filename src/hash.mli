type h28 (* Hash of 28 byte length *)
type h56 (* Hash of nodes, 56 byte length *)

type 'h t = private string
(** Type for the hash.  The size is given by the type parameter *)

type hash28 = h28 t
type hash56 = h56 t

val to_string : 'h t -> string

val hash28_of_string : string -> hash28
val hash56_of_string : string -> hash56

val hash : string -> hash28
val hash_list : string list -> hash28

val extend_to_hash56 : hash28 -> hash56
val shorten_to_hash28 : hash56 -> hash28

val reset_last_2bits : hash28 -> hash28
