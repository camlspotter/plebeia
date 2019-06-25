type h28 (* Hash of 28 byte length *)
type h56 (* Hash of nodes, 56 byte length *)

type 'h gen = private string
(** Type for the hash.  The size is given by the type parameter *)

type hash28 = h28 gen
type hash56 = h56 gen
type t = hash56
  
val to_string : 'h gen -> string

val hash28_of_string : string -> hash28
val hash56_of_string : string -> hash56

val hash : string -> hash28
val hash_list : string list -> hash28

val extend_to_t : hash28 -> t
val shorten_to_hash28 : t -> hash28

val reset_last_2bits : hash28 -> hash28
