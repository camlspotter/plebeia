type t = private string
(** Type for the hash.  28 bytes *)

val to_string : t -> string
(** Returns a binary string representation of hash *)

val of_string : string -> t (* may raise exn *)
(** Create a hash from 28 bytes binary string *)

val hash : string -> t
(** Compute the hash of the given binary string *)
  
val hash_list : string list -> t
(** Compute the hash of the concatenation of the given binary strings *)

val reset_last_2bits : t -> t
(** set the last 2 bits of the hash to 0s *)
   
val zero : t
(** 28 bytes of 0's *)
