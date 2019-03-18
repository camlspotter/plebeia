type t = private string
(** Type for the hash.  448 bits *)

val to_string : t -> string
val to_hex : t -> string

val of_string : string -> t

type value_hash = private string
(** The first 224 bits of the leaf hash, used in KVS and storage *)

val string_of_value_hash : value_hash -> string
val value_hash_of_string : string -> value_hash

val of_leaf : Value.t -> t
val to_value_hash : t -> value_hash
(** Note: no check whether the hash is for the leaf *)

val of_value_hash : value_hash -> t
(** Make a hash of leaf from its first 224bits recorded in the storage *)

val of_empty_bud : t
val of_bud : t option -> t
val of_internal_node : t -> t -> t
val of_extender : Path.segment -> t -> t
val of_extender' : segment_code: string -> t -> t

val encode_segment : Path.segment -> string
val decode_segment : string -> Path.segment
