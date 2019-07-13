open Node

(** Long hash calculation 

    Node's has 56 byte length "long" hash.
*)

type t (** the type for the long hash *)

(** Compute long hash of a node using the underlying long hashes *)

val of_internal : t -> t -> t
val of_bud : t option -> t
val of_leaf : Value.t -> t
val of_leaf_hash : Hash.t -> t
val of_extender : Segment.t -> t -> t
val of_extender' : segment_code:string -> t -> t

val shorten : t -> Hash.t

val long_hash: Context.t -> node -> (view * t)
(** Computes the hash of the cursor without committing. *)

val hash : Context.t -> node -> (view * Hash.t)

