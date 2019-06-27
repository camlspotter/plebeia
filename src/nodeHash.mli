open Node

(** Hash calculation *)

val of_empty_bud : Hash.t
val of_bud : Hash.t option -> Hash.t
val of_leaf : Value.t -> Hash.t
val of_internal : Hash.t -> Hash.t -> Hash.t
val of_extender : Segment.t -> Hash.t -> Hash.t
val of_extender' : segment_code:string -> Hash.t -> Hash.t 

val hash: cursor -> (cursor * Hash.t)
(** Computes the hash of the cursor without committing. *)

