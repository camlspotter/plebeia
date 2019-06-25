open Hash 
open Node

val of_empty_bud : t
val of_bud : t option -> t
val of_leaf : Value.t -> t
val of_internal : t -> t -> t
val of_extender : Segment.t -> t -> t
val of_extender' : segment_code:string -> t -> t 

val hash: cursor -> (cursor * t)
(** Computes the hash of the cursor without committing. *)

