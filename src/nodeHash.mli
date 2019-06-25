open Hash 
open Node

val of_empty_bud : hash56
val of_bud : hash56 option -> hash56
val of_leaf : Value.t -> hash56
val of_internal : hash56 -> hash56 -> hash56
val of_extender : Segment.t -> hash56 -> hash56
val of_extender' : segment_code:string -> hash56 -> hash56 

val hash: cursor -> (cursor * hash56)
(** Computes the hash of the cursor without committing. *)

