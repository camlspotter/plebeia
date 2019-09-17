open Cursor

val deep : 
  go_up: bool (* recover the original cursor position or not *)
  -> create_subtrees: bool (* create_subtree if necessary *)
  -> t
  -> Segment.t list 
  -> (t -> Segment.t -> (t * 'a, Error.t) Result.t) 
  -> (t * 'a, Error.t) Result.t
(** Multi Bud level interface. [deep] performs [f] against the node 
    pointed by the multi segments.
*)

val deep_ro : 
  t
  -> Segment.t list 
  -> (t -> Segment.t -> ('a, Error.t) Result.t) 
  -> ('a, Error.t) Result.t
(** Simplified version of [deep] for read only operations *)

val get : t -> Segment.t list -> (Value.t, Error.t) Result.t

val get' : t -> Segment.t list -> ([`Value of Value.t | `Bud of t], Error.t) Result.t

val insert : t -> Segment.t list -> Value.t -> (t, Error.t) Result.t

val upsert : t -> Segment.t list -> Value.t -> (t, Error.t) Result.t

val update : t -> Segment.t list -> Value.t -> (t, Error.t) Result.t

val delete : t -> Segment.t list -> (t, Error.t) Result.t
(** If the target does not exists, do nothing *)
    
val create_subtree : create_subtrees: bool -> t -> Segment.t list -> (t, Error.t) Result.t

val subtree : t -> Segment.t list -> (t, Error.t) Result.t

val subtree_or_create : create_subtrees: bool -> t -> Segment.t list -> (t, Error.t) Result.t

val copy : create_subtrees: bool -> t -> Segment.t list -> Segment.t list -> (t, Error.t) Result.t
(** Subtree copy by making two nodes point to the same subtree. 
    
    Copy attempts which introduce loops are rejected. 
*)
