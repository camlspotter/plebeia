type t
type cursor = Cursor.t

val create : 
  ?context_pos:int64 
  -> ?context_length:int 
  -> prefix:string
  -> unit
  -> t

val open_ : 
  ?context_pos:int64 
  -> prefix:string
  -> t

val close : t -> unit

val empty_cursor : t -> Cursor.t

val hash : Cursor.t -> Cursor.t * Hash.t

val get : Cursor.t -> Segment.t list -> (Value.t, Error.t) Result.t
val get' : Cursor.t -> Segment.t list -> ([`Value of Value.t | `Bud of Cursor.t], Error.t) Result.t
val insert : Cursor.t -> Segment.t list -> Value.t -> (Cursor.t, Error.t) Result.t
val upsert : Cursor.t -> Segment.t list -> Value.t -> (Cursor.t, Error.t) Result.t
val update : Cursor.t -> Segment.t list -> Value.t -> (Cursor.t, Error.t) Result.t
val delete : Cursor.t -> Segment.t list -> (Cursor.t, Error.t) Result.t
val create_subtree : create_subtrees: bool -> Cursor.t -> Segment.t list -> (Cursor.t, Error.t) Result.t
val subtree_or_create : create_subtrees: bool -> Cursor.t -> Segment.t list -> (Cursor.t, Error.t) Result.t
val deep : 
  go_up: bool (* recover the original cursor position or not *)
  -> create_subtrees: bool (* create_subtree if necessary *)
  -> Cursor.t
  -> Segment.t list 
  -> (Cursor.t -> Segment.t -> (Cursor.t * 'a, Error.t) Result.t) 
  -> (Cursor.t * 'a, Error.t) Result.t
(** Multi Bud level interface. [deep] performs [f] against the node 
    pointed by the multi segments.
*)

val copy: create_subtrees: bool -> Cursor.t -> Segment.t list -> Segment.t list -> (Cursor.t, Error.t) Result.t

val fold : init:'a -> Cursor.t -> ('a -> Segment.t -> [`Leaf of Value.t | `Bud] -> ('a, 'b) Result.t) -> (('a, 'b) Result.t, Error.t) Result.t

val commit : t -> Cursor.t -> Cursor.t * Hash.t
val checkout : t -> Hash.t -> Cursor.t option

val stat : t -> Stat.t
