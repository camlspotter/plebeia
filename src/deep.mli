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
  
type where_from =
  | From_above of dir
  | From_below of dir

and dir =
  | Left
  | Right
  | Center

type position = 
  where_from list (* Where the traversal comes from *)
  * t (* The cursor *)
  
val traverse : position -> position option
(** Traverse the entire tree 

    let f cursor =
      let rec loop acc (_, cursor as pos) =
        (* do something over cursor *)
        let acc = ... in
        (* update the position and loop *)
        match traverse pos with
        | None -> acc
        | Some pos -> loop acc pos
      in
      loop ([], cursor)
    
    * For huge trees, you may want to [forget] on memory nodes.
*)

