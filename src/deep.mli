open Cursor

val deep : 
  go_up: bool (* recover the original cursor position or not *)
  -> create_subtrees: bool (* create_subtree if necessary *)
  -> t
  -> Segment.t list 
  -> (t -> Segment.t -> (t * 'a, Error.t) result) 
  -> (t * 'a, Error.t) result
(** Multi Bud level interface. [deep] performs [f] against the node 
    pointed by the multi segments.
*)

val deep_ro : 
  t
  -> Segment.t list 
  -> (t -> Segment.t -> ('a, Error.t) result) 
  -> ('a, Error.t) result
(** Simplified version of [deep] for read only operations *)

val deep_get : t -> Segment.t list -> (Value.t, Error.t) result

val deep_upsert : t -> Segment.t list -> Value.t -> (t, Error.t) result

val deep_delete : t -> Segment.t list -> (t, Error.t) result
(** If the target does not exists, do nothing *)
    
val deep_create_subtree : t -> Segment.t list -> (t, Error.t) result

val copy : create_subtrees: bool -> t -> Segment.t list -> Segment.t list -> (t, Error.t) result
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

