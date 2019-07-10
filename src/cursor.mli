open Types
open Node

type t = cursor
(** Zipper for Merkle tree *)

val empty : Context.t -> t
(** Creates a cursor to a new, empty tree. *)

val subtree : t -> Segment.t -> (t, error) result
(** Moves the cursor down a segment, to the root of a sub-tree. Think
    "cd segment/" *)

val create_subtree: t -> Segment.t -> (t, error) result
(** Create a subtree (bud). Think "mkdir segment".
    The cursor does NOT move from the original position. *)

val subtree_or_create : t -> Segment.t -> (t, error) result
(** Same as subtree but create a subtree if not exists *)
    
(** Result of access_gen *)
type access_result =
  | Empty_bud (* The bud is empty *)
  | Collide of cursor * view (* The segment was blocked by an existing leaf or bud *)
  | Middle_of_extender of cursor * Segment.t * Segment.t * Segment.t (* The segment ends or diverges at the middle of an Extender with the common prefix, the remaining extender, and the rest of segment *)
  | Reached of cursor * view (* just reached to a node *)

val error_access : access_result -> ('a, error) result
(** Make an access result into an error *)

val access_gen : t -> Segment.t -> (access_result, error) result
(** Follow a segment *)

val go_top : t -> (t, error) result
(** Move up to the top *)

val parent : t -> (t, error) result
(** Moves the cursor back to the parent tree. Think "cd .." *)

val get : t -> Segment.t -> (Value.t, error) result
(** Gets a value if present in the current tree at the given
    segment. *)

val insert: t -> Segment.t -> Value.t -> (t, error) result
(** Inserts a value at the given segment in the current tree.
    The cursor does NOT move from the original position. *)

val upsert: t -> Segment.t -> Value.t -> (t, error) result
(** Upserts. This can still fail if the segment leads to a subtree.
    The cursor does NOT move from the original position. *)

val update: t -> Segment.t -> Value.t -> (t, error) result
(** Update. A value must be bound at the segment. *)

val delete: t -> Segment.t -> (t, error) result
(** Delete a leaf or subtree.
    The cursor does NOT move from the original position. *)

val snapshot: t -> Segment.t -> Segment.t -> (t, error) result
(** Snapshots a subtree at segment and place a soft link to it at
    another segment location. 

    When a snapshot is created, a subtree is pointed by more than one nodes.
    
    XXX Not tested.
*)

val go_below_bud : t -> (t option, error) result
(** This function expects a cursor positionned on a bud 
    and moves it one step below. *)

val go_down_extender : t -> (t, error) result
(** Go down an Extender node.  The cursor must point to an Extender. *)
    
val go_side : Segment.side -> t -> (t, error) result
(** Go down an Internal node.  The cursor must point to an Internal. *)

val go_up : t -> (t, error) result
(** Go up one level *)

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

val dot_of_cursor_ref : (t -> string) ref
(** Placeholder of Graphviz rendering of cursors *)
   
(** Tools to create Not_Indexed and Not_Hashed nodes *)
module NotHashed : sig
  val leaf : Value.t -> node
  val extend : Segment.t -> node -> node
  val bud : node option -> node
  val internal : node -> node -> indexing_rule -> node
end

val stat : t -> Stat.t
