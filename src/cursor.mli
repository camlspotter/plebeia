open Types
open Node

val root : Context.t -> hash -> (cursor, error) result
(** Gets the root cursor corresponding to a given root hash in the
    context. *)

val empty : Context.t -> cursor
(** Creates a cursor to a new, empty tree. *)

val subtree : cursor -> segment -> (cursor, error) result
(** Moves the cursor down a segment, to the root of a sub-tree. Think
    "cd segment/" *)

val create_subtree: cursor -> segment -> (cursor, error) result
(** Create a subtree (bud). Think "mkdir segment" *)

val go_top : cursor -> (cursor, error) result

val parent : cursor -> (cursor, error) result
(** Moves the cursor back to the parent tree. Think "cd .." *)

val get : cursor -> segment -> (value, error) result
(** Gets a value if present in the current tree at the given
    segment. *)

val insert: cursor -> segment -> value -> (cursor, error) result
(** Inserts a value at the given segment in the current tree.
    Returns the new cursor if successful. *)

val upsert: cursor -> segment -> value -> (cursor, error) result
(** Upserts. This can still fail if the segment leads to a subtree. *)

val delete: cursor -> segment -> (cursor, error) result
(** Delete a leaf or subtree. *)

val snapshot: cursor -> segment -> segment -> (cursor, error) result
(** Snapshots a subtree at segment and place a soft link to it at
    another segment location. *)

val go_below_bud : cursor -> (cursor option, error) result
(** This function expects a cursor positionned on a bud 
    and moves it one step below. *)

val go_down_extender : cursor -> (cursor, error) result
val go_side : Path.side -> cursor -> (cursor, error) result
val go_up : cursor -> (cursor, error) result
val access_gen : cursor -> segment -> (cursor * (segment * segment * segment) option, error) result

type where_from =
  | From_above of dir
  | From_below of dir

and dir =
  | Left
  | Right
  | Center

type position = 
  where_from list (* Where the traversal comes from *)
  * cursor (* The cursor *)
  
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

val dot_of_cursor_ref : (cursor -> string) ref

(** Tools to create Not_Indexed and Not_Hashed nodes *)
module NotHashed : sig
  val leaf : Value.t -> node
  val extend : Path.segment -> node -> node
  val bud : node option -> node
  val internal : node -> node -> indexing_rule -> node
end
