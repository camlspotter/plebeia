module type S = sig

  module Index : sig
    type t (* Position on the data file *)
  end
    
  (** Module manipulating patricia trees and persisting them to disk *)
  
  module Context : sig
    type t
  end
  (** A context represents the storage of a collection of trees sharing
      nodes on disk. *)
  
  module Hash : sig
    type hash56
  end
  (** Root hash of a tree. *)
  
  type cursor
  (** Cursor in a tree to efficiently search and edit sub-trees. *)
  
  module Segment : sig
    type side = Left | Right
    type t = side list
  end
  (** A segment represents a path from the root of a tree to a leaf or
      to the root of a sub-tree. *)

  module Segment_encoding : sig
    val encode : Segment.t -> string
    val decode : string -> Segment.t
  end

  type error

  module Value : sig
    type t
  end
  
  val open_ : ?pos: int64 -> ?shared: bool -> ?kvs: KVS.t -> string -> Context.t
  (** Opens or creates a new context backed up at a given location
      in the filesystem. *)

  val close : Context.t -> unit
    
  val gc: src:Context.t -> Hash.hash56 list -> dest:Context.t -> unit
  (** Copies from the src context trees rooted in the hash list
      into a new context. Used for garbage collection. *)
  
  module Cursor : sig
(*
    val root : context -> Hash.hash56 -> (cursor, error) result
    (** Gets the root cursor corresponding to a given root hash in the
        context. *)
*)
  
    val empty : Context.t -> cursor
    (** Creates a cursor to a new, empty tree. *)
    
    val subtree : cursor -> Segment.t -> (cursor, error) result
    (** Moves the cursor down a segment, to the root of a sub-tree. Think
        "cd segment/" *)
    
    val create_subtree: cursor -> Segment.t -> (cursor, error) result
    (** Create a subtree (bud). Think "mkdir segment" *)
    
    val parent : cursor -> (cursor, error) result
    (** Moves the cursor back to the parent tree. Think "cd .." *)
    
    val get : cursor -> Segment.t -> (Value.t, error) result
    (** Gets a value if present in the current tree at the given
        segment. *)
    
    val insert: cursor -> Segment.t -> Value.t -> (cursor, error) result
    (** Inserts a value at the given segment in the current tree.
        Returns the new cursor if successful. *)
    
    val upsert: cursor -> Segment.t -> Value.t -> (cursor, error) result
    (** Upserts. This can still fail if the segment leads to a subtree. *)
    
    val delete: cursor -> Segment.t -> (cursor, error) result
    (** Delete a leaf or subtree. *)
    
    val snapshot: cursor -> Segment.t -> Segment.t -> (cursor, error) result
    (** Snapshots a subtree at segment and place a soft link to it at
        another segment location. *)
  end

  val commit: cursor -> (cursor * Index.t * Hash.hash56, error) result
  (** Commits the change made in a cursor to disk. 
      The cursor must point to a root. 
      Returns the updated cursor and its new root hash. 

(* XXX It must be implemented outside 
      If the same root hash [h] is already bound to an index [i'] 
      in the roots table, the function returns an [Error (c, i, h, i')],
      WITHOUT adding the index [i] new change to the roots table.
      Even in this case, the changes are stored in the node storage.
*)
  *)
  
  val hash: cursor -> (cursor * Hash.hash56)
  (** Computes the hash of the cursor without committing. *)
end
