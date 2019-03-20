module type S = sig

  (** Module manipulating patricia trees and persisting them to disk *)
  
  
  module Context : sig
    type t
    (** A context represents the storage of a collection of trees sharing
        nodes on disk. *)
  end
  
  type hash
  (** Root hash of a tree. *)
  
  type index = int
  (** Index in the storage.  Assuming 64bits arch *)
    
  type cursor
  (** Cursor in a tree to efficiently search and edit sub-trees. *)
  
  type segment
  (** A segment represents a path from the root of a tree to a leaf or
      to the root of a sub-tree. *)
  
  type error
  
  type value
  
  val open_context : filename:string -> Context.t
  (** Opens or creates a new context backed up at a given location
      in the filesystem. *)
  
  val gc: src:Context.t -> hash list -> dest:Context.t -> unit
  (** Copies from the src context trees rooted in the hash list
      into a new context. Used for garbage collection. *)
  
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
  
  val commit: cursor -> (cursor * hash, cursor * index * hash * index) result
  (** Commits the change made in a cursor to disk. 
      The cursor must point to a root. 
      Returns the updated cursor and its new root hash. 

      If the same root hash [h] is already bound to an index [i'] 
      in the roots table, the function returns an [Error (c, i, h, i')],
      WITHOUT adding the index [i] new change to the roots table.
      Even in this case, the changes are stored in the node storage.
  *)
  
  val hash: cursor -> (cursor * hash)
  (** Computes the hash of the cursor without committing. *)

end
