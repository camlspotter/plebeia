module type S = sig

  module Index : sig
    type t = Stdint.uint32 (* Position on the data file *)
  end
    
  (** Module manipulating patricia trees and persisting them to disk *)
  module Context : sig
    type t
    (** A context represents the storage of a collection of trees sharing
        nodes on disk. *)
  end
  
  module Hash : sig
    type t
    (** Root hash of a tree. *)
  end

  (** A segment represents a path from the root of a tree to a leaf or
      to the root of a sub-tree. *)
  module Segment : sig
    type side = Left | Right
    type t = side list
  end

  (** Human readable directory names *)
  module Key : sig
    type t = string
    val to_segments : t -> (Segment.t list, string) result
    val of_segments : Segment.t list -> (t, string) result
  end

  (** Encoding of segments in nodes *)
  module Segment_encoding : sig
    val encode : Segment.t -> string
    val decode : string -> Segment.t
  end

  type error = string

  (** Stored value *)
  module Value : sig
    type t
      
    val of_string : string -> t
    val to_string : t -> string
  end
  
  val open_ : ?pos: int64 -> ?shared: bool -> ?kvs: KVS.t -> string -> Context.t
  (** Opens or creates a new context backed up at a given location
      in the filesystem. *)

  val close : Context.t -> unit
    
  val gc: src:Context.t -> Hash.t list -> dest:Context.t -> unit
  (** Copies from the src context trees rooted in the hash list
      into a new context. Used for garbage collection. *)
  
  module Cursor : sig
    type t
    (** Cursor in a tree to efficiently search and edit sub-trees. *)
  
    val empty : Context.t -> t
    (** Creates a cursor to a new, empty tree. *)
    
    val subtree : t -> Segment.t -> (t, error) result
    (** Moves the cursor down a segment, to the root of a sub-tree. Think
        "cd segment/" *)
    
    val create_subtree: t -> Segment.t -> (t, error) result
    (** Create a subtree (bud). Think "mkdir segment" *)
    
    val subtree_or_create : t -> Segment.t -> (t, error) result
    (** Same as subtree but create a subtree if not exists *)
    
    val parent : t -> (t, error) result
    (** Moves the cursor back to the parent tree. Think "cd .." *)

    val go_top : t -> (t, error) result
        
    val get : t -> Segment.t -> (Value.t, error) result
    (** Gets a value if present in the current tree at the given
        segment. *)
    
    val insert: t -> Segment.t -> Value.t -> (t, error) result
    (** Inserts a value at the given segment in the current tree.
        Returns the new cursor if successful. *)
    
    val upsert: t -> Segment.t -> Value.t -> (t, error) result
    (** Upserts. This can still fail if the segment leads to a subtree. *)
    
    val delete: t -> Segment.t -> (t, error) result
    (** Delete a leaf or subtree. *)
    
    val snapshot: t -> Segment.t -> Segment.t -> (t, error) result
    (** Snapshots a subtree at segment and place a soft link to it at
        another segment location. *)
  end

  val commit: Cursor.t -> (Cursor.t * Index.t * Hash.t, error) result
  (** Commits the change made in a cursor to disk. 
      The cursor must point to a root. 
      Returns the updated cursor and its new root hash. *)

  val hash: Cursor.t -> (Cursor.t * Hash.t)
  (** Computes the hash of the cursor without committing. *)
                        
  module Roots : sig
    (* Implementation of the root table.
       
       All the data should be in memory.
       Very simple append only format on disk.
    *)
    
    type t
    (** Storage type *)
      
    val create : string -> t
    (** Create a new root storage.  Note that if the file already exists, it is truncated. *)
    
    val open_ : string -> t
    (** Create a new, or open an exising root storage *)
    
    val close : t -> unit
    (** Close the root storage *)
      
    val add : t -> Hash.t -> Types.Index.t -> unit
    (** Add a root *)
    
    val find : t -> Hash.t -> Types.Index.t option
    (** Find a root of the given hash *)
    
    val remove : t -> Hash.t -> unit
    (** Remove a root.  If it does not exist, do nothing *)
  end

  (** Error/Result/Either monad *)
  module Error : module type of Error
end
