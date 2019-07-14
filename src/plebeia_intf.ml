module type S = sig

  module Index : sig
    type t = Stdint.uint32 (* Position on the data file *)
  end

  module Error : sig
    type t = string
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

    val to_string : t -> string
    val of_string : string -> t
  end

  (** A segment represents a path from the root of a tree to a leaf or
      to the root of a sub-tree. *)
  module Segment : sig
    type side = Left | Right
    type t = side list

    val to_string : t -> string 
    (** LLRRLL *)

    val of_string : string -> t option
    (** LLRRLL *)
  end

  (** Human readable directory names *)
  module Key : sig
    type t = string
    val to_segments : t -> (Segment.t list, string) Result.t
    val of_segments : Segment.t list -> (t, string) Result.t
  end

  (** Encoding of segments in nodes *)
  module Segment_encoding : sig
    val encode : Segment.t -> string
    val decode : string -> Segment.t
  end

  (** Stored value *)
  module Value : sig
    type t
      
    val of_string : string -> t
    val to_string : t -> string
  end
  
  val create : ?pos: int64 -> ?length: int -> string -> Context.t

  val open_ : ?pos: int64 -> ?shared: bool -> string -> Context.t
  (** Opens or creates a new context backed up at a given location
      in the filesystem. *)

  val close : Context.t -> unit
    
  val gc: src:Context.t -> Hash.t list -> dest:Context.t -> unit
  (** Copies from the src context trees rooted in the hash list
      into a new context. Used for garbage collection. *)
  
  module Stat : sig
    type t
    val create : unit -> t
    val pp : Format.formatter -> t -> unit
  end

  module Cursor : sig
    type t
    (** Cursor in a tree to efficiently search and edit sub-trees. *)
  
    val empty : Context.t -> t
    (** Creates a cursor to a new, empty tree. *)
    
    val subtree : t -> Segment.t -> (t, Error.t) Result.t
    (** Moves the cursor down a segment, to the root of a sub-tree. Think
        "cd segment/" *)
    
    val create_subtree: t -> Segment.t -> (t, Error.t) Result.t
    (** Create a subtree (bud). Think "mkdir segment" *)
    
    val subtree_or_create : t -> Segment.t -> (t, Error.t) Result.t
    (** Same as subtree but create a subtree if not exists *)
    
    type view 

    (** Result of access_gen *)
    type access_result =
      | Empty_bud (* The bud is empty *)
      | Collide of t * view (* The segment was blocked by an existing leaf or bud *)
      | Middle_of_extender of t * Segment.t * Segment.t * Segment.t (* The segment ends or deeprges at the middle of an Extender with the common prefix, the remaining extender, and the rest of segment *)
      | Reached of t * view (* just reached to a node *)
    
    val error_access : access_result -> ('a, Error.t) Result.t
    (** Make an access result into an error *)
    
    val access_gen : t -> Segment.t -> (access_result, Error.t) Result.t
    (** Follow a segment *)

    val go_up_to_a_bud : t -> (t, Error.t) Result.t
    (** Moves the cursor back to the bud above.  
        Note that this is not like "cd ../".  If the cursor is already 
        at a bud, it does not move it.
    *)

    val go_top : t -> (t, Error.t) Result.t
        
    val get : t -> Segment.t -> (Value.t, Error.t) Result.t
    (** Gets a value if present in the current tree at the given
        segment. *)
    
    val insert: t -> Segment.t -> Value.t -> (t, Error.t) Result.t
    (** Inserts a value at the given segment in the current tree.
        Returns the new cursor if successful. *)
    
    val upsert: t -> Segment.t -> Value.t -> (t, Error.t) Result.t
    (** Upserts. This can still fail if the segment leads to a subtree. *)
    
    val update: t -> Segment.t -> Value.t -> (t, Error.t) Result.t
    (** Update. A value must be bound at the segment. *)

    val delete: t -> Segment.t -> (t, Error.t) Result.t
    (** Delete a leaf or subtree. *)
    
(*
    val snapshot: t -> Segment.t -> Segment.t -> (t, error) Result.t
    (** Snapshots a subtree at segment and place a soft link to it at
        another segment location. 
    
        Not implemented
    *)
*)
        
    val stat : t -> Stat.t
  end

  module Deep : sig
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

    val deep_ro : 
      Cursor.t
      -> Segment.t list 
      -> (Cursor.t -> Segment.t -> ('a, Error.t) Result.t) 
      -> ('a, Error.t) Result.t
    (** Simplified version of [deep] for read only operations *)
    
    val deep_get : Cursor.t -> Segment.t list -> (Value.t, Error.t) Result.t

    val deep_upsert : Cursor.t -> Segment.t list -> Value.t -> (Cursor.t, Error.t) Result.t
    
    val deep_delete : Cursor.t -> Segment.t list -> (Cursor.t, Error.t) Result.t

    val deep_create_subtree : Cursor.t -> Segment.t list -> (Cursor.t, Error.t) Result.t

    val copy : 
      create_subtrees: bool 
      -> Cursor.t -> Segment.t list -> Segment.t list -> (Cursor.t, Error.t) Result.t
    (** Subtree copy by making two nodes point to the same subtree. 
        
        Copy attempts which introduce loops are rejected. 
    *)
  end
    
  val commit: Cursor.t -> (Cursor.t * Index.t * Hash.t, Error.t) Result.t
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
      
    val add : t -> ?parent:Hash.t -> Hash.t -> Index.t -> unit
    (** Add a root *)
    
    val find : t -> Hash.t -> (Index.t * Hash.t option) option
    (** Find a root of the given hash *)
    
    val remove : t -> Hash.t -> unit
    (** Remove a root.  If it does not exist, do nothing *)
  end

  val commit' : Roots.t -> Cursor.t -> (Cursor.t * Index.t * Hash.t, Error.t) Result.t
  (** Same as [commit] but also register the hash to the [Roots.t] *)

  val checkout : Roots.t -> Context.t -> Hash.t -> Cursor.t option

  (** Result monad *)
  module Result : module type of Result
end
