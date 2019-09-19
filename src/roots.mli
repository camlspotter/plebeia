(**  

  Implementation of the root hash table.
   
  All the data should be in memory.

  The root hashes are stored in the context.  They are chained and the last 
  root hash entry is recorded in the header of the context.
    
  Note that the chain of root hashes in the context do not correspond with 
  the parent-children relationship of root hashes.

*)

type entry = 
  { index : Index.t
  ; parent : Index.t option
  ; meta1 : string (* Commit log (currently empty) *)
  ; meta2 : string (* To store Irmin's context hash *)
  ; hash : Hash.t
  }
  
type t
(** Storage type *)

val create : Context.t -> t
(** Load root hashes of the context and return t *)

val add : t -> ?parent: Index.t -> Hash.t -> Index.t -> meta1:string -> meta2:string -> unit
(** Add a new root hash, with its index, parent, meta1 commit log, and meta2 for Irmin contex hash.

    It immediately saves the root to the context.
*)

val mem : t -> Hash.t -> bool
(** Existence check *)

val find : t -> Hash.t -> entry option
(** Find a root of the given hash *)

val find_by_index : t -> Index.t -> entry option
(** Find by index *)

val genesis : t -> Hash.t list
(** Returns the hashes which have no parents *)

val children : t -> Index.t -> entry list

val fold : (entry -> 'acc -> 'acc) -> t -> 'acc -> 'acc

val length : t -> int

val to_seq : t -> entry Seq.t

