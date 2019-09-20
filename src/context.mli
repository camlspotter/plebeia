(** 
   Sparse merkle tree storage
*)

type t = {
  storage : Storage.t ;
  hashcons : Hashcons.t ; (* Hashcons tbl *)
  stat : Stat.t ;         (* Statistics *)
}

val create : 
  ?pos:int64 
  -> ?length:int 
  -> string (* path *)
  -> t
(** Create a new context storage.  
    Note that if the file already exists, [create] fails.

    pos: the start position in the file
    length: initial size of the file in bytes
*) 

val open_ : 
  ?pos:int64 
  -> ?shared:bool 
  -> ?load_hashcons:bool
  -> string (* path *)
  -> t
(** Open an existing context storage.

    pos: The start position in the file
    shared: If false, read only.  Default: false.
    load_hashcons: If false, do not load the small value cache.  Default: true.
*)

val close : t -> unit
(** Closes the context.  
                         
    If program exits or crashes without closing a context, some data may be lost 
    even if they are written on the disk.
*)    
    
val ref_load_leaf_value : (t -> Index.t -> Value.t option) ref
(* Forward declaration to load a leaf value from context *)
