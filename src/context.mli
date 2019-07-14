(** 
   Sparse merkle tree storage
*)

type t

val create : 
  ?pos:int64 
  -> ?length:int 
  -> string (* path *)
  -> t
(** Create a new context storage.  
    Note that if the file already exists, the contents are cleaned away. 

    pos: the start position in the file
    length: initial size of the file in bytes
*) 

val open_ : 
  ?pos:int64 
  -> ?shared:bool 
  -> string (* path *)
  -> t
(** Open an existing context storage.

    pos: the start position in the file
    shared: if false, read only.
*)

val stat : t -> Stat.t

val close : t -> unit

val new_index : t -> Stdint.Uint32.t
(** Allocate 1 cell and return its index *)

val new_indices : t -> int -> Stdint.Uint32.t
(** Allocate the give number of contiguous cells and return the first index *)

val get_cell : t -> Index.t -> Cstruct.t
(** Map the cell of the given index to Cstruct.t *)
   
val get_bytes : t -> Index.t -> int -> Cstruct.t
(** Map the bytes starting from the cell of the given index to Cstruct.t *)
