(** 
   Sparse merkle tree storage
*)

type t = {
  storage : Storage.t ;

  hashcons : Hashcons.t ;
  (* Hashcons tbl *)

  stat : Stat.t ;
  (* Statistics *)
}

val create : 
  ?pos:int64 
  -> ?length:int 
  -> hashcons: Hashcons.t
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
  -> hashcons: Hashcons.t
  -> string (* path *)
  -> t
(** Open an existing context storage.

    pos: the start position in the file
    shared: if false, read only.
*)

val close : t -> unit
