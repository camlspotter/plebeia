(** 
   Sparse merkle tree storage
*)

open Types

type t

val make : ?pos:int64 -> ?shared:bool -> ?kvs:KVS.t -> ?length:int -> string -> t

val open_ : ?pos:int64 -> ?shared:bool -> ?kvs:KVS.t -> string -> t

val kvs : t -> KVS.t option

val may_resize : Index.t -> t -> unit

val close : t -> unit

val new_index : t -> Stdint.Uint32.t

val new_indices : t -> int -> Stdint.Uint32.t

val get_cell : t -> Index.t -> Cstruct.t
                                 
val get_bytes : t -> Index.t -> int -> Cstruct.t
