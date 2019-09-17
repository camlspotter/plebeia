(** Storage system *)

type storage
type t = storage

module Header : sig
  val commit : t -> unit
  (** Write the current state of the storage to the header.
      Any updates to the storage after the last commit will be lost
      if the system crashes, even if they are written to a file.
  *)
end

val get_last_root_index       : t -> Index.t option
val get_last_cache_index      : t -> Index.t option

val set_last_root_index       : t -> Index.t option -> unit
val set_last_cache_index      : t -> Index.t option -> unit

val get_current_length : t -> Index.t

val get_cell : t -> Index.t -> Cstruct.t
val get_bytes : t -> Index.t -> int -> Cstruct.t

val create : ?pos:int64 -> ?length:int -> string -> t
val open_ : ?pos:int64 -> ?shared:bool -> string -> t
val close : t -> unit

val may_resize : Index.t -> t -> unit

val new_index : t -> Index.t
val new_indices : t -> int -> Index.t

val make_buf : t -> Index.t -> Cstruct.t
val make_buf2 : t -> Index.t -> Cstruct.t      

module Chunk : sig
  val read : t -> Index.t -> string
  val write : t -> ?max_cells_per_chunk:int -> string -> Index.t
                                                           
  val test_write_read : Random.State.t -> t -> unit
end
