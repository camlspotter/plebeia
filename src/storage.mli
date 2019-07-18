type storage
type t = storage

val get_last_checkpoint_index : t -> Index.t option
val get_last_root_index       : t -> Index.t option
val get_last_cache_index      : t -> Index.t option

val set_last_checkpoint_index : t -> Index.t option -> unit
val set_last_root_index       : t -> Index.t option -> unit
val set_last_cache_index      : t -> Index.t option -> unit

module Checkpoint : sig
  type t

  val check : storage -> unit
  (** last_xxx_index's are written to the storage as a checkpoint
      only by an explicit call of [check].  If program crashes, anything 
      after the last checkpoint will be lost.
  *)
end

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
