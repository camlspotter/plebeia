type storage
type t = storage

module Cstruct : sig
  include module type of struct include Cstruct end
  val get_index : Cstruct.t -> int -> Index.t
  val set_index : Cstruct.t -> int -> Index.t -> unit
  val get_hash : Cstruct.t -> int -> Hash.t
  val write_string : string -> Cstruct.t -> int -> int -> unit
end

module Header : sig
  type t = 
    { next_index : Index.t 
    ; last_root_index : Index.t option
    ; last_cache_index : Index.t option
    }

  val read : storage -> t
  val write : storage -> t -> unit

end

val get_cell : t -> Index.t -> Cstruct.t
val get_bytes : t -> Index.t -> int -> Cstruct.t

val create : ?pos:int64 -> ?length:int -> string -> t
val open_ : ?pos:int64 -> ?shared:bool -> string -> t
val close : t -> unit

val may_resize : Index.t -> t -> unit

val set_current_length : t -> Index.t -> unit
val read_last_commit_index : t -> Index.t option
val write_last_commit_index : t -> Index.t option -> unit

val new_index : t -> Index.t
val new_indices : t -> int -> Index.t

val make_buf : t -> Index.t -> Cstruct.t
val make_buf2 : t -> Index.t -> Cstruct.t      

module Chunk : sig
  val read : t -> Index.t -> string
  val write : t -> ?max_cells_per_chunk:int -> string -> Index.t
                                                           
  val test_write_read : Random.State.t -> t -> unit
end
