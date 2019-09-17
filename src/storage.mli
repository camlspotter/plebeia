(** Storage system *)

type storage
type t = storage

val create : ?pos:int64 -> ?length:int -> string -> t
(** Create a new storage *)

val open_ : ?pos:int64 -> ?shared:bool -> string -> t
(** Open an existing storage *)

val close : t -> unit
(** Close the storage *)

module Header : sig
  val commit : t -> unit
  (** Write the current state of the storage to the header.
      Any updates to the storage after the last commit will be lost
      if the system crashes, even if they are written to a file.
  *)
end

val get_last_root_index  : t -> Index.t option
val get_last_cache_index : t -> Index.t option
val get_current_length   : t -> Index.t
(** Get the status of the storage *)

val set_last_root_index  : t -> Index.t option -> unit
val set_last_cache_index : t -> Index.t option -> unit
(** Set the last indices of root hash and hashcons cache.
    Note that the data are only saved to the file when [Header.commit]
    is called. 
*)

val get_cell : t -> Index.t -> Cstruct.t
(** Get the content of the cell specified by the index *)

val get_bytes : t -> Index.t -> int -> Cstruct.t
(** Get the contiguous bytes from the head of the index *)

val new_index : t -> Index.t
(** Allocate a cell and returns its index *)

val new_indices : t -> int -> Index.t
(** Allocate cells and return the first index *)

val make_buf : t -> Index.t -> Cstruct.t
(** make a writable buffer of the cell of the given index *)

val make_buf2 : t -> Index.t -> Cstruct.t      
(** make a 64 bytes writable buffer from the beginning of 
    the cell of the given index *)

module Chunk : sig
  (** Bigger data than a cell *)

  val read : t -> Index.t -> string
  (** Read a big data which is stored from the specified index *)

  val write : t -> ?max_cells_per_chunk:int -> string -> Index.t
  (** Write a big data and returns the index *)
                                                           
  val test_write_read : Random.State.t -> t -> unit
  (** A test function *)
end
