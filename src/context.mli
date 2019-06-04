(** 
   Sparse merkle tree storage
*)

type t = {
  mtree : Mtree.t ; 
  (* sparse Merkle tree *)

  roots : Roots.t ;
  (* Persisitent DB of root hashes to indices in the array. *)
}

val make : ?pos:int64 -> ?shared:bool -> ?kvs:KVS.t -> ?length:int -> string -> t

val open_ : ?pos:int64 -> ?shared:bool -> ?kvs:KVS.t -> string -> t

val close : t -> unit
