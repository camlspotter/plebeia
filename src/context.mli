open Types

type t = {
  array : Bigstring.t ;
  (* mmaped array where the nodes are written and indexed. *)
  mutable length : Index.t ;
  (* Current length of the node table. *)
  leaf_table  : KVS.t ;
  (* Hash table  mapping leaf hashes to their values. *)
  store_in_leaf_table : bool ;
  (* If [false], all the values are stored in the tree *)
  roots_table : (Hash.hash56, Index.t) Hashtbl.t ;
  (* Hash table mapping root hashes to indices in the array. *)
  fd : Unix.file_descr ; 
  (* File descriptor to the mapped file *)
}

val make :
  ?pos:int64 -> ?shared:bool -> ?length:int -> ?use_kvs:bool -> string -> t
(** length = -1 means that the size of [fn] determines the size of
   [array]. This is almost certainly NOT what we want. Rather the array
   should be made x% bigger than the file (say x ~ 25%). When the array
   is close to being full, the file should be closed and reopened with
   a bigger length.
*) (* FIXME *)

val free : t -> unit

val new_index : t -> Stdint.Uint32.t

val new_indices : t -> int -> Stdint.Uint32.t
