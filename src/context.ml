open Stdint
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

let make ?pos ?(shared=false) ?(length=(-1)) ?(use_kvs=false) fn =
  let fd = Unix.openfile fn [O_RDWR] 0o644 in
  let array =
    (* length = -1 means that the size of [fn] determines the size of
       [array]. This is almost certainly NOT what we want. Rather the array
       should be made x% bigger than the file (say x ~ 25%). When the array
       is close to being full, the file should be closed and reopened with
       a bigger length.
    *) (* FIXME *)
    let open Bigarray in
    array1_of_genarray @@ Unix.map_file fd ?pos
      char c_layout shared [| length |] in
  { array ;
    length = Uint32.zero ;
    leaf_table = KVS.make () ;
    store_in_leaf_table = use_kvs;
    roots_table = Hashtbl.create 1 ;
    fd = fd ;
  }

let new_index c =
  (* XXX check of size *)
  let i = Uint32.succ c.length in
  c.length <- i;
  i

let new_indices c n =
  (* XXX check of size *)
  assert (n > 0);
  let i = Uint32.succ c.length in
  c.length <- Index.(c.length + of_int n);
  i

let free { fd ; _ } = Unix.close fd

