(** Implementation of the root table.
   
   All the data should be in memory.
   Very simple append only format on disk.
*)

type t = 
  { tbl : (Hash.t, (Index.t * Index.t option)) Hashtbl.t  (* all are in the memory *)
  ; mutable last_commit_index : Index.t option
  ; storage : Storage.t (* == context.storage *)
  ; context : Context.t
  }
(** Storage type *)

val create : Context.t -> t

val read_commits : t -> unit
  
val add : t -> ?parent: Index.t -> Hash.t -> Index.t -> unit
(** Add a root *)

val mem : t -> Hash.t -> bool

val find : t -> Hash.t -> (Index.t * Index.t option) option
(** Find a root of the given hash *)
