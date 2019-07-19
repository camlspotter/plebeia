(** Implementation of the root table.
   
   All the data should be in memory.
   Very simple append only format on disk.
*)

type t = 
  { tbl : (Hash.t, (Index.t * Index.t option * string)) Hashtbl.t  (* all are in the memory *)
  ; context : Context.t
  }
(** Storage type *)

val create : Context.t -> t

val read_commits : t -> unit
  
val add : t -> ?parent: Index.t -> Hash.t -> Index.t -> string -> unit
(** Add a root *)

val mem : t -> Hash.t -> bool

val find : t -> Hash.t -> (Index.t * Index.t option * string) option
(** Find a root of the given hash *)
