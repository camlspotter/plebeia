(** Implementation of the root table.
   
   All the data should be in memory.
   Very simple append only format on disk.
*)

type entry = 
  { index : Index.t
  ; parent : Index.t option
  ; meta1 : string
  ; meta2 : string
  }
  
type t = 
  { tbl : (Hash.t, entry) Hashtbl.t  (* all are in the memory *)
  ; context : Context.t
  }
(** Storage type *)

val create : Context.t -> t

val read_commits : t -> unit
  
val add : t -> ?parent: Index.t -> Hash.t -> Index.t -> meta1:string -> meta2:string -> unit
(** Add a root *)

val mem : t -> Hash.t -> bool

val find : t -> Hash.t -> entry option
(** Find a root of the given hash *)
