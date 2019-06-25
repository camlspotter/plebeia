(* Implementation of the root table.
   
   All the data should be in memory.
   Very simple append only format on disk.
*)

type t
(** Storage type *)
  
val create : string -> t
(** Create a new root storage.  Note that if the file already exists, it is truncated. *)

val open_ : string -> t
(** Create a new, or open an exising root storage *)

val close : t -> unit
(** Close the root storage *)
  
val add : t -> Hash.hash56 -> Types.index -> unit
(** Add a root *)

val find : t -> Hash.hash56 -> Types.index option
(** Find a root of the given hash *)

val remove : t -> Hash.hash56 -> unit
(** Remove a root.  If it does not exist, do nothing *)
