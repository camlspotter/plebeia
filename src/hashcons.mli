type t
  
val create : Storage.t -> t
(** Create an empty hashcons table.  To load the existing hashconsed
    table from the storage, [read] must be used.
*)

val read : t -> load_leaf_value:(Index.t -> Value.t option) -> unit
(** Load the hasheconsed values from the storage *)
   
val find : t -> Value.t -> (Index.t option, Error.t) Result.t
(** Find the hasheconsed value from the table *)

val add : t -> Value.t -> Index.t -> (unit, Error.t) Result.t
(** Register a value from the table *)

val stat : t -> unit
(** Print out statistics to stderr *)
