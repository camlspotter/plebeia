(* Implementation of the root table.
   
   All the data should be in memory.
   Very simple append only format on disk.
*)

type t
  
val create : string -> t
val open_ : string -> t
val close : t -> unit
val add : t -> Types.hash -> Types.index -> unit
val find : t -> Types.hash -> Types.index option

