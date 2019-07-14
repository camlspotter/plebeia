open Types

type t
  
val create : string -> t
val open_ : string -> t
val close : t -> unit
val find : t -> Value.t -> (Index.t option, error) result
val add : t -> Value.t -> Index.t -> (unit, error) result
