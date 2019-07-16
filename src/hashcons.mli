type t
  
val create : string -> t
val open_ : string -> t
val close : t -> unit
val find : t -> Value.t -> (Index.t option, Error.t) Result.t
val add : t -> Value.t -> Index.t -> (unit, Error.t) Result.t
