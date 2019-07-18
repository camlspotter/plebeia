type t
  
val create : Storage.t -> t
val read : t -> load_leaf_value:(Index.t -> Value.t option) -> unit
val find : t -> Value.t -> (Index.t option, Error.t) Result.t
val add : t -> Value.t -> Index.t -> (unit, Error.t) Result.t
