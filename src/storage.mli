open Types
open Node

exception LoadFailure of error
val parse_cell : Context.t -> Index.t -> view
val write : Context.t -> view -> unit

val write_small_leaf : Context.t -> Value.t -> unit
val write_large_leaf_to_kvs : Context.t -> Hash.hash28 -> Value.t -> unit
val write_large_leaf_to_plebeia : Context.t -> Value.t -> unit

module Chunk : sig
  (* XXX move to tests *)
  val test_write_read : Random.State.t -> Context.t -> unit
end

val commit_node : Context.t -> node -> node * index * hash

