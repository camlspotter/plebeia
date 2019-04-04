open Types
open Node

exception LoadFailure of error

val parse_cell : Context.t -> Index.t -> view

module Chunk : sig
  (* XXX move to tests *)
  val test_write_read : Random.State.t -> Context.t -> unit
end

val commit_node : Context.t -> node -> node * index * hash

