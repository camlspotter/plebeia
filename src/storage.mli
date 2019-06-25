open Types
open Node

exception LoadFailure of error

val parse_cell : Context.t -> Index.t -> view

module Chunk : sig
  (* XXX move to tests *)
  val test_write_read : Random.State.t -> Context.t -> unit
end

val commit_node : Context.t -> node -> node * index * Hash.hash56

val load_node : Context.t -> Index.t -> extender_witness -> view
(** Read the node from context.array, parse it and create a view node with it. *)

val load_node_fully : Context.t -> node -> node
(** Recusively visit and load all the subnodes in memory.
   Only for test purposes
*)
