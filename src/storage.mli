open Node

exception LoadFailure of Error.t

val parse_cell : Context.t -> Index.t -> view
(** Exposed for test *)

module Chunk : sig
  (* XXX move to tests *)
  val test_write_read : Random.State.t -> Context.t -> unit
end

val commit_node : Context.t -> node -> node * Index.t * Hash.t
(** Write a node to the storage, and returns the updated version 
    of the node with its index and hash *)
    
val load_node : Context.t -> Index.t -> extender_witness -> view
(** Read the node from context.array, parse it and create a view node with it. *)

val load_node_fully : Context.t -> node -> node
(** Recusively visit and load all the subnodes in memory.
   Only for test purposes
*)

type commit = 
  { commit_meta : string (* 20 bytes *)
  ; commit_prev : Index.t option
  ; commit_parent : Index.t option
  ; commit_index : Index.t
  }

val read_commit : Context.t -> Index.t -> commit
val write_commit : Context.t -> commit -> Index.t

val write_leaf : Context.t -> Value.t -> Node_hash.t -> view * Stdint.uint32 * Node_hash.t
