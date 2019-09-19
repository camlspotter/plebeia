open Node
open Cursor
    
val string_of_node : node -> int (* indent *) -> string
(** Dump a node *)

val dot_of_node : node -> string
(** Obtain Graphviz dot file representation of the tree *)

val dot_of_cursor : cursor -> string
(** Obtain Graphviz dot file representation of the cursor *)

val validate_node : Context.t -> node -> (unit, string) Result.t
(** Invariant checking of node *)

val save_cursor_to_dot : string -> cursor -> unit
val save_node_to_dot : string -> node -> unit
(** Graphviz visualization *)
