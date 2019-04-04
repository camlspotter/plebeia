open Node

val string_of_node : node -> int (* indent *) -> string

val dot_of_node : node -> string
(** Obtain Graphviz dot file representation of the tree *)

val dot_of_cursor : cursor -> string

val validate_node : Context.t -> node -> (unit, string) result
