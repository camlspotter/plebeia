open Plebeia_impl

val string_of_tree : ex_node -> int (* indent *) -> string

val dot_of_tree : ex_node -> string
(** Obtain Graphviz dot file representation of the tree *)
