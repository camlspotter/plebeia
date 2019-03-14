open Plebeia_impl
open Plebeia_impl.PrivateNode

val string_of_node : node -> int (* indent *) -> string

val dot_of_node : node -> string
(** Obtain Graphviz dot file representation of the tree *)

val dot_of_cursor : cursor -> (string, error) result

val validate_node : context -> node -> (unit, string) result
