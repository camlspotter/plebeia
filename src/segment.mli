(** A module encapsulating the concept of a path through the Patricia tree.
  A path is a sequence of n full segments. The n-1 first segments end in
  a bud and the nth ends in a leaf. Internal segments are bit of paths
  encoded in the internal nodes of the Patricia tree while tip segments
  represent the bits of path encoded close to the leaf. *)

type side = Left | Right
(** Binary tree, by convention when a bool or a bit is used, 0 = false = Left
  and 1 = true = Right *)

val string_of_side : side -> string
(* L or R *)
  
val dummy_side : side
(** Whenever we need to consistently pick side that doesn't really correspond
  to a real side. By convention, it is Left. *)

type segment = side list
type t = segment
(** A segment is always just a list of sides. TODO: use bit arithmetic for
  speed and memory footprint.*)

val empty : segment
(** The empty segment. *)

val is_empty : segment -> bool

val cut : segment -> (side * segment) option
(** Cuts a path into a head and tail if not empty. *)

val common_prefix : segment -> segment -> segment * segment * segment
(** Factors a common prefix between two segments. *)

val of_side_list : side list -> segment
(** Converts a list of side to a segment. *)

val to_string : segment -> string
(** String representation of a segment, e.g. "LLLRLLRL" *)

val of_string : string -> segment option
(** Parse the string representation of a segment, e.g. "LLRLLRL" *)

val concat : segment -> segment -> segment
