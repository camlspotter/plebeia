type t = string
(** Human readable directory name *)

type kind =
  | Hex | Lower | Alphanum | Ascii | Binary
(** Keys are encoded in different ways depending on the character sets
    they have *)

val to_segments : t -> (Segment.t list, string) result
(** Very long keys may be encoded to multiple segments,
    since one segment can have 222 bits at most. *)

val of_segments : Segment.t list -> (string, string) result
