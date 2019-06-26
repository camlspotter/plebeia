type t = string

type kind =
  | Hex | Lower | Alphanum | Ascii | Binary

val to_segment : t -> Segment.t option
val of_segment : Segment.t -> t option
