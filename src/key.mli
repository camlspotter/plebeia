type t = string

type kind =
  | Hex | Lower | Alphanum | Ascii | Binary

val to_segments : t -> (Segment.t list, string) result
val of_segments : Segment.t list -> (string, string) result

