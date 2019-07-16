(** Segment encoding in storage *)
           
(*
   |<-------- 224 bits = 28 bytes ------->|
   |0*1|<- segment bits upto 222 bits ->|1|
*)

val encode : Segment.t -> string
(** The length of the segment must be <= 222.  Otherwise the funciton fails. *)

val decode : string -> Segment.t
