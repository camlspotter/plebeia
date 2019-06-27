(** Segment encoding in storage *)
           
(*
   |<-------- 224 bits = 28 bytes ------->|
   |0*1|<- segment bits upto 222 bits ->|1|
*)

val encode : Segment.t -> string
val decode : string -> Segment.t
