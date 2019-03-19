(*       
   |<-------- 224 bits = 28 bytes ------->|
   |0*1|<- segment bits upto 222 bits ->|1|
*)

val encode : Path.segment -> string
val decode : string -> Path.segment
