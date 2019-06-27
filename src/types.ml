open Stdint

type error = string
(** Error type.  Currently, simply error messages *)

module Index = Uint32
(** Position in the storage *)  
(* XXX In 64bit arch, we can simply use unboxed int *)

