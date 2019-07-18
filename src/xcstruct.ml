open Stdint

include Cstruct
include Cstruct.LE (* Intel friendly *)

(* The original [uint32] functions of Cstruct returns **[int32]**.
   Very confusing, so we patch them here. *)
let get_uint32 buf x = Uint32.of_int32 @@ Cstruct.LE.get_uint32 buf x
let set_uint32 buf x v = Cstruct.LE.set_uint32 buf x @@ Uint32.to_int32 v
let get_index buf x = Index.of_uint32 @@ get_uint32 buf x
let set_index buf x v = set_uint32 buf x @@ Index.to_uint32 v
let get_hash buf pos = Hash.of_string @@ copy buf pos (pos+28)

(* Cstruct.blit_from_string, but make sure all the string contents are written *)
let write_string s buf off len =
  let slen = String.length s in
  if slen <> len then begin Format.eprintf "write_string: %d <> %d@." slen len; assert false end;
  blit_from_string s 0 buf off len


