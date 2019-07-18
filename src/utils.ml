let failwithf fmt = Printf.ksprintf failwith fmt

let to_hex s =
  (* XXX not really fast I am afraid 
     XXX We must use hex package.
  *)
  let buf = Buffer.create (String.length s * 2) in
  String.iter (fun c ->
      Buffer.add_string buf @@ Printf.sprintf "%02x" @@ Char.code c) s;
  Buffer.contents buf

let from_Some = function
  | Some x -> x
  | None -> assert false

let to_file fn s =
  let oc = open_out fn in
  output_string oc s;
  close_out oc

module Exn = struct
  let catch f a = match f a with
    | exception e -> Error (`Exn e)
    | x -> Ok x
  
  let protect f fin = 
    match f () with
    | exception e -> fin (); raise e
    | x -> fin (); x
end

(* XXX to be removed *)
module Cstruct = struct
  open Stdint

  include Cstruct
  include Cstruct.LE (* Intel friendly *)

  (* The original [uint32] functions of Cstruct returns **[int32]**.
     Very confusing, so we patch them here. *)
  let get_uint32 buf x = Uint32.of_int32 @@ Cstruct.LE.get_uint32 buf x
  let set_uint32 buf x v = Cstruct.LE.set_uint32 buf x @@ Uint32.to_int32 v
  let get_index buf x = Index.of_uint32 @@ get_uint32 buf x
  let set_index buf x v = set_uint32 buf x @@ Index.to_uint32 v
end

module Option = struct
  let default d = function
    | None -> d
    | Some v -> v
end
