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

module Option = struct
  let default d = function
    | None -> d
    | Some v -> v
end
