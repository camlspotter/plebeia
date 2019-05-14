let failwithf fmt = Printf.ksprintf failwith fmt

let to_hex s =
  (* XXX not really fast I am afraid *)
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
