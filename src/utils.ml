let failwithf fmt = Printf.ksprintf failwith fmt

let to_hex s =
  (* XXX not really fast I am afraid *)
  let buf = Buffer.create (String.length s * 2) in
  String.iter (fun c ->
      Buffer.add_string buf @@ Printf.sprintf "%02x" @@ Char.code c) s;
  Buffer.contents buf
