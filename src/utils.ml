let failwithf fmt = Printf.ksprintf failwith fmt

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

let xassert b =
  let open Printexc in
  if not b then begin
    prerr_endline ("*****************************************************\n" ^ raw_backtrace_to_string (get_callstack 10));
    assert false
  end
