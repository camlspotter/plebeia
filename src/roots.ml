(* Implementation of the root table.
   
   All the data should be in memory.
   Very simple append only format on disk.
*)

type t = 
  { tbl : (Types.hash, Types.index) Hashtbl.t  (* all are in the memory *)
  ; fd : Unix.file_descr
  }

let create path =
  let open Unix in
  if Sys.file_exists path then failwith "roots file already exists"
  else
    let fd = openfile path [ O_RDWR; O_CREAT; O_TRUNC ] 0o644 in
    { tbl = Hashtbl.create 1001; fd }

let read_commit fd =
  let open Unix in
  let buf = Bytes.create 60 in
  let r = read fd buf 0 60 in
  if r <> 60 then failwith (string_of_int r) (* XXX *)
  (* XXX garbage may exist, if write_commit fails. we have to ignore them *)
  else
    (Hash.hash56_of_string @@ Bytes.sub_string buf 0 56,
     let cstr = Cstruct.of_bytes buf in
     Stdint.Uint32.of_int32 @@ Cstruct.LE.get_uint32 cstr 56)
    
let write_commit fd hash index =
  let open Unix in
  let hash = (hash : Hash.hash56 :> string) in
  let w = single_write_substring fd hash 0 56 in (* XXX must be done with the index *)
  if w <> 56 then failwith (string_of_int w) (* XXX *)
  else 
    let buf = Bytes.create 4 in
    let cstr = Cstruct.of_bytes buf in
    Cstruct.LE.set_uint32 cstr 0 @@ Stdint.Uint32.to_int32 index;
    let w = write fd buf 0 4 in
    if w <> 4 then failwith (string_of_int w) (* XXX *)

let open_ path =
  let open Unix in
  let fd = openfile path [ O_RDWR ] 0o644 in
  let tbl = Hashtbl.create 1001 in
  let rec loop () =
    match read_commit fd with
    | exception (Failure x) when x = "0" -> { tbl; fd }
    | exception (Failure _) ->
        (* XXX garbate found, overwrite from here. 
           XXX we have to rewind to the point of the last valid read
        *)
        { tbl; fd }
    | (h,i) -> 
        Hashtbl.replace tbl h i; (* XXX overwrite should be warned *)
        loop ()
  in
  loop ()

let add { tbl; fd } hash index =
  Hashtbl.replace tbl hash index; (* XXX overwrite should be warned *)
  write_commit fd hash index

let find { tbl ; _ } = Hashtbl.find_opt tbl

let close { fd ; _ } = Unix.close fd
    
