(* Many small leaves share the same value, therefore sharing the nodes
   can reduce the storage usage tremendously.
*)

type t = 
  { tbl : (int * Value.t, Index.t) Hashtbl.t
  ; fd : Unix.file_descr
  }
      
let create path =
  let open Unix in
  let fd = openfile path [ O_RDWR; O_CREAT; O_TRUNC ] 0o644 in
  { tbl = Hashtbl.create 1001; fd }

let read_entry fd =
  let open Unix in
  let buf = Bytes.create 44 in (* 36 + 4 + 4 *)
  let r = read fd buf 0 44 in
  if r = 0 then None (* EOF *)
  else if r <> 44 then Utils.failwithf "Hashcons.read_entry: garbage of %d bytes found" r
  else begin
    let cstr = Cstruct.of_bytes buf in
    let size = Stdint.Uint32.to_int @@ Utils.Cstruct.get_uint32 cstr 0 in
    let index = Utils.Cstruct.get_uint32 cstr 4 in
    let v = Value.of_string @@ Bytes.sub_string buf 8 size in
    Some (size, v, index)
  end

let write_entry fd v index =
  let open Unix in
  let v = Value.to_string v in
  let size = String.length v in
  assert (size <= 36);
  let buf = Bytes.create 44 in
  Bytes.blit_string v 0 buf 8 size;
  let cstr = Cstruct.create 8 in
  Utils.Cstruct.set_uint32 cstr 0 (Stdint.Uint32.of_int size);
  Utils.Cstruct.set_uint32 cstr 4 index;
  Cstruct.blit_to_bytes cstr 0 buf 0 8;
  let w = single_write fd buf 0 44 in
  if w <> 44 then Utils.failwithf "Hashcons.write_entry: failed (%d bytes written)" w

let open_ path =
  let open Unix in
  let fd = openfile path [ O_RDWR ] 0o644 in
  let tbl = Hashtbl.create 1001 in
  let rec loop () =
    match read_entry fd with
    | None -> { tbl; fd }
    | Some (size, v, index) ->
        Hashtbl.replace tbl (size, v) index;
        loop ()
  in
  loop ()

let exists = Sys.file_exists

let open_ path = if not @@ exists path then create path else open_ path
  
let find { tbl ; _ } v =
  let s = Value.to_string v in
  let len = String.length s in
  if len > 36 then Error "hashcons: too large"
  else
    Ok (Hashtbl.find_opt tbl (len, v))

let add { tbl ; fd } v index =
  let s = Value.to_string v in
  let len = String.length s in
  if len > 36 then Error "hashcons: too large"
  else 
    match Hashtbl.find_opt tbl (len, v) with
    | Some _ -> Error "hashcons: registered"
    | None ->
        Hashtbl.replace tbl (len, v) index;
        write_entry fd v index;
        Ok ()
  
let close { fd ; _ } = Unix.close fd

   
