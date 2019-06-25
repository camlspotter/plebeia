(* Implementation of the root table.
   
   All the data should be in memory.
   Very simple append only format on disk.


   TODO: each add/remove accesses disk.  We can make the IO buffered.
*)
open Types

type t = 
  { tbl : (Hash.hash56, Index.t) Hashtbl.t  (* all are in the memory *)
  ; fd : Unix.file_descr
  }

let index_removed = Stdint.Uint32.max_int
(* Special index to express "removed".  
   No problem of conflict since 2^32-1 is invalid for an index.
*)
                      
let exists = Sys.file_exists

let create path =
  let open Unix in
  let fd = openfile path [ O_RDWR; O_CREAT; O_TRUNC ] 0o644 in
  { tbl = Hashtbl.create 1001; fd }

let read_commit fd =
  let open Unix in
  let buf = Bytes.create 60 in
  let r = read fd buf 0 60 in
  if r = 0 then None (* EOF *)
  else if r <> 60 then failwith (string_of_int r) (* XXX *)
  (* XXX garbage may exist, if write_commit fails. we have to ignore them *)
  else begin
    Format.eprintf "%S@." (Bytes.to_string buf);
    Some (Hash.hash56_of_string @@ Bytes.sub_string buf 0 56,
          let cstr = Cstruct.of_bytes buf in
          Stdint.Uint32.of_int32 @@ Cstruct.LE.get_uint32 cstr 56)
  end
    
let write_commit fd hash index =
  let open Unix in
  let hash = (hash : Hash.hash56 :> string) in
  let buf = Bytes.create 60 in
  Bytes.blit_string hash 0 buf 0 56;
  let cstr = Cstruct.create 4 in
  Cstruct.LE.set_uint32 cstr 0 @@ Stdint.Uint32.to_int32 index;
  Cstruct.blit_to_bytes cstr 0 buf 56 4;
  let w = single_write fd buf 0 60 in
  if w <> 60 then failwith (string_of_int w) (* XXX *)

let open_ path =
  let open Unix in
  let fd = openfile path [ O_RDWR ] 0o644 in
  let tbl = Hashtbl.create 1001 in
  let rec loop () =
    match read_commit fd with
    | None -> { tbl; fd }
    | Some (h,i) when i = index_removed -> 
        Hashtbl.remove tbl h;
        loop ()
    | Some (h,i) -> 
        Hashtbl.replace tbl h i; (* XXX overwrite should be warned *)
        loop ()
    | exception (Failure _) ->
        (* XXX garbate found, overwrite from here. 
           XXX we have to rewind to the point of the last valid read
        *)
        { tbl; fd }
  in
  loop ()

let open_ path = if not @@ exists path then create path else open_ path
  
let add { tbl; fd } hash index =
  Hashtbl.replace tbl hash index; (* XXX overwrite should be warned *)
  write_commit fd hash index;
  Format.eprintf "Added root %S at %Ld@." (Hash.to_string hash) (Types.Index.to_int64 index)

let mem { tbl ; _ } = Hashtbl.mem tbl

let remove ({ tbl; fd } as t) hash =
  if mem t hash then begin
    Hashtbl.remove tbl hash;
    write_commit fd hash index_removed
  end else ()

let find { tbl ; _ } = Hashtbl.find_opt tbl

let close { fd ; _ } = Unix.close fd
