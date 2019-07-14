(* Implementation of the root table.
   
   All the data should be in memory.
   Very simple append only format on disk.

   TODO: each add/remove accesses disk.  We can make the IO buffered.
*)
type t = 
  { tbl : (Hash.t, (Index.t * Hash.t option)) Hashtbl.t  (* all are in the memory *)
  ; fd : Unix.file_descr
  }

let index_removed = Stdint.Uint32.max_int
(* Special index to express "removed".  
   No problem of conflict since 2^32-1 is invalid for an index.
*)
                      
let none = String.make 28 '\000'

let exists = Sys.file_exists

let create path =
  let open Unix in
  let fd = openfile path [ O_RDWR; O_CREAT; O_TRUNC ] 0o644 in
  { tbl = Hashtbl.create 1001; fd }

let read_commit fd =
  let open Unix in
  let buf = Bytes.create 60 in (* 28 + 28 + 4 *)
  let r = read fd buf 0 60 in
  if r = 0 then None (* EOF *)
  else if r <> 60 then Utils.failwithf "Roots.read_commit: garbage of %d bytes found" r
  else begin
    Some (Hash.of_string @@ Bytes.sub_string buf 0 28,
          (let cstr = Cstruct.of_bytes buf in
           Utils.Cstruct.get_uint32 cstr 28,

           let s = Bytes.sub_string buf 32 28 in
           if s = none then None else Some (Hash.of_string s))
          )
  end
    
let write_commit fd ?parent hash index =
  let open Unix in
  let hash = (hash : Hash.t :> string) in
  let buf = Bytes.create 60 in
  Bytes.blit_string hash 0 buf 0 28;
  let cstr = Cstruct.create 4 in
  Utils.Cstruct.set_uint32 cstr 0 index;
  Cstruct.blit_to_bytes cstr 0 buf 28 4;
  Bytes.blit_string (match parent with None -> none | Some p -> (p : Hash.t :> string)) 0 buf 32 28;
  let w = single_write fd buf 0 60 in
  if w <> 60 then Utils.failwithf "Roots.write_commit: failed (%d bytes written)" w

let open_ path =
  let open Unix in
  let fd = openfile path [ O_RDWR ] 0o644 in
  let tbl = Hashtbl.create 1001 in
  let rec loop () =
    match read_commit fd with
    | None -> { tbl; fd }
    | Some (h,(i,_)) when i = index_removed -> 
        Hashtbl.remove tbl h;
        loop ()
    | Some (h,(i,p)) -> 
        Hashtbl.replace tbl h (i,p);
        loop ()
  in
  loop ()

let open_ path = if not @@ exists path then create path else open_ path
  
let add { tbl; fd } ?parent hash index =
  Hashtbl.replace tbl hash (index, parent);
  write_commit fd ?parent hash index;
  Format.eprintf "Added root %S at %Ld@." (Hash.to_string hash) (Index.to_int64 index)

let mem { tbl ; _ } = Hashtbl.mem tbl

let remove ({ tbl; fd } as t) hash =
  if mem t hash then begin
    Hashtbl.remove tbl hash;
    write_commit fd hash index_removed
  end else ()

let find { tbl ; _ } = Hashtbl.find_opt tbl

let close { fd ; _ } = Unix.close fd
