module Int64 = Stdint.Int64
  
(* XXX Problem in 32bit arch
     
   * [Cstruct.of_bigarray] only takes [char] indexed [Bigstring.t].
   * Offset must be [int] in [Cstruct].
   
   The current simple implementation to map the entire file to one [Bigstring.t]
   restricts the maximum file size in 32bit arch to [1_073_741_823], which is roughly just 1GB.
*)

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
  let get_hash buf pos = Hash.of_string @@ copy buf pos (pos+28)

  (* Cstruct.blit_from_string, but make sure all the string contents are written *)
  let write_string s buf off len =
    let slen = String.length s in
    if slen <> len then begin Format.eprintf "write_string: %d <> %d@." slen len; assert false end;
    blit_from_string s 0 buf off len
end

module C = Cstruct

type storage = {
  mutable array : Bigstring.t ;
  (* mmaped array where the nodes are written and indexed. *)

  mutable current_length : Index.t ;
  (* Current length of the node table.  
     The next index number to be used. *)

  mutable mapped_length : Index.t ;

  fd : Unix.file_descr ; 
  (* File descriptor to the mapped file *)

  pos : int64 ; 
  (* Position of the first cell in the file *)

  shared : bool ;
  (* Write to the file or not *)
}

type t = storage

(* Constants *)

(* 2^32 - 256 .. 2^32 - 1 are used for tags for nodes
   
   Max index: 4G = 4_294_967_040
   The maximum data size is about 128GB
*)
let max_index = Index.(max_int - of_int 256)

let bytes_per_cell = 32L


(* Header *)
  
module Header = struct

  type t = 
    { next_index : Index.t 
    ; last_root_index : Index.t option
    ; last_cache_index : Index.t option
    }

  (* Header is the first cell, which carries the current length *)
  (*
  |0                19|20        23|24        27|28        31|
  |...................|<- i hash ->|<- i root ->|<- i last ->|
  *)
  
  let zero_then_none x = if x = Index.zero then None else Some x
    
  let raw_read array =
    let cstr = Cstruct.of_bigarray ~off:0 ~len:32 array in
    let next_index = Cstruct.get_index cstr 28 in
    let last_root_index = zero_then_none @@ Cstruct.get_index cstr 24 in
    let last_cache_index = zero_then_none @@ Cstruct.get_index cstr 20 in
    { next_index ; last_root_index ; last_cache_index }

  let read t = raw_read t.array

  let raw_write array { next_index ; last_root_index ; last_cache_index } =
    (* XXX atomic! *)
    let cstr = Cstruct.of_bigarray ~off:0 ~len:32 array in
    Cstruct.set_index cstr 28 next_index;
    Cstruct.set_index cstr 24 (Utils.Option.default Index.zero last_root_index);
    Cstruct.set_index cstr 20 (Utils.Option.default Index.zero last_cache_index)

  let write t = raw_write t.array
    
end

(* Access *)

let get_cell t i =
  (* XXX May overflow in 32bits arch! *)
  let i = Index.to_int i in 
  Cstruct.of_bigarray ~off:(i*32) ~len:32 t.array

let get_bytes t i n =
  (* XXX May overflow in 32bits arch! *)
  let i = Index.to_int i in 
  Cstruct.of_bigarray ~off:(i*32) ~len:n t.array

(* Initialize and resize *)

let resize_step = Index.of_int 1000_000 (* 32MB *)

let make_array fd ~pos ?(shared=false) mapped_length =
  let open Bigarray in
  let size = Int64.(of_uint32 mapped_length * bytes_per_cell + 32L (* header *)) in
  let size = 
    if size > Int64.of_int Pervasives.max_int then 
      Utils.failwithf "Size %Ld is too big in this archtecture" (Int64.of_uint32 mapped_length)
    else Int64.to_int size
  in
  array1_of_genarray @@ Unix.map_file fd ~pos char c_layout shared [| size |] 

let resize required t =
  let open Index in
  let new_mapped_length = 
    ((required - t.mapped_length) / resize_step + Index.one) * resize_step  + t.mapped_length
  in
  Format.eprintf "Storage: resizing to %Ld@." (Index.to_int64 new_mapped_length);
  let array = make_array t.fd ~pos:t.pos ~shared:t.shared new_mapped_length in
  t.array <- array;
  t.mapped_length <- new_mapped_length

let may_resize =
  fun required t ->
    if t.mapped_length < required then 
      resize required t
    else ()

let create ?(pos=0L) ?length fn =
  let fd = Unix.openfile fn [O_CREAT; O_TRUNC; O_RDWR] 0o644 in
  let mapped_length = 
    match length with 
    | None -> resize_step 
    | Some i ->
        match Sys.int_size with
        | 31 | 32 -> Index.of_int i
        | 63 ->
            if i > Index.(to_int max_int) then Utils.failwithf "create: too large: %d@." i
            else Index.of_int i
        | _ -> assert false
  in
  let array = make_array fd ~pos ~shared:true mapped_length in
  Header.raw_write array { next_index = Index.one (* zero is for header *)
                         ; last_root_index = None
                         ; last_cache_index = None };
  { array ;
    mapped_length ;
    current_length = Index.one ;
    fd ; 
    pos ;
    shared = true ;
  }

let open_ ?(pos=0L) ?(shared=false) fn =
  if not @@ Sys.file_exists fn then 
    if shared then create ~pos fn 
    else Utils.failwithf "%s: not found" fn
  else begin
    let fd = Unix.openfile fn [O_RDWR] 0o644 in
    let st = Unix.LargeFile.fstat fd in
    let sz = Int64.sub st.Unix.LargeFile.st_size pos in (* XXX think about the garbage *)
    assert (Int64.rem sz 32L = 0L);
    let cells = Int64.(sz / 32L - 1L (* header *) ) in 
    if cells > Index.to_int64 max_index then assert false;
    let mapped_length = Index.of_int64 cells in
    let array = make_array fd ~pos ~shared mapped_length in
    let header = Header.raw_read array in
    { array ;
      mapped_length ;
      current_length = header.next_index ;
      fd = fd ;
      pos ; 
      shared ;
    }
  end

(* XXX must be changed *)  
let set_current_length c i =
  c.current_length <- i;
  let h = Header.read c in
  Header.write c { h with next_index = i }
  
let read_last_commit_index c =
  let h = Header.read c in 
  h.last_root_index 
  
let write_last_commit_index c i =
  let h = Header.read c in
  Header.write c { h with last_root_index = i }
  
let new_index c =
  (* XXX check of size *)
  let i = c.current_length in
  let i' = Index.succ i in
  set_current_length c i';
  may_resize i' c;
  i

let new_indices c n =
  (* XXX check of size *)
  assert (n > 0);
  let i = c.current_length in
  let i' = Index.(i + of_int n) in
  set_current_length c i';
  may_resize i' c;
  i

let close ({ fd ; current_length ; _ } as c) =
  set_current_length c current_length;
  Unix.close fd

let make_buf storage i = get_cell storage i
let make_buf2 storage i = get_bytes storage i 64

module Chunk = struct

  (* Store data bigger than 32 bytes *)

  let ncells size = (size + 8 + 31) / 32

  let get_footer_fields storage last_index =
    let buf = make_buf storage last_index in
    let cdr = C.get_uint32 buf 28 in
    let size = C.get_uint16 buf 26 in
    (cdr, size)

  let get_chunk storage last_index =
    let cdr, size = get_footer_fields storage last_index in
    (* Format.eprintf "Loading from %d size=%d@." (Index.to_int last_index) size; *)
    let ncells = ncells size in
    let first_index = Index.(last_index - of_int ncells + one) in
    (get_bytes storage first_index size, size, cdr)

  let get_chunks storage last_index =
    let rec aux (bufs, size) last_index =
      let buf, bytes, cdr = get_chunk storage last_index in
      let bufs = buf :: bufs in
      let size = size + bytes in (* overflow in 32bit? *)
      if cdr = Index.zero then (bufs, size)
      else aux (bufs, size) cdr
    in
    aux ([], 0) last_index

  let string_of_cstructs bufs = 
    String.concat "" @@ List.map C.to_string bufs

  let read t i = string_of_cstructs @@ fst @@ get_chunks t i
      
  let write_to_chunk storage cdr s off len =
    assert (String.length s >= off + len);
    let ncells = ncells len in
    let cdr_pos = ncells * 32 - 4 in
    let size_pos = cdr_pos - 2 in

    let i = new_indices storage ncells in
    let last_index = Index.(i + of_int ncells - one) in
    let chunk = get_bytes storage i (32 * ncells) in

    C.blit_from_string s off chunk 0 len;
    (* Format.eprintf "Blit to %d %S@." (Index.to_int last_index) (String.sub s off len); *)
    C.set_uint16 chunk size_pos len;
    C.set_uint32 chunk cdr_pos cdr;
    last_index

  let write storage ?(max_cells_per_chunk=1000) s =
    let max_bytes_per_chunk = 32 * max_cells_per_chunk - 6 in
    let rec f off remain cdr  =
      let len = if remain > max_bytes_per_chunk then max_bytes_per_chunk else remain in
      let cdr' = write_to_chunk storage cdr s off len in
      let off' = off + len in
      let remain' = remain - len in
      if remain' > 0 then f off' remain' cdr'
      else cdr'
    in
    f 0 (String.length s) Index.zero

  let test_write_read st storage =
    let max_cells_per_chunk = Random.State.int st 246 + 10 in
    let size = Random.State.int st (max_cells_per_chunk * 32) + 32 in
    let s = String.init size @@ fun i -> Char.chr (Char.code 'A' + i mod 20) in
    let i = write storage ~max_cells_per_chunk s in
    let s' = string_of_cstructs @@ fst @@ get_chunks storage i in
(*
      if s <> s' then begin
        Format.eprintf "%S %S@." s s';
        assert (s = s')
      end;
      prerr_endline "done"
*)
      assert (s = s')
end
