module Int64 = Stdint.Int64
  
(* XXX Problem in 32bit arch
     
   * [Cstruct.of_bigarray] only takes [char] indexed [Bigstring.t].
   * Offset must be [int] in [Cstruct].
   
   The current simple implementation to map the entire file to one [Bigstring.t]
   restricts the maximum file size in 32bit arch to [1_073_741_823], which is roughly just 1GB.
*)

module C = Xcstruct

type storage = {
  mutable array : Bigstring.t ;
  (* mmaped array where the nodes are written and indexed. *)

  mutable current_length : Index.t ;
  (* Current length of the node table.  
     The next index number to be used. *)

  mutable mapped_length : Index.t ;

  mutable last_root_index : Index.t option ;
  mutable last_cache_index : Index.t option ;
  
  fd : Unix.file_descr ; 
  (* File descriptor to the mapped file *)

  pos : int64 ; 
  (* Position of the first cell in the file *)

  shared : bool ;
  (* Write to the file or not *)
  
  version : int ;
}

type t = storage

let set_last_root_index t x       = t.last_root_index <- x
let set_last_cache_index t x      = t.last_cache_index <- x
let get_last_root_index t         = t.last_root_index
let get_last_cache_index t        = t.last_cache_index

let get_current_length t = t.current_length

(* Constants *)

(* 2^32 - 256 .. 2^32 - 1 are used for tags for nodes
   
   Max index: 4G = 4_294_967_040
   The maximum data size is about 128GB
*)
let max_index = Index.(max_int - of_int 256)

let bytes_per_cell = 32L


(* Resize *)

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


(* Access *)

let get_cell t i =
  (* XXX May overflow in 32bits arch! *)
  let i = Index.to_int i in 
  C.of_bigarray ~off:(i*32) ~len:32 t.array

let get_bytes t i n =
  (* XXX May overflow in 32bits arch! *)
  let i = Index.to_int i in 
  C.of_bigarray ~off:(i*32) ~len:n t.array

let new_index c =
  (* XXX check of size *)
  let i = c.current_length in
  let i' = Index.succ i in
  c.current_length <- i';
  may_resize i' c;
  i

let new_indices c n =
  (* XXX check of size *)
  assert (n > 0);
  let i = c.current_length in
  let i' = Index.(i + of_int n) in
  c.current_length <- i';
  may_resize i' c;
  i

module Header = struct
  type t = 
    { last_next_index : Index.t
    ; last_root_index : Index.t option
    ; last_cache_index : Index.t option
    }

  (*
  Cell #1
  |0                   19|20         23|24        27|28        31|
  |< hash of the right ->|<- i cache ->|<- i root ->|<- i next ->|
  
  Cell #2
  |0                   19|20         23|24        27|28        31|
  |< hash of the right ->|<- i cache ->|<- i root ->|<- i next ->|
  *)
    
  module Blake2B_20 = struct
    open Blake2.Blake2b
  
    let of_string s =
      let b = init 20 in
      update b (Bigstring.of_string s);
      let Hash bs = final b in
      Bigstring.to_string bs
  end
  
  let raw_read' array i =
    let cstr = C.of_bigarray ~off:(i*32) ~len:32 array in
    let last_next_index = C.get_index cstr 28 in
    let last_root_index = Index.zero_then_none @@ C.get_index cstr 24 in
    let last_cache_index = Index.zero_then_none @@ C.get_index cstr 20 in
    let h = C.copy cstr 0 20 in
    let string_20_31 = C.copy cstr 20 12 in
    let h' = Blake2B_20.of_string string_20_31 in (* XXX hash is bigger than the original *)
    if h <> h' then None
    else Some { last_next_index ; last_root_index ; last_cache_index }

  let raw_read array =
    match raw_read' array 1 with
    | Some x -> Some x
    | None -> (* something wrong in the cell #1 *)
        match raw_read' array 2 with
        | Some x -> Some x
        | None -> None (* something wrong in the cell #2 *)
      
  let _read t = raw_read t.array

  let write' t i { last_next_index ; last_root_index ; last_cache_index } =
    let cstr = get_cell t @@ Index.of_int i in
    C.set_index cstr 28 last_next_index;
    C.set_index cstr 24 (Utils.Option.default Index.zero last_root_index);
    C.set_index cstr 20 (Utils.Option.default Index.zero last_cache_index);
    let string_20_31 = C.copy cstr 20 12 in
    let h = Blake2B_20.of_string string_20_31 in
    C.write_string h cstr 0 20

  let write t x =
    (* The write is NOT atomic but corruption can be detected by the checksum 
       and the double writes
    *)
    write' t 1 x;
    write' t 2 x
  
  let commit t =
    let cp = { last_next_index = t.current_length
             ; last_root_index = t.last_root_index
             ; last_cache_index = t.last_cache_index } 
    in
    write t cp
      
end

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

  let version = 1 in
  let cstr = C.of_bigarray ~off:0 ~len:32 array in
  C.blit_from_string ("PLEBEIA " ^ String.make 28 '\000') 0 cstr 0 32;
  C.set_index cstr 24 (Index.of_int version);
  let t = 
    { array ;
      mapped_length ;
      current_length = Index.of_int 3; (* #0 for PLEBEIA..., #1 and #2 for header *)
      last_root_index = None ;
      last_cache_index = None ;
      fd ; 
      pos ;
      shared = true ;
      version
    }
  in
  Header.commit t;
  t

let open_ ?(pos=0L) ?(shared=false) fn =
  if not @@ Sys.file_exists fn then 
    if shared then create ~pos fn 
    else Utils.failwithf "%s: not found" fn
  else begin
    let fd = Unix.openfile fn [O_RDWR] 0o644 in
    let st = Unix.LargeFile.fstat fd in
    let sz = Int64.sub st.Unix.LargeFile.st_size pos in (* XXX think about the garbage *)
    assert (Int64.rem sz 32L = 0L);
    let cells = Int64.(sz / 32L) in 
    if cells > Index.to_int64 max_index then assert false;
    let mapped_length = Index.of_int64 cells in
    let array = make_array fd ~pos ~shared mapped_length in

    let cstr = C.of_bigarray ~off:0 ~len:32 array in
    if not (C.copy cstr 0 8 = "PLEBEIA ") then failwith "This is not Plebeia data file";
    let version = Index.to_int @@ C.get_index cstr 24 in
    assert (version = 1);

    match Header.raw_read array with
    | None -> Utils.failwithf "Failed to load header"
    | Some h ->
        { array ;
          mapped_length ;
          current_length = h.Header.last_next_index;
          last_root_index = h.Header.last_root_index;
          last_cache_index = h.Header.last_cache_index;
          fd = fd ;
          pos ; 
          shared ;
          version
        }
  end

let close ({ fd ; _ } as t) =
  Header.commit t;
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
