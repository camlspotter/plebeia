module Int64 = Stdint.Int64
  
(* XXX Problem in 32bit arch
     
   * [Cstruct.of_bigarray] only takes [char] indexed [Bigstring.t].
   * Offset must be [int] in [Cstruct].
   
   The current simple implementation to map the entire file to one [Bigstring.t]
   restricts the maximum file size in 32bit arch to [1_073_741_823], which is roughly just 1GB.
*)

type t = {
  mutable array : Bigstring.t ;
  (* mmaped array where the nodes are written and indexed. *)

  mutable current_length : Index.t ;
  (* Current length of the node table. *)

  mutable mapped_length : Index.t ;

  fd : Unix.file_descr ; 
  (* File descriptor to the mapped file *)

  pos : int64 ; 
  (* Position of the first cell in the file *)

  shared : bool ;
  (* Write to the file or not *)
  
  stat : Stat.t ;
  (* Statistics *)
}

let stat t = t.stat
              
module Header = struct
  (* Header is the first cell, which carries the current length *)
  let read a = 
    let cstr = Cstruct.of_bigarray ~off:0 ~len:32 a in
    Index.of_uint32 @@ Utils.Cstruct.get_uint32 cstr 0

  let write a i = 
    let cstr = Cstruct.of_bigarray ~off:0 ~len:32 a in
    Utils.Cstruct.set_uint32 cstr 0 i
end

let get_cell t i =
  (* XXX May overflow in 32bits arch! *)
  let i = Index.to_int i + 1 (* header *) in 
  Cstruct.of_bigarray ~off:(i*32) ~len:32 t.array

let get_bytes t i n =
  (* XXX May overflow in 32bits arch! *)
  let i = Index.to_int i + 1 (* header *) in 
  Cstruct.of_bigarray ~off:(i*32) ~len:n t.array

(* 2^32 - 256 .. 2^32 - 1 are used for tags 
   
   Max index: 4G = 4_294_967_040
   The maximum data size is about 128GB
*)
let max_index = Index.(max_int - of_int 256)

let bytes_per_cell = 32L

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
            if i > Index.(to_int max_int) then Utils.failwithf "Context.create: too large: %d@." i
            else Index.of_int i
        | _ -> assert false
  in
  let array = make_array fd ~pos ~shared:true mapped_length in
  Header.write array Index.zero;
  { array ;
    mapped_length ;
    current_length = Index.zero ;
    fd ; 
    pos ;
    shared = true ;
    stat = Stat.create ()
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
    let current_length = Header.read array in
    { array ;
      mapped_length ;
      current_length ;
      fd = fd ;
      pos ; 
      shared ;
      stat = Stat.create ()
    }
  end
  
let set_current_length c i =
  c.current_length <- i;
  Header.write c.array i
  
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

let close { fd ; array ; current_length ; _ } =
  Header.write array current_length ;
  Unix.close fd
