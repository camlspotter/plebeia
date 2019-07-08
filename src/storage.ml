open Stdint
open Utils
open Types
open Node

(** node storage.
    See Layout.md for the format *)

exception LoadFailure of error

module C = Utils.Cstruct

(* Cstruct.blit_from_string, but make sure all the string contents are written *)
let write_string s buf off len =
  let slen = String.length s in
  if slen <> len then begin Format.eprintf "write_string: %d <> %d@." slen len; assert false end;
  C.blit_from_string s 0 buf off len

let make_buf context i = Context.get_cell context i
let make_buf2 context i = Context.get_bytes context i 64

(* get the last 32 bits *)
let get_index buf : Index.t = Index.of_uint32 @@ C.get_uint32 buf 28

let set_index buf i = C.set_uint32 buf 28 @@ Index.to_uint32 i

(* get the first 224 bits *)
let get_hash buf = Hash.hash28_of_string @@ C.copy buf 0 28


module Chunk = struct

  let ncells size = (size + 8 + 31) / 32

  let get_footer_fields context last_index =
    let buf = make_buf context last_index in
    let cdr = C.get_uint32 buf 28 in
    let size = C.get_uint16 buf 26 in
    (cdr, size)

  let get_chunk context last_index =
    let cdr, size = get_footer_fields context last_index in
    (* Format.eprintf "Loading from %d size=%d@." (Index.to_int last_index) size; *)
    let ncells = ncells size in
    let first_index = Uint32.(last_index - of_int ncells + one) in
    (Context.get_bytes context first_index size, size, cdr)

  let get_chunks context last_index =
    let rec aux (bufs, size) last_index =
      let buf, bytes, cdr = get_chunk context last_index in
      let bufs = buf :: bufs in
      let size = size + bytes in (* overflow in 32bit? *)
      if cdr = Index.zero then (bufs, size)
      else aux (bufs, size) cdr
    in
    aux ([], 0) last_index

  let write_to_chunk context cdr s off len =
    assert (String.length s >= off + len);
    let ncells = ncells len in
    let cdr_pos = ncells * 32 - 4 in
    let size_pos = cdr_pos - 2 in

    let i = Context.new_indices context ncells in
    let last_index = Index.(i + of_int ncells - one) in
    let chunk = Context.get_bytes context i (32 * ncells) in

    C.blit_from_string s off chunk 0 len;
    (* Format.eprintf "Blit to %d %S@." (Index.to_int last_index) (String.sub s off len); *)
    C.set_uint16 chunk size_pos len;
    C.set_uint32 chunk cdr_pos cdr;
    last_index

  let write_to_chunks context max_cells_per_chunk s =
    let max_bytes_per_chunk = 32 * max_cells_per_chunk - 6 in
    let rec f off remain cdr  =
      let len = if remain > max_bytes_per_chunk then max_bytes_per_chunk else remain in
      let cdr' = write_to_chunk context cdr s off len in
      let off' = off + len in
      let remain' = remain - len in
      if remain' > 0 then f off' remain' cdr'
      else cdr'
    in
    f 0 (String.length s) Index.zero

  let string_of_cstructs bufs = 
    String.concat "" @@ List.map C.to_string bufs

  let test_write_read st context =
    let max_cells_per_chunk = Random.State.int st 246 + 10 in
    let size = Random.State.int st (max_cells_per_chunk * 32) + 32 in
    let s = String.init size @@ fun i -> Char.chr (Char.code 'A' + i mod 20) in
    let i = write_to_chunks context max_cells_per_chunk s in
    let s' = string_of_cstructs @@ fst @@ get_chunks context i in
(*
      if s <> s' then begin
        Format.eprintf "%S %S@." s s';
        assert (s = s')
      end;
      prerr_endline "done"
*)
      assert (s = s')
end

let rec parse_cell context i =
  let buf = make_buf context i in
  let tag = get_index buf in
  let tag_int32 = Uint32.to_int32 tag in (* easier to match *)
  match tag_int32 with
  | -256l -> (* bud *)
      begin match C.get_char buf 0 with
        | '\255' -> 
            _Bud (None, Indexed i, Hashed NodeHash.of_empty_bud)
        | _ ->  
            (* We must load the child for the hash *)
            let i' = C.get_uint32 buf 24 in
            let v = parse_cell context i' in
            begin match hash_of_view v with
            | None -> assert false
            | Some h -> _Bud (Some (View v), Indexed i, Hashed h)
            end
      end

  | -254l -> (* leaf whose value is in the external KVS *)
      begin match Context.kvs context with
        | None -> assert false (* no external KVS created *)
        | Some kvs ->
            let h = get_hash buf in
            match KVS.get_opt kvs h with
            | None -> raise (LoadFailure (Printf.sprintf "Hash %s is not found in KVS" @@ to_hex @@ Hash.to_string h))
            | Some v -> _Leaf (v, Indexed i, Hashed (Hash.extend_to_t h))
      end

  | x when -32l <= x && x <= -1l -> (* leaf whose value is in the previous cell *)
      let l = - Int32.to_int x in (* 1 to 32 *)
      let h = get_hash buf in
      let buf = make_buf context (Index.pred i) in
      let v = Value.of_string @@ C.copy buf 0 l in
      _Leaf (v, Indexed i, Hashed (Hash.extend_to_t h))

  | x when -64l <= x && x <= -33l -> (* leaf whose value is in the 2 previous cells *)
      let l = - Int32.to_int x in (* 33 to 64 *)
      let h = get_hash buf in
      let buf = make_buf2 context (Index.pred @@ Index.pred i) in
      let v = Value.of_string @@ C.copy buf 0 l in
      _Leaf (v, Indexed i, Hashed (Hash.extend_to_t h))

  | -255l -> (* leaf whose value is in Plebeia *)
      let h = get_hash buf in
      let (bufs, _size) = Chunk.get_chunks context @@ Index.pred i in
      let v = Value.of_string @@ Chunk.string_of_cstructs bufs in
      _Leaf (v, Indexed i, Hashed (Hash.extend_to_t h))

  | x when -253l <= x && x <= -65l -> assert false

  | _ -> 
      let s_224 = C.copy buf 0 28 in
      let last_byte = Char.code @@ String.unsafe_get s_224 27 in
      match last_byte land 0x01 with
      | 1 -> (* extender *)
          (* extender  |0*1|<- segment ---------------->|1| |<- the index of the child ------------>| *)
          let seg_code = s_224 in
          let seg = Segment_encoding.decode seg_code in
          let i' = get_index buf in
          (* We must load the child for the hash *)
          let v = parse_cell context i' in
          let h = match hash_of_view v with
            | None -> assert false
            | Some h -> h
          in
          let h = NodeHash.of_extender' ~segment_code:seg_code h in
          _Extender (seg, View v, Indexed i, Hashed h)
      | 0 -> (* internal *)
          let s_0_215 = C.copy buf 0 27 (* 216bits *) in
          let c_216_223, refer_to_right = 
            let c = Char.code @@ C.get_char buf 27 in
            (Char.chr (c land 0xfc), (c land 2) = 2)
          in
          let h = Hash.extend_to_t @@ Hash.hash28_of_string (s_0_215 ^ String.make 1 c_216_223) in
          let i' = get_index buf in
          if refer_to_right then
            _Internal (Disk (Index.pred i, Maybe_Extender), Disk (i', Maybe_Extender), Indexed i, Hashed h)
          else
            _Internal (Disk (i', Maybe_Extender), Disk(Index.pred i, Maybe_Extender), Indexed i, Hashed h)
      | _ -> assert false

(* index 32 bits (4294967296)
   block 32 bytes 
   max size of the storage 137_438_953_472 =~ 130Gb
*)
let index n = match index n with
  | Some i -> i
  | None -> assert false

let bud_first_28 = String.make 28 '\255'
let zero_24 = String.make 24 '\000'

let write_small_leaf context v =
  let len = Value.length v in
  assert (1 <= len && len <= 32);
  let i = Context.new_index context in
  let buf = make_buf context i in
  write_string (Value.to_string v) buf 0 len

let write_medium_leaf context v =
  let len = Value.length v in
  assert (33 <= len && len <= 64);
  let i = Context.new_indices context 2 in
  let buf = make_buf2 context i in
  write_string (Value.to_string v) buf 0 len

let write_large_leaf_to_kvs kvs h v =
  let len = Value.length v in
  assert (len > 32);
  KVS.insert kvs h v

let write_large_leaf_to_plebeia context v =
  ignore @@ Chunk.write_to_chunks context 1000 (Value.to_string v)

let write_internal context nl nr h =
  (* internal  |<- first 222 of hash -------->|D|0| |<- the index of one of the child ----->| *)
  let i = Context.new_index context in
  let buf = make_buf context i in

  let hstr = Hash.to_string h in
  let il = index nl in
  let ir = index nr in
  let refer_to_left =
    if i = Index.succ il then
      (* the following index refers to the right *)
      false
    else if i = Index.succ ir then true
    else assert false
  in

  (* 0 to 215 bits *)
  C.blit_from_string hstr 0 buf 0 27;

  (* fix for the 223rd and 224th bits (pos 222, 223) *)
  C.set_char buf 27
    (let c = Char.code @@ String.unsafe_get hstr 27 in
     let c = c land 0xfc in
     Char.chr (if refer_to_left then c else c lor 2));

  (* next 32bits *)
  C.set_uint32 buf 28 (if refer_to_left then il else ir);

  Stat.incr_written_internals (Context.stat context);
  _Internal (nl, nr, Indexed i, Hashed h), i, h

let write_empty_bud context =
  (* XXX No point to store the empty bud... *)
  (* empty bud |<- 1111111111111111111111111111 ->| |<- 2^32 - 256 ------------------------>| *)
  let i = Context.new_index context in
  let buf = make_buf context i in
  write_string bud_first_28 buf 0 28;
  set_index buf (Uint32.of_int32 (-256l));
  Stat.incr_written_buds (Context.stat context);
  _Bud (None, Indexed i, Hashed NodeHash.of_empty_bud), i, NodeHash.of_empty_bud


let write_bud context n h = 
  (* bud       |<- 192 0's ->|<-   child index  ->| |<- 2^32 - 256 ------------------------>| *)
  let i = Context.new_index context in
  let buf = make_buf context i in
  write_string zero_24 buf 0 24;
  C.set_uint32 buf 24 @@ index n;
  set_index buf (Uint32.of_int32 (-256l));
  Stat.incr_written_buds (Context.stat context);
  _Bud (Some n, Indexed i, Hashed h), i, h

let write_leaf context v h =
  (* contents are ALREADY written *)
  let i = Context.new_index context in
  let len = Value.length v in
  if 1 <= len && len <= 64 then begin
    let buf = make_buf context i in
    let h = Hash.shorten_to_hash28 h in
    write_string (Hash.to_string h) buf 0 28;
    set_index buf (Uint32.of_int (-len)) (* 1 => -1  64 -> -64 *)
  end else begin
    let h = Hash.shorten_to_hash28 h in
    let k = 
      match Context.kvs context with
      | Some _kvs ->
          (* kvs should be filled out of this function *)
          -254l
      | None -> -255l
    in
    let buf = make_buf context i in
    let h = Hash.to_string h in
    write_string h buf 0 28;
    set_index buf (Uint32.of_int32 k)
  end;
  Stat.incr_written_leaves (Context.stat context);
  Stat.incr_written_leaf_sizes (Context.stat context) len;
  _Leaf (v, Indexed i, Hashed h), i, h

let write_extender context seg n h =
  (* extender  |0*1|<- segment ---------------->|1| |<- the index of the child ------------>| *)
  let i = Context.new_index context in
  let buf = make_buf context i in
  write_string (Segment_encoding.encode seg) buf 0 28;
  set_index buf @@ index n;
  Stat.incr_written_extenders (Context.stat context);
  _Extender (seg, n, Indexed i, Hashed h), i, h

(* XXX Operations are NOT atomic at all *)
let commit_node context node =
  let rec commit_aux : node -> (node * Index.t * Hash.t) = function
    | Disk (index, wit) ->
        (* Need to get the hash from the disk *)
        let v, i, h = commit_aux' (load_node context index wit) in
        assert (index = i);
        View v, i, h
    | View v -> 
        let v, i, h = commit_aux' v in
        View v, i, h

  and commit_aux' : view -> (view * Index.t * Hash.t) = fun v -> 
    match v with
    (* easy case where it's already commited *)
    | Leaf (_, Indexed i, Hashed h) -> (v, i, h)
    | Bud (_, Indexed i, Hashed h) -> (v, i, h)
    | Internal (_, _, Indexed i, Hashed h)  -> (v, i, h)
    | Extender (_, _, Indexed i, Hashed h) -> (v, i, h)

    (* indexing is necessary below.  If required, the hash is also computed *)
    | Leaf (value, Not_Indexed, h) ->
        let h = match h with
          | Not_Hashed -> NodeHash.of_leaf value 
          | Hashed h -> h
        in
        (* if the size of the value is 1 <= size <= 32, the contents are written
           to the previous index of the leaf *)
        let len = Value.length value in
        if 1 <= len && len <= 32 then begin
          write_small_leaf context value;
          write_leaf context value h
        end else if 33 <= len && len <= 64 then begin
          write_medium_leaf context value;
          write_leaf context value h
        end else begin
          match Context.kvs context with
          | Some kvs -> 
              let h28 = Hash.shorten_to_hash28 h in
              write_large_leaf_to_kvs kvs h28 value;
              write_leaf context value h
          | None ->
              write_large_leaf_to_plebeia context value;
              write_leaf context value h
        end
        
    | Bud (Some underneath, Not_Indexed, h) ->
        let (node, _, h') = commit_aux underneath in
        let h = match h with
          | Hashed h -> assert (h = h'); h
          | _ -> NodeHash.of_bud (Some h')
        in
        write_bud context node h

    | Bud (None, Not_Indexed, h) ->
        begin match h with
          | Hashed h -> assert (h = NodeHash.of_empty_bud)
          | _ -> ()
        end;
        write_empty_bud context

    | Internal (left, right, Left_Not_Indexed, h) ->
        let (right, _ir, hr) = commit_aux right in
        let (left, _il, hl) = commit_aux left in (* This one must be the latter *)
        let h = match h with
          | Not_Hashed -> NodeHash.of_internal hl hr
          | Hashed h -> h
        in
        write_internal context left right h

    | Internal (left, right, Right_Not_Indexed, h) ->
        let (left, _il, hl) = commit_aux left in
        let (right, _ir, hr) = commit_aux right in (* This one must be the latter *)
        let h = match h with
          | Not_Hashed -> NodeHash.of_internal hl hr
          | Hashed h -> h
        in
        write_internal context left right h

    | Extender (segment, underneath, Not_Indexed, h)  ->
        let (underneath, _i, h') = commit_aux underneath in
        let h = match h with
          | Hashed h -> h
          | Not_Hashed -> NodeHash.of_extender segment h'
        in
        write_extender context segment underneath h
        
    | (Leaf (_, Left_Not_Indexed, _)|
       Leaf (_, Right_Not_Indexed, _)|
       Bud (Some _, Left_Not_Indexed, _)|
       Bud (Some _, Right_Not_Indexed, _)|
       Bud (None, Left_Not_Indexed, _)|
       Bud (None, Right_Not_Indexed, _)|
       Internal (_, _, Not_Indexed, _)|
       Extender (_, _, Left_Not_Indexed, _)|
       Extender (_, _, Right_Not_Indexed, _)) -> assert false

    | (Leaf (_, Indexed _, Not_Hashed)|Bud (None, Indexed _, Not_Hashed)|
       Bud (Some _, Indexed _, Not_Hashed)|
       Internal (_, _, Indexed _, Not_Hashed)|
       Extender (_, _, Indexed _, Not_Hashed)) -> assert false

  in 
  let (node, i, h) =  commit_aux node in
  node, i, h

let load_node (context : Context.t) (index : Index.t) (ewit:extender_witness) : view = 
  let v = parse_cell context index in
  Stat.incr_loaded_nodes (Context.stat context);
  match ewit, v with
  | Is_Extender, Extender _ -> v
  | Is_Extender, _ -> assert false (* better report *)
  | Maybe_Extender, Extender _ -> v
  | Not_Extender, Extender _ -> assert false (* better report *)
  | Not_Extender, _ -> v
  | Maybe_Extender, _ -> v

let () = load_node_ref := load_node

let rec load_node_fully context n =
  let v = match n with
    | Disk (i, ewit) -> load_node context i ewit
    | View v -> v
  in
  match v with
  | Leaf _ -> View v
  | Bud (None, _, _) -> View v
  | Bud (Some n, i, h) ->
      let n = load_node_fully context n in
      View (_Bud (Some n, i, h))
  | Internal (n1, n2, i, h) ->
      let n1 = load_node_fully context n1 in
      let n2 = load_node_fully context n2 in
      View (_Internal (n1, n2, i, h))
  | Extender (seg, n, i, h) ->
      let n = load_node_fully context n in
      View (_Extender (seg, n, i, h))
