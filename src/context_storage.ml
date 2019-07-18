open Node
open Storage
module C = Storage.Cstruct

(** node storage.
    See Layout.md for the format *)

exception LoadFailure of Error.t

let rec parse_cell storage i =
  let buf = make_buf storage i in
  let tag = C.get_index buf 28 in
  let tag_int32 = Index.to_int32 tag in (* easier to match *)
  match tag_int32 with
  | -256l -> (* bud *)
      begin match C.get_char buf 0 with
        | '\255' -> 
            _Bud (None, Indexed i, Hashed Node_hash.(shorten @@ of_bud None))
        | _ ->  
            (* We must load the child for the hash *)
            let i' = C.get_index buf 24 in
            let v = parse_cell storage i' in
            begin match hash_of_view v with
            | None -> assert false
            | Some h -> _Bud (Some (View v), Indexed i, Hashed h)
            end
      end

  | -65l -> (* zero size leaf *)
      let h = C.get_hash buf 0 in
      let v = Value.of_string "" in
      _Leaf (v, Indexed i, Hashed h)
      
  | x when -32l <= x && x <= -1l -> (* leaf whose value is in the previous cell *)
      let l = - Int32.to_int x in (* 1 to 32 *)
      let h = C.get_hash buf 0 in
      let buf = make_buf storage (Index.pred i) in
      let v = Value.of_string @@ C.copy buf 0 l in
      _Leaf (v, Indexed i, Hashed h)

  | x when -64l <= x && x <= -33l -> (* leaf whose value is in the 2 previous cells *)
      let l = - Int32.to_int x in (* 33 to 64 *)
      let h = C.get_hash buf 0 in
      let buf = make_buf2 storage (Index.pred @@ Index.pred i) in
      let v = Value.of_string @@ C.copy buf 0 l in
      _Leaf (v, Indexed i, Hashed h)

  | -254l -> (* linked *)
      let i' = C.get_index buf 24 in
      parse_cell storage i'
      
  | -255l -> (* leaf whose value is in Plebeia *)
      let h = C.get_hash buf 0 in
      let v = Value.of_string @@ Chunk.read storage @@ Index.pred i in
      _Leaf (v, Indexed i, Hashed h)

  | x when -256l <= x && x <= -1l -> assert false

  | _ -> 
      let s_224 = C.copy buf 0 28 in
      let last_byte = Char.code @@ String.unsafe_get s_224 27 in
      match last_byte land 0x01 with
      | 1 -> (* extender *)
          (* extender  |0*1|<- segment ---------------->|1| |<- the index of the child ------------>| *)
          let seg_code = s_224 in
          let seg = Segment_encoding.decode seg_code in
          let i' = C.get_index buf 28 in
          (* We must load the child for the hash *)
          let v = parse_cell storage i' in
          let h = match hash_of_view v with
            | None -> assert false
            | Some h -> h
          in
          _Extender (seg, View v, Indexed i, Hashed h)
      | 0 -> (* internal *)
          let s_0_215 = C.copy buf 0 27 (* 216bits *) in
          let c_216_223, refer_to_right = 
            let c = Char.code @@ C.get_char buf 27 in
            (Char.chr (c land 0xfc), (c land 2) = 2)
          in
          let h = Hash.of_string (s_0_215 ^ String.make 1 c_216_223) in
          let i' = C.get_index buf 28 in
          if refer_to_right then
            _Internal (Disk (Index.pred i, Maybe_Extender), Disk (i', Maybe_Extender), Indexed i, Hashed h)
          else
            _Internal (Disk (i', Maybe_Extender), Disk(Index.pred i, Maybe_Extender), Indexed i, Hashed h)
      | _ -> assert false

let load_node context (index : Index.t) (ewit:extender_witness) : view = 
  let storage = context.Context.storage in
  let v = parse_cell storage index in
  Stat.incr_loaded_nodes context.Context.stat;
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

(* index 32 bits (4294967296)
   block 32 bytes 
   max size of the storage 137_438_953_472 =~ 130Gb
*)
let index n = match index n with
  | Some i -> i
  | None -> assert false

let bud_first_28 = String.make 28 '\255'
let zero_24 = String.make 24 '\000'

let write_small_leaf storage v =
  let len = Value.length v in
  assert (1 <= len && len <= 32);
  let i = Storage.new_index storage in
  let buf = make_buf storage i in
  C.write_string (Value.to_string v) buf 0 len

let write_medium_leaf storage v =
  let len = Value.length v in
  assert (33 <= len && len <= 64);
  let i = Storage.new_indices storage 2 in
  let buf = make_buf2 storage i in
  C.write_string (Value.to_string v) buf 0 len

let write_large_leaf_to_plebeia storage v =
  ignore @@ Storage.Chunk.write storage (Value.to_string v)

let write_leaf context v lh =
  (* contents are ALREADY written *)
  let storage = context.Context.storage in
  let h = Node_hash.shorten lh in
  let i = Storage.new_index storage in
  let len = Value.length v in
  if len = 0 then begin
    let buf = make_buf storage i in
    C.write_string (Hash.to_string h) buf 0 28;
    C.set_index buf 28 (Index.of_int (-65))
  end else if len <= 64 then begin
    let buf = make_buf storage i in
    C.write_string (Hash.to_string h) buf 0 28;
    C.set_index buf 28 (Index.of_int (-len)) (* 1 => -1  64 -> -64 *)
  end else begin
    let k = -255l in
    let buf = make_buf storage i in
    let h = Hash.to_string h in
    C.write_string h buf 0 28;
    C.set_index buf 28 (Index.of_int32 k)
  end;
  Stat.incr_written_leaves context.Context.stat;
  Stat.incr_written_leaf_sizes context.Context.stat len;
  _Leaf (v, Indexed i, Hashed h), i, lh

let write_link context i index =
  (* |<- 192 0's ->|<-   child index  ->| |<- 2^32 - 254 ------------------------>| *)
  let storage = context.Context.storage in
  let buf = make_buf storage i in
  C.write_string zero_24 buf 0 24;
  C.set_index buf 24 index;
  C.set_index buf 28 (Index.of_int32 (-254l));
  Stat.incr_written_links context.Context.stat

let write_internal context nl nr lh =
  (* internal  |<- first 222 of hash -------->|D|0| |<- the index of one of the child ----->| *)
  let storage = context.Context.storage in
  let h = Node_hash.shorten lh in
  let i = Storage.new_index storage in
  let buf = make_buf storage i in

  let hstr = Hash.to_string h in
  let il = index nl in
  let ir = index nr in

  let refer_to_left, i =
    if i = Index.succ il then
      (* the following index refers to the right *)
      false, i
    else if i = Index.succ ir then true, i
    else begin
      (* Fat internal *)
      (* Write the link to the right at i *)
      write_link context i ir;
      let i = Index.succ i in
      true, i
    end 
  in

  (* 0 to 215 bits *)
  C.blit_from_string hstr 0 buf 0 27;

  (* fix for the 223rd and 224th bits (pos 222, 223) *)
  C.set_char buf 27
    (let c = Char.code @@ String.unsafe_get hstr 27 in
     let c = c land 0xfc in
     Char.chr (if refer_to_left then c else c lor 2));

  (* next 32bits *)
  C.set_index buf 28 (if refer_to_left then il else ir);

  Stat.incr_written_internals context.Context.stat;
  _Internal (nl, nr, Indexed i, Hashed h), i, lh

let write_empty_bud context =
  (* XXX No point to store the empty bud... *)
  (* empty bud |<- 1111111111111111111111111111 ->| |<- 2^32 - 256 ------------------------>| *)
  let storage = context.Context.storage in
  let i = Storage.new_index storage in
  let buf = make_buf storage i in
  C.write_string bud_first_28 buf 0 28;
  C.set_index buf 28 (Index.of_int32 (-256l));
  Stat.incr_written_buds context.Context.stat;
  Stat.incr_written_empty_buds context.Context.stat;
  _Bud (None, Indexed i, Hashed (Node_hash.(shorten @@ of_bud None))), i, Node_hash.of_bud None


let write_bud context n lh = 
  (* bud       |<- 192 0's ->|<-   child index  ->| |<- 2^32 - 256 ------------------------>| *)
  let h = Node_hash.shorten lh in
  let storage = context.Context.storage in
  let i = Storage.new_index storage in
  let buf = make_buf storage i in
  C.write_string zero_24 buf 0 24;
  C.set_index buf 24 @@ index n;
  C.set_index buf 28 (Index.of_int32 (-256l));
  Stat.incr_written_buds context.Context.stat;
  _Bud (Some n, Indexed i, Hashed h), i, lh

let write_extender context seg n lh =
  (* extender  |0*1|<- segment ---------------->|1| |<- the index of the child ------------>| *)
  let storage = context.Context.storage in
  let h = Node_hash.shorten lh in
  let i = Storage.new_index storage in
  let buf = make_buf storage i in
  C.write_string (Segment_encoding.encode seg) buf 0 28;
  C.set_index buf 28 @@ index n;
  Stat.incr_written_extenders context.Context.stat;
  _Extender (seg, n, Indexed i, Hashed h), i, lh

(* XXX Operations are NOT atomic at all 
   
   XXX This is complicated because this function hashes + commits 
   at the same time. 
*)
let commit_node context node =
  let check_hash h lh = match h with
    | Hashed h -> assert (h = Node_hash.shorten lh)
    | Not_Hashed -> ()
  in
  let storage = context.Context.storage in
  let rec commit_aux : node -> (node * Index.t * Node_hash.t) = function
    | Disk (index, wit) ->
        (* Need to get the hash from the disk *)
        let v, i, h = commit_aux' (load_node context index wit) in
        assert (index = i);
        View v, i, h
    | View v -> 
        let v, i, h = commit_aux' v in
        View v, i, h

  and commit_aux' : view -> (view * Index.t * Node_hash.t) = fun v -> 
    match v with
    (* easy case where it's already commited *)
    | Leaf (_, Indexed i, Hashed h) 
    | Bud (_, Indexed i, Hashed h)
    | Internal (_, _, Indexed i, Hashed h)
    | Extender (_, _, Indexed i, Hashed h) ->
        let lh = snd @@ Node_hash.long_hash context (View v) in
        assert (Node_hash.shorten lh = h);
        (v, i, lh)
         
    (* indexing is necessary below.  If required, the hash is also computed *)
    | Leaf (value, Not_Indexed, h) ->
        let _v, lh = Node_hash.long_hash context (View v) in
        check_hash h lh;
        (* if the size of the value is 1 <= size <= 32, the contents are written
           to the previous index of the leaf *)
        let len = Value.length value in

  if len <> 0 then Stat.incr_written_leaf_sizes' context.Context.stat len;

        let create_new () =
          if len = 0 then begin
            write_leaf context value lh
          end else if len <= 32 then begin
            write_small_leaf storage value;
            write_leaf context value lh
          end else if 33 <= len && len <= 64 then begin
            write_medium_leaf storage value;
            write_leaf context value lh
          end else begin
            write_large_leaf_to_plebeia storage value;
            write_leaf context value lh
          end
        in
        if 1 <= len && len <= 36 then begin
          (* try hashcons *)
          let hashcons = context.Context.hashcons in
          match Hashcons.find hashcons value with
          | Error e -> failwith e
          | Ok (Some index) ->
              let lh = Node_hash.of_leaf value (* XXX inefficient! *) in
              let h = Node_hash.shorten lh in
              _Leaf (value, Indexed index, Hashed h), index, lh
          | Ok None -> 
              let v, i, lh = create_new () in
              begin match Hashcons.add hashcons value i with
                | Ok () -> ()
                | Error e -> failwith e
              end;
              (v, i, lh)
        end else create_new ()

    | Bud (Some underneath, Not_Indexed, h) ->
        let (node, _, lh') = commit_aux underneath in
        let lh = Node_hash.of_bud @@ Some lh' in
        check_hash h lh;
        write_bud context node lh

    | Bud (None, Not_Indexed, h) ->
        let lh = Node_hash.of_bud None in
        check_hash h lh;
        write_empty_bud context

    | Internal (left, right, Left_Not_Indexed, h) ->
        let (right, _ir, lhr) = commit_aux right in
        let (left, _il, lhl) = commit_aux left in (* This one must be the latter *)
        let lh = Node_hash.of_internal lhl lhr in
        check_hash h lh;
        write_internal context left right lh

    | Internal (left, right, Right_Not_Indexed, h) ->
        let (left, _il, lhl) = commit_aux left in
        let (right, _ir, lhr) = commit_aux right in (* This one must be the latter *)
        let lh = Node_hash.of_internal lhl lhr in
        check_hash h lh;
        write_internal context left right lh

    | Extender (segment, underneath, Not_Indexed, h)  ->
        let (underneath, _i, lh') = commit_aux underneath in
        let lh = Node_hash.of_extender segment lh' in
        check_hash h lh;
        write_extender context segment underneath lh

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
  let (node, i, lh) =  commit_aux node in
  node, i, Node_hash.shorten lh

