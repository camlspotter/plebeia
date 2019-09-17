(* Many small leaves share the same value, therefore sharing the nodes
   can reduce the storage usage tremendously.
   
storage format
   * index to the value
   * index to the previous record

|0        7|8            31|
|<- prev ->|<- 7 indexes ->|

*)

module C = Xcstruct

let max_size = 36

type score = int
let max_score = 255

(* score
   
   0 : Only in the memory.  Wiped out in the next clean-up. Another +1 score changes it to 3.
   1 : Just added.  Never in the file.  Another +1 score changes it to 3.
   2 : In the file.  Wiped out in the next clean-up.
   3 or more : In the file.
*)
   
type t = 
  { tbl : (Value.t, Index.t * score) Hashtbl.t array
  ; mutable journal : Index.t list
  ; storage : Storage.t
  ; mutable count : int
  }

let create storage = 
  { tbl = Array.init max_size (fun _ -> Hashtbl.create 101)
  ; journal = []
  ; storage
  ; count = 0
  }
  
let zero_28 = String.make 28 '\000'
    
let write_entry t xs =
  let len = List.length xs in
  assert (0 < len && len <= 7);
  let i = Storage.new_index t.storage in
  let buf = Storage.make_buf t.storage i in
  C.set_index buf 0 (Utils.Option.default Index.zero (Storage.get_last_cache_index t.storage));
  C.blit_from_string zero_28 0 buf 4 28;
  let rec aux pos = function
    | [] -> ()
    | x::xs -> 
        C.set_index buf pos x;
        aux (pos+4) xs
  in
  aux 4 xs;
  i

let may_flush_journal t =
  if List.length t.journal <> 7 then ()
  else begin
    let i = write_entry t t.journal in
    t.journal <- [];
    Storage.set_last_cache_index t.storage (Some i);
    (* Format.eprintf "Plebeia cache is flushed at %Ld@." (Index.to_int64 i) *)
  end
    
let read_entry t i =
  let buf = Storage.make_buf t.storage i in
  let prev = Index.zero_then_none @@ C.get_index buf 0 in
  let rec parse pos =
    if pos = 32 then []
    else
      let i = C.get_index buf pos in
      if i = Index.zero then parse (pos+4)
      else i :: parse (pos+4)
  in
  prev, parse 4

let read t ~load_leaf_value =
  let cntr = ref 0 in
  let add_index t i =
    match load_leaf_value i with
    | None -> () (* bad value. ignore it *)
    | Some v ->
        incr cntr;
        if !cntr mod 100000 = 0 then begin
          Format.eprintf "Hashcons: loaded %d cached small values@." !cntr
        end;
        let len = String.length @@ Value.to_string v in
        if len <= max_size then
          Hashtbl.replace (Array.unsafe_get t.tbl (len-1)) v (i, 3)
  in    
  let rec loop = function
    | None -> ()
    | Some i ->
        let prev, indices = read_entry t i in
        List.iter (add_index t) indices;
        loop prev
  in
  loop @@ Storage.get_last_cache_index t.storage;
  Format.eprintf "Hashcons: loaded %d cached small values@." !cntr
  
let weight tbl =
  (* high score : low weight *)
  Hashtbl.fold (fun _k (_,score) acc -> 
      let weight = match score with
        | 0 | 1 | 2 | 3 -> 10
        | n -> max 0 (13 - n)
      in
      weight + acc) tbl 0
  
let age_ tbl =
  let keys = Hashtbl.fold (fun k _ acc -> k :: acc) tbl [] in
  List.iter (fun key ->
      match Hashtbl.find tbl key with
      | (_, (0|2)) -> Hashtbl.remove tbl key
      | (idx, score) -> Hashtbl.replace tbl key (idx, score-1)
    ) keys

let age t =
  for i = 1 to max_size do
    let tbl = Array.unsafe_get t.tbl (i-1) in
    if weight tbl > 200_000_0 (* 33 bytes has 179_650 elems on 2019-09-17 *) then begin
      Format.eprintf "Aging size %d...@." i;
      let t1 = Unix.gettimeofday () in
      age_ tbl;
      let t2 = Unix.gettimeofday () in
      Format.eprintf "Aging size %d done in %.2f secs@." i (t2 -. t1);
    end
  done
  
let stat t =
  Format.eprintf "Memory cache status@.";
  for i = 1 to max_size do
    let counts = ref 0 in
    let tbl = Array.unsafe_get t.tbl (i-1) in
    Hashtbl.iter (fun _ _ -> incr counts) tbl;
    let w = weight tbl in
    Format.eprintf "%d, %d, %d@." i !counts w
  done

let add t v index =
  t.count <- t.count + 1;
  if t.count mod 1000 = 0 then begin
    stat t;
    age t;
    stat t;
  end;
  let s = Value.to_string v in
  let len = String.length s in
  if len > max_size then Error "hashcons: too large"
  else 
    let tbl = Array.unsafe_get t.tbl (len-1) in
    match Hashtbl.find_opt tbl v with
    | Some _ -> assert false
    | None ->
        Hashtbl.replace tbl  v (index, 1);
        (* too early to save into the disk *)
        Ok ()

let find t v =
  let s = Value.to_string v in
  let len = String.length s in
  if len > max_size then Error "hashcons: too large"
  else
    Ok (
      match Hashtbl.find_opt (Array.unsafe_get t.tbl (len-1)) v with
      | None -> None
      | Some (idx, score) -> 
          let score, need_to_write = 
            match score with
            | 0 | 1 -> 3, true
            | x -> x + 1, false
          in
          let tbl = Array.unsafe_get t.tbl (len-1) in
          Hashtbl.replace tbl v (idx, min max_score score);
          if need_to_write then begin
            t.journal <- idx :: t.journal;
            may_flush_journal t;
          end;
          Some idx
    )

