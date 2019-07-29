(* Many small leaves share the same value, therefore sharing the nodes
   can reduce the storage usage tremendously.
   
storage format
   * index to the value
   * index to the previous record

|0        7|8            31|
|<- prev ->|<- 7 indexes ->|
   
*)

module C = Xcstruct

type t = 
  { tbl : (int * Value.t, Index.t) Hashtbl.t
  ; mutable journal : Index.t list
  ; storage : Storage.t
  }

let create storage = 
  { tbl = Hashtbl.create 101 
  ; journal = []
  ; storage
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
        if !cntr mod 1000 = 0 then begin
          Format.eprintf "Hashcons: loaded %d cached small values@." !cntr
        end;
        let len = String.length @@ Value.to_string v in
        Hashtbl.replace t.tbl (len, v) i
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
  
let find { tbl ; _ } v =
  let s = Value.to_string v in
  let len = String.length s in
  if len > 36 then Error "hashcons: too large"
  else
    Ok (Hashtbl.find_opt tbl (len, v))


let add t v index =
  let s = Value.to_string v in
  let len = String.length s in
  if len > 36 then Error "hashcons: too large"
  else 
    match Hashtbl.find_opt t.tbl (len, v) with
    | Some _ -> Error "hashcons: registered"
    | None ->
        Hashtbl.replace t.tbl (len, v) index;
        t.journal <- index :: t.journal;
        may_flush_journal t;
        Ok ()

   
