(* Implementation of the root table.
   
   All the data should be in memory.
*)

module C = Xcstruct

let zero_then_none x = if x = Index.zero then None else Some x

type entry = 
  { index  : Index.t
  ; parent : Index.t option
  ; meta1  : string
  ; meta2  : string
  }
  
type t = 
  { tbl      : (Hash.t, entry) Hashtbl.t       (* all are in the memory *)
  ; context  : Context.t                       (* where to store *)
  ; by_index : (Index.t, entry) Hashtbl.t
  ; children : (Index.t, entry list) Hashtbl.t
  }

let add_entry t h ent = 
  Hashtbl.replace t.tbl h ent;
  Hashtbl.replace t.by_index ent.index ent;
  match ent.parent with
  | None -> ()
  | Some i ->
      let entries = 
        match Hashtbl.find_opt t.children i with
        | None -> []
        | Some entries -> entries
      in
      Hashtbl.replace t.children i (ent::entries)
    
(* commits

|0        19|20      23|24        27|28      31|
|<- meta  ->|<- prev ->|<- parent ->|<- idx  ->|

Previous cell:
|0                                           31|
|<------------------- meta2 ------------------>|

The intention for meta2 is to store Irmin context hash.

*)

type commit = 
  { commit_meta   : string (* 20 bytes *)
  ; commit_prev   : Index.t option
  ; commit_parent : Index.t option
  ; commit_index  : Index.t
  ; commit_meta2  : string (* 32 bytes *)
  }

let write_commit storage commit =
  let i = Storage.new_indices storage 2 in
  let buf = Storage.make_buf storage i in
  C.write_string commit.commit_meta2 buf 0 32;
  let i = Index.succ i in
  let buf = Storage.make_buf storage i in
  C.set_index buf 28 commit.commit_index;
  C.set_index buf 24 (Utils.Option.default Index.zero commit.commit_parent);
  C.set_index buf 20 (Utils.Option.default Index.zero commit.commit_prev);
  C.write_string commit.commit_meta buf 0 20;
  i

let read_commit storage i =
  let buf = Storage.make_buf storage i in
  let commit_index = C.get_index buf 28 in
  let commit_parent = zero_then_none @@ C.get_index buf 24 in
  let commit_prev = zero_then_none @@ C.get_index buf 20 in
  let commit_meta = C.copy buf 0 20 in
  let i = Index.pred i in
  let buf = Storage.make_buf storage i in
  let commit_meta2 = C.copy buf 0 32 in
  { commit_index ; commit_parent ; commit_prev ; commit_meta ; commit_meta2 }

let pp_entry ppf (hash, { index ; parent ; meta1 ; _ }) =
  let f fmt = Format.fprintf ppf fmt in
  f "%S at %Ld (parent=%a) %S" 
    (Hash.to_string hash) 
    (Index.to_int64 index)
    (fun _ppf -> function
       | None -> f "none"
       | Some x -> f "%Ld" (Index.to_int64 x)) parent
    meta1

let read_commits t =
  let cntr = ref 0 in
  let rec aux = function
    | None -> ()
    | Some i ->
        let commit = read_commit t.context.Context.storage i in
        let h = match Node.(hash_of_view @@ load_node t.context commit.commit_index Not_Extender) with
          | None -> assert false
          | Some h -> h
        in
        if Hashtbl.mem t.tbl h then assert false; (* hash collision *)
        let ent = { index = commit.commit_index
                  ; parent = commit.commit_parent
                  ; meta1 = commit.commit_meta
                  ; meta2 = commit.commit_meta2 }
        in
        add_entry t h ent;
(*
        Format.eprintf "read %a@." pp_entry (h,ent);
*)
        incr cntr;
        if !cntr mod 1000 = 0 then begin
          Format.eprintf "read %d commmits@." !cntr;
        end;
        aux commit.commit_prev
  in
  aux (Storage.get_last_root_index t.context.Context.storage);
  Format.eprintf "read %d commmits@." !cntr
  
let write_commit t ?parent index ~meta1 ~meta2=
  let storage = t.context.Context.storage in
  let commit_prev = Storage.get_last_root_index storage in
  let commit = { commit_prev ; commit_index = index ; commit_parent = parent ; commit_meta = meta1 ; commit_meta2 = meta2 } in
  let i = write_commit storage commit in
  Storage.set_last_root_index storage (Some i)

let add t ?parent hash index ~meta1 ~meta2 =
  assert (String.length meta1 = 20);
  assert (String.length meta2 = 32);
  let ent = { index ; parent ; meta1 ; meta2 } in
  (* XXX hash collision check *)
  add_entry t hash ent;
  write_commit t ?parent index ~meta1 ~meta2;
  Format.eprintf "Added root %a@." pp_entry (hash, ent)

let mem { tbl ; _ } = Hashtbl.mem tbl

let find { tbl ; _ } = Hashtbl.find_opt tbl

let create context = 
  let t = 
    { tbl = Hashtbl.create 101 
    ; context
    ; by_index = Hashtbl.create 101
    ; children = Hashtbl.create 101
    }
  in
  read_commits t;
  t

let genesis t =
  Hashtbl.fold (fun hash entry acc ->
      if entry.parent = None then hash::acc
      else acc) t.tbl []
