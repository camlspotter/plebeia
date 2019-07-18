(* Implementation of the root table.
   
   All the data should be in memory.
*)

module C = Xcstruct

let zero_then_none x = if x = Index.zero then None else Some x

type t = 
  { tbl : (Hash.t, (Index.t * Index.t option)) Hashtbl.t  (* all are in the memory *)
  ; context : Context.t
  }

(* commits

|0        19|20      23|24        27|28      31|
|<- meta  ->|<- prev ->|<- parent ->|<- idx  ->|

*)

type commit = 
  { commit_meta : string (* 20 bytes *)
  ; commit_prev : Index.t option
  ; commit_parent : Index.t option
  ; commit_index : Index.t
  }

let write_commit storage commit =
  let i = Storage.new_index storage in
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
  { commit_index ; commit_parent ; commit_prev ; commit_meta }

(* Cache

|0    23|24      27|28      31|
|   0   |<- prev ->|<- leaf ->|
    
let write_cache context previous_cache_idx leaf_idx size     
*)

let pp_entry ppf (hash, (index, parent)) =
  let f fmt = Format.fprintf ppf fmt in
  f "%S at %Ld (parent=%a)" 
    (Hash.to_string hash) 
    (Index.to_int64 index)
    (fun _ppf -> function
       | None -> f "none"
       | Some x -> f "%Ld" (Index.to_int64 x)) parent

let read_commits t =
  let rec aux = function
    | None -> ()
    | Some i ->
        let commit = read_commit t.context.Context.storage i in
        let h = match Node.(hash_of_view @@ load_node t.context commit.commit_index Not_Extender) with
          | None -> assert false
          | Some h -> h
        in
        if Hashtbl.mem t.tbl h then assert false (* hash collision *)
        else Hashtbl.add t.tbl h (commit.commit_index, commit.commit_parent);
        Format.eprintf "read %a@." pp_entry (h, (commit.commit_index, commit.commit_parent));
        aux commit.commit_prev
  in
  aux (Storage.get_last_root_index t.context.Context.storage)
  
let write_commit t ?parent index =
  let storage = t.context.Context.storage in
  let commit_prev = Storage.get_last_root_index storage in
  let commit = { commit_prev ; commit_index = index ; commit_parent = parent ; commit_meta = "dummydummydummydummy" } in
  let i = write_commit storage commit in
  Storage.set_last_root_index storage (Some i)

let add t ?parent hash index =
  Hashtbl.replace t.tbl hash (index, parent);
  write_commit t ?parent index;
  Format.eprintf "Added root %a@." pp_entry (hash, (index, parent))

let mem { tbl ; _ } = Hashtbl.mem tbl

let find { tbl ; _ } = Hashtbl.find_opt tbl

let create context = 
  let t = { tbl = Hashtbl.create 101 ; context } in
  read_commits t;
  t

