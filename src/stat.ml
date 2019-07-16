(* Immutable stat *)
type t = 
  { mutable loaded_nodes       : int
  ; mutable written_leaves     : int
  ; mutable written_empty_buds : int
  ; mutable written_buds       : int
  ; mutable written_internals  : int
  ; mutable written_extenders  : int
  ; mutable written_leaf_sizes : (int, int) Hashtbl.t 
  ; mutable written_33_34      : (string, int) Hashtbl.t
  }

let create () =
  { loaded_nodes       = 0
  ; written_leaves     = 0
  ; written_empty_buds = 0
  ; written_buds       = 0
  ; written_internals  = 0
  ; written_extenders  = 0
  ; written_leaf_sizes = Hashtbl.create 0
  ; written_33_34 = Hashtbl.create 0
  }

let pp ppf t =
  let f fmt = Format.fprintf ppf fmt in
  f "Loaded nodes       : %d@." t.loaded_nodes;
  f "Written leaves     : %d@." t.written_leaves;
  f "Written empty buds : %d@." t.written_empty_buds;
  f "Written buds       : %d@." t.written_buds;
  f "Written internals  : %d@." t.written_internals;
  f "Written extenders  : %d@." t.written_extenders;
  let xs = List.sort compare @@ Hashtbl.fold (fun k v st -> (k,v)::st) t.written_leaf_sizes [] in
  List.iter (fun (sz,n) -> f "%d : %d@." sz n) xs;
  let xs = List.sort compare @@ Hashtbl.fold (fun k v st -> (k,v)::st) t.written_33_34 [] in
  f "Accounts %d@." @@ List.length xs;
  let sum = List.fold_left (fun st (_,v) -> st + v) 0 xs in
  f "Dupes %f@." @@ float sum /. float (List.length xs)

  
let incr_loaded_nodes t = t.loaded_nodes <- t.loaded_nodes + 1
let incr_written_leaves t = t.written_leaves <- t.written_leaves + 1
let incr_written_empty_buds t = t.written_empty_buds <- t.written_empty_buds + 1
let incr_written_buds t = t.written_buds <- t.written_buds + 1
let incr_written_internals t = t.written_internals <- t.written_internals + 1
let incr_written_extenders t = t.written_extenders <- t.written_extenders + 1
let incr_written_leaf_sizes t s = 
  let tbl = t.written_leaf_sizes in
  match Hashtbl.find_opt tbl s with
  | None -> Hashtbl.add tbl s 1
  | Some x -> Hashtbl.replace tbl s (x + 1)
let incr_written_33_34 t v = 
  let s = Value.to_string v in
  let tbl = t.written_33_34 in
  match Hashtbl.find_opt tbl s with
  | None -> Hashtbl.add tbl s 1
  | Some x -> Hashtbl.replace tbl s (x + 1)
