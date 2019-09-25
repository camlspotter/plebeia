open Plebeia.Impl
open Utils

let (//) = Filename.concat

module IS = Set.Make(struct 
    type t = Index.t 
    let compare = compare 
  end) 

module NS = Set.Make(struct 
    type t = Index.t * Node.view
    let compare (n1,_) (n2,_) = Index.compare n1 n2
  end) 

module IM = Map.Make(struct 
    type t = Index.t 
    let compare = compare 
  end) 

(*
   Suppose we have 2 root hashes, rh1, rh2, where rh2's parent is rh1.
   Let top1 and top2 are the top nodes of rh1 and rh2 respectively.

   A node n which is reachable from top2 is:
   
   * If the index of n is newer than the index of top1, n is unreachable
     from top1.
   
   * If the index of n is older than the index of top1, 
                                                        
       * If n is a small leaf which is cached, n may be unreachable from top1.
       * Otherwise, n is always reachable from top1.

   Using the above property, we can reduce the memory usage.
*)

let find_new_nodes vc past_nodes rh : (_ IM.t * IS.t) =
  let entry = from_Some @@ Roots.find (Vc.roots vc) rh in
  let parent_i = match entry.parent with Some i -> i | None -> Index.of_int 0 in
                   
  let c = from_Some @@ Vc.checkout vc rh in

  Cursor.fold ~init:(IM.empty, IS.empty) c @@ fun (new_nodes, frontier) c ->
  let _, v = Cursor.view c in
  let i = from_Some @@ Node.index_of_view v in
  match v with
  | Node.Leaf (v, _, _) when Value.length v <= 36 (* XXX hard coded *) ->
      (* small leaves, handled by the cache system *)
      `Continue (new_nodes, frontier)
  | _ when i > parent_i ->
      (* This is a new node, which cannot appear in past_nodes *)
      assert (not @@ IS.mem i past_nodes);
      if IM.mem i new_nodes then begin
        (* quite surprising to have shared node in one commit *)
        Format.eprintf "Shared node found in one commit! %d@." (Index.to_int i)
      end;
      `Continue (IM.add i v new_nodes, frontier)
  | _ ->
      (* This should be an old node *)
      assert (IS.mem i past_nodes);
      `Continue (new_nodes, IS.add i frontier)

let get_nodes c : IS.t = 
  (* excluding small leaves *)
  Cursor.fold ~init:IS.empty c @@ fun new_nodes c ->
  let _, v = Cursor.view c in
  let i = from_Some @@ Node.index_of_view v in
  match v with
  | Leaf (v, _, _) when Value.length v <= 36 (* XXX hard coded *) ->
      (* This is a small leaf *)
      `Continue new_nodes
  | _ ->
      `Continue (IS.add i new_nodes)

(*
let copy_new_nodes vc2 new_nodes =
  let open Node in
  let new_nodes = 
    List.sort (fun (i1, _) (i2, _) -> Index.compare i1 i2) @@ NS.elements new_nodes
  in
  let get_copied_node _acc _n = assert false in
  (* if n is a small leaf not cached, it cannot be found in acc *)
  List.fold_left (fun acc (i,v) ->
      let n' = match v with
        | Leaf (v, _i, _h) -> new_leaf v
        | Internal (n1, n2, _i, _h) ->
            let n1' = get_copied_node acc n1 in
            let n2' = get_copied_node acc n2 in
            new_internal n1' n2'
        | Bud (None, _i, _h) -> new_bud None
        | Bud (Some n, _i, _h) ->
            let n' = get_copied_node acc n in
            new_bud n'
        | Extender (seg, n, _i _h) -
            let n' = get_copied_node acc n in
            new_extender seg n'
      in
      (i,n')::acc) [] new_nodes
  *)

let () =
  let path1 = Sys.argv.(1) in
  let vc1 = Vc.open_ ~shared:false ~load_hashcons:false path1 in
(*
  let path2 = Sys.argv.(2) in
  let vc2 = Vc.create path2 in
*)

  let roots = Vc.roots vc1 in

  let ncells = Index.to_int @@ Storage.get_current_length (Vc.context vc1).Context.storage in

  let t1 = Unix.gettimeofday () in

  (* past node table *)
  let past_nodes_tbl = Hashtbl.create 0 in

  let report i =
    let i = Index.to_int i in
    let t2 = Unix.gettimeofday () in
    let ratio = float i /. float ncells in
    Format.eprintf "Report: index %d, %.02f, %.0f, %.0f@."
      i
      (ratio *. 100.0)
      (t2 -. t1)
      ((t2 -. t1) /. ratio)
  in

  let threshold = 4_000_000 in

  let f e () = 
    let { Roots.hash = h ; _ } = 
      from_Some @@ Roots.find_by_index roots e.Roots.index 
    in
    let (past_nodes, n) = match e.Roots.parent with
      | None -> (IS.empty, 0)
      | Some parent_i -> 
          let (refc, past_nodes, n) = 
            Hashtbl.find past_nodes_tbl parent_i 
          in
          if refc <= 1 then begin
            Hashtbl.remove past_nodes_tbl parent_i;
            Format.eprintf "Forgot %d. %d entries in the table@." 
              (Index.to_int parent_i)
              (Hashtbl.length past_nodes_tbl);
          end else 
            Hashtbl.replace past_nodes_tbl parent_i (refc-1, past_nodes, n);
          (past_nodes, n)
    in
    let new_nodes = find_new_nodes vc1 past_nodes h in
    let new_n = NS.cardinal new_nodes in
    Format.eprintf "%s: %d new nodes@." (Hash.to_hex_string h) new_n;
    
    (* copy the new nodes and get new_nodes of (Index.t * Index.t) set *)
    let new_nodes = NS.fold (fun (i,_) acc -> IS.add i acc) new_nodes IS.empty in
    
    let n = new_n + n in
    let past_nodes, n = 
      (* if nchildren is 0, no point to calculate *)
      if n > threshold then
        (* rescan the tree and refresh *)
        let c = from_Some @@ Vc.checkout vc1 h in
        let past_nodes = get_non_small_nodes c in
        let n = IS.cardinal past_nodes in
        report e.Roots.index;
        (past_nodes, n)
      else
        (IS.union new_nodes past_nodes, n) 
    in
    let nchildren = List.length @@ Roots.children roots e in
    if nchildren > 0 then begin
      Format.eprintf "Adding %d: %d %d@." (Index.to_int e.Roots.index) n threshold;
      Hashtbl.replace past_nodes_tbl e.Roots.index (nchildren, past_nodes, n)
    end
  in

  Roots.fold_breadth_first f roots ();
  let t2 = Unix.gettimeofday () in
  Format.eprintf "Traversed %d cells in %f secs@." ncells (t2 -. t1)
