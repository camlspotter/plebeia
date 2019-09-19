open Plebeia.Impl

let (//) = Filename.concat

module IS = Set.Make(struct 
    type t = Index.t 
    let compare = compare 
  end) 

(* Breadth first iteration of roots from genesis to the latest *)
let fold_roots roots init f =
  let open Roots in
  match genesis roots with
  | [] -> assert false
  | (_::_::_ as hs) -> 
      Format.eprintf "Roots have %d genesis hashes@." (List.length hs);
      assert false
  | [genesis_h] ->
      let e = Hashtbl.find roots.tbl genesis_h in
      let rec loop acc = function
        | [] -> ()
        | e::es ->
            let acc = f acc e in
            let es' = 
              match Hashtbl.find_opt roots.children e.index with
              | None -> []
              | Some es' -> es'
            in
            loop acc (es @ es')
      in
      loop init [e]

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

let find_new_nodes vc past_nodes rh =
  let entry = Hashtbl.find (Vc.roots vc).Roots.tbl rh in
  let parent_i = match entry.parent with Some i -> i | None -> Index.of_int 0 in
                   
  let c = Utils.from_Some @@ Vc.checkout vc rh in

  let rec loop new_nodes = function
    | None -> new_nodes
    | Some ((Cursor.From_above _ :: _ as log),c) ->
        let (c,v) = Cursor.view_cursor c in
        begin match v with
        | Leaf (v, _, _) when Value.length v <= 36 (* XXX hard coded *) ->
            (* This is a small leaf *)
            loop new_nodes (Cursor.traverse_up (log,c))
        | _ ->
            let i = Utils.from_Some @@ Node.index (View v) in
            if i > parent_i then begin
              (* This is a new node, which cannot appear in past_nodes *)
              assert (not @@ IS.mem i past_nodes);
              if IS.mem i new_nodes then begin
                (* quite surprising to have shared node in one commit *)
                Format.eprintf "Shared node found in one commit! %d@." (Index.to_int i)
              end;
              let new_nodes = IS.add i new_nodes in
              loop new_nodes (Cursor.traverse (log,c))
            end else begin
              assert (IS.mem i past_nodes);
              loop new_nodes (Cursor.traverse_up (log,c))
            end
        end
    | Some logc -> loop new_nodes (Cursor.traverse logc)
  in
  loop IS.empty (Some ([], c))

let get_non_small_nodes c = 
  let rec loop new_nodes = function
    | None -> new_nodes
    | Some ((Cursor.From_above _ :: _ as log),c) ->
        let (c,v) = Cursor.view_cursor c in
        let i = Utils.from_Some @@ Node.index (View v) in
        begin match v with
        | Leaf (v, _, _) when Value.length v <= 36 (* XXX hard coded *) ->
            (* This is a small leaf *)
            loop new_nodes (Cursor.traverse_up (log,c))
        | _ ->
            let new_nodes = IS.add i new_nodes in
            loop new_nodes (Cursor.traverse (log,c))
        end
    | Some logc -> loop new_nodes (Cursor.traverse logc)
  in
  loop IS.empty (Some ([], c))
  
let () =
  let path1 = Sys.argv.(1) in
  let vc1 = Vc.open_ ~shared:false ~load_hashcons:false path1 in
  (* let path2 = Sys.argv.(2) in *)
  (* let _vc2 = Vc.create path2 in *)
  let roots = Vc.roots vc1 in
  let _nhashes = Hashtbl.length roots.tbl in

  let cells = Index.to_int @@ Storage.get_current_length (Vc.context vc1).Context.storage in

  (* Cursor.traversal can be too slow *)
  let _t1 = Unix.gettimeofday () in

  (* past node table *)
  let past_nodes_tbl = Hashtbl.create 0 in
  
  let f () e = 
    let h,_ = Hashtbl.find roots.Roots.by_index e.Roots.index in
    let (past_nodes, n, threshold) = match e.Roots.parent with
      | None -> (IS.empty, 0, 10000)
      | Some parent_i -> 
          let (refc, past_nodes, n, threshold) = Hashtbl.find past_nodes_tbl parent_i in
          if refc <= 1 then begin
            Format.eprintf "Forgetting %d %0.2f@." (Index.to_int parent_i) 
              (float (Index.to_int parent_i) /. float cells  *. 100.0);
            Hashtbl.remove past_nodes_tbl parent_i;
            Format.eprintf "Forgot %d. %d entries in the table@." 
              (Index.to_int parent_i)
              (Hashtbl.length past_nodes_tbl);
          end else Hashtbl.replace past_nodes_tbl parent_i (refc-1, past_nodes, n, threshold);
          (past_nodes, n, threshold)
    in
    let new_nodes = find_new_nodes vc1 past_nodes h in
    let new_n = IS.cardinal new_nodes in
    Format.eprintf "%S: %d new nodes@." (Hash.to_string h) new_n;
    let n = new_n + n in
    let past_nodes, n, threshold = 
      (* if nchildren is 0, no point to calculate *)
      if n > threshold then
        (* rescan the tree and refresh *)
        let c = Utils.from_Some @@ Vc.checkout vc1 h in
        let past_nodes = get_non_small_nodes c in
        let n = IS.cardinal past_nodes in
        (past_nodes, n, n * 8)
      else
        (IS.union past_nodes new_nodes, n, threshold) 
    in
    let nchildren = match Hashtbl.find_opt roots.children e.index with
      | None -> 0
      | Some es -> List.length es
    in
    if nchildren > 0 then begin
      Format.eprintf "Adding %d: %d %d@." (Index.to_int e.Roots.index) n threshold;
      Hashtbl.replace past_nodes_tbl e.Roots.index (nchildren, past_nodes, n, threshold)
    end
  in

  fold_roots roots () f
