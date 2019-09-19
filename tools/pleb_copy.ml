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
    | Some (log,c) ->
        let (c,v) = Cursor.view_cursor c in
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
          (* This is an old node, which was written before rh *)
          match v with
          | Leaf (v, _, _) when Value.length v <= 36 (* XXX hard coded *) ->
              (* This is a small leaf *)
              loop new_nodes (Cursor.traverse_up (log,c))
          | _ ->
              (* This is not a target of caching. Therefore it is in past_nodes *)
              assert (IS.mem i past_nodes);
              loop new_nodes (Cursor.traverse_up (log,c))
        end
  in
  loop IS.empty (Some ([], c))
  
let () =
  let path1 = Sys.argv.(1) in
  let path2 = Sys.argv.(2) in
  let ctxt1 = Vc.open_ ~shared:false ~load_hashcons:false path1 in
  let _ctxt2 = Vc.create path2 in
  let roots = Vc.roots ctxt1 in
  let _nhashes = Hashtbl.length roots.tbl in

  (* Cursor.traversal can be too slow *)
  let _t1 = Unix.gettimeofday () in
  
  let f () _e = () in

  fold_roots roots () f
(*    
  let _ = Hashtbl.fold (fun hash { Roots.index=_; _} ->
      fun (seen, nseen, pointed, ncopied, nhashes_done) -> 
        Format.eprintf "Checkout %S %d/%d@." (Hash.to_string hash) nhashes_done nhashes;
        match Vc.checkout ctxt hash with
        | None -> assert false
        | Some c ->
            let rec loop log_c_opt (seen, nseen, pointed, ncopied) =
              match log_c_opt with
              | None -> (seen, nseen, pointed, ncopied)
              | Some (log, c) ->
                  match log, Cursor.view_cursor c with
                  | _, (_, Bud (_, Not_Indexed, _)) -> assert false
                  | Cursor.From_above _ :: _, (c, Bud (nopt, Indexed i, _)) ->
                      if IS.mem i seen then
                        loop (Cursor.traverse_up (log, c))
                          (seen, nseen, pointed, ncopied)
                      else begin
                        let seen = IS.add i seen in
                        let nseen = nseen + 1 in
                        if nseen mod 1000 = 0 then begin 
                          Format.eprintf "%d bud seen@." nseen;
                        end;
                        begin match nopt with
                        | None -> 
                            loop (Cursor.traverse (log, c))
                              (seen, nseen, pointed, ncopied)
                        | Some n ->
                            match Node.index n with
                            | None -> assert false
                            | Some j ->
                                if IS.mem j pointed then begin
                                  let ncopied = ncopied + 1 in
                                  if ncopied mod 100 = 0 then Format.eprintf "%d copies seen@." ncopied;
                                  loop (Cursor.traverse_up (log, c))
                                    (seen, nseen, pointed, ncopied)
                                end else
                                  loop (Cursor.traverse (log, c)) (seen, nseen, IS.add j pointed, ncopied)
                        end
                      end
                  | _ -> 
                      loop (Cursor.traverse (log,c)) (seen, nseen, pointed, ncopied)
            in
            let (seen, nseen, pointed, ncopied) = loop (Some ([], c)) (seen, nseen, pointed, ncopied) in

            let nhashes_done = nhashes_done + 1 in
            
            let t2 = Unix.gettimeofday () in
            Format.eprintf "%.2f sec / 10000 bud@." ((t2 -. t1) /. float (nseen / 10000));
            Format.eprintf "%d bud / commit@." (nseen / nhashes_done);

            (seen, nseen, pointed, ncopied, nhashes_done)) 
      roots.tbl (IS.empty, 0, IS.empty, 0, 0)
  in
  ()
*)
