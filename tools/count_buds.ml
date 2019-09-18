(*
   
   Counts the buds of all the roots.  It does not visit the buds already seen.
   Warning: it takes super long time for a big plebeia context.

*)
open Plebeia.Impl

let (//) = Filename.concat

module IS = Set.Make(struct 
    type t = Index.t 
    let compare = compare 
  end) 

let () =
  let dir = Sys.argv.(1) in
  let ctxt = Vc.open_ ~shared:false ~load_hashcons:false ~prefix:(dir // "plebeia") () in
  let roots = Vc.roots ctxt in

  let nhashes = Hashtbl.length roots.tbl in

  (* Cursor.traversal can be too slow *)
  let t1 = Unix.gettimeofday () in
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
