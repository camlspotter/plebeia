(*
open Plebeia.Impl
open Utils

let (//) = Filename.concat

let () =
  let dir = Sys.argv.(1) in
  let ctxt = Vc.open_ ~shared:false ~prefix:(dir // "plebeia") () in
  let roots = Vc.roots ctxt in
  let module IS = Set.Make(struct 
      type t = Index.t 
      let compare = compare 
    end) 
  in

  let nhashes = Hashtbl.length roots.tbl in
  let buds = ref 0 in

  let _ = Hashtbl.fold (fun hash { Roots.index=_; _} ->
      fun (seen, nseen, pointed, ncopied, nhashes_done) -> 
        Format.eprintf "Checkout %S %d/%d@." (Hash.to_string hash) nhashes_done nhashes;
        match Vc.checkout ctxt hash with
        | None -> assert false
        | Some c ->
            let rec loop seen = function
              | [] -> seen
              | c::cs ->
                  let c, i = 
                    let c, view = Cursor.view_cursor c in
                    c, 
                    from_Some @@ Node.index (View view) 
                  in
                    | None -> a
                    match view with
                    | Bud (
                    
                    
                  incr buds;
                  if !buds mod 1000 = 0 then Format.eprintf "done %d buds@." !buds; 
                  let seen = IS.add c seen in
                  let nseen = nseen + 1 in
                  let pointed, ncopied, continue = 
                    let c, view = Cursor.view_cursor c in
                    match view with
                    | Bud (Some n, _, _) ->
                        begin match Node.index n with
                        | None -> pointed, ncopied, true
                        | Some i ->
                            if IS.mem i pointed then pointed, ncopied + 1, false
                            else IS.add i pointed, ncopied, true
                        end
                    | _ -> pointed, ncopied, true
                  in
                  if not continue then
                    (seen, nseen, pointed, ncopied)
                  else
                    match
                      Cursor.fold ~init:(seen, []) c (fun (seen, new_) c ->
                          match Cursor.view_cursor c with
                          | _, (Bud _ as v) -> 
                              let i = from_Some (Node.index (View v)) in
                              if IS.mem i seen then begin
                                Ok (seen, new_)
                              end else begin
                                  Ok (IS.add i seen, c :: new_)
                                end
                          | _, Leaf _ -> 
                              Ok (seen, new_)
                              | _ -> assert false
                        )

                    with
                    | Error _e -> assert false
                    | Ok (Error _) -> assert false
                    | Ok (Ok (seen, new_)) -> 
                        loop seen (cs @ new_)
            in
            let seen = loop seen [c] in
            Format.eprintf "buds: %d@." (IS.cardinal seen);
            seen, nhashes_done + 1) roots.tbl (IS.empty, 0)
  in
  ()

*)
