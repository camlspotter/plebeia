(*
   Print the stats of leaves of the last commit.
   
   count_leaves ~/.tezos-node/plebeia.context
*)

open Plebeia.Impl

let (//) = Filename.concat

let () =
  let path = Sys.argv.(1) in
  let vc = Vc.open_ ~shared:false ~load_hashcons:false path in
  let roots = Vc.roots vc in
  
  let new_roots = 
    Roots.fold (fun e acc ->
        match Roots.children roots e.Roots.index with
        | [] -> e::acc
        | _ -> acc) roots []
  in

  let leaves = Array.init 36 (fun _ -> Hashtbl.create 0) in

  match new_roots with
  | [] -> assert false
  | { hash= h ; _ } :: _ ->
      let c = Utils.from_Some @@ Vc.checkout vc h in
      let rec loop ls (log,c) =
        let (c, v) = Cursor.view_cursor c in
        let ls = match v with
          | Leaf (v, _, _) ->
              let len = Value.length v in
              if  len = 0 || len > 36 then ()
              else begin
                let n = match Hashtbl.find_opt leaves.(len-1) v with
                  | None -> 1
                  | Some n -> n + 1
                in
                Hashtbl.replace leaves.(len-1) v n
              end;
              let ls = ls + 1 in
              if ls mod 100000 = 0 then Format.eprintf "checked %d leaves@." ls;
              ls
          | _ -> ls
        in
        match Cursor.traverse (log,c) with
        | Some loc -> loop ls loc
        | None -> Format.eprintf "checked %d leaves!@." ls
      in
      loop 0 ([],c);
      
      Array.iteri (fun i tbl -> 
          let size = i + 1 in
          let count = Hashtbl.fold (fun _ n acc -> n + acc) tbl 0 in
          let distinct = Hashtbl.length tbl in
          Format.eprintf "%d, %d, %d@." size count distinct;
          ()) leaves;
      
      let buds = Hashtbl.create 0 in

      let rec loop (uniq, copied as st) (log,c) =
        let (c, v) = Cursor.view_cursor c in
        match log, v with
        | Cursor.From_above _ :: _, Bud _ ->
            let index = Utils.from_Some @@ Node.index (View v) in
            if Hashtbl.mem buds index then
              match Cursor.traverse_up (log,c) with
              | None -> (uniq, copied+1)
              | Some loc -> loop (uniq, copied+1) loc
            else begin
              Hashtbl.add buds index ();
              match Cursor.traverse (log,c) with
              | Some loc -> loop (uniq+1, copied) loc
              | None -> (uniq+1,copied)
            end
        | _ ->
            match Cursor.traverse (log,c) with
            | Some loc -> loop st loc
            | None -> st
      in
      let uniq, copied = loop (0,0) ([],c) in
      Format.eprintf "%d uniq buds,  %d copied@." uniq copied
      
