open Plebeia.Impl

let (//) = Filename.concat

let () =
  let dir = Sys.argv.(1) in
  let ctxt = Vc.open_ ~shared:false ~load_hashcons:false ~prefix:(dir // "plebeia") () in
  let roots = Vc.roots ctxt in
  let open Roots in
  match genesis roots with
  | [] -> assert false
  | (_::_::_ as hs) -> 
      Format.eprintf "Roots have %d genesis hashes@." (List.length hs);
      assert false
  | [genesis_h] ->
      let e = Hashtbl.find roots.tbl genesis_h in
      let rec loop f = function
        | [] -> ()
        | e::es ->
            f e;
            let es' = Hashtbl.find roots.children e.index in
            loop f (es @ es')
      in
      loop (fun e -> 
          match Hashtbl.find_opt roots.children e.index with
          | None -> ()
          | Some [] -> ()
          | Some [_] -> ()
          | Some children  ->
              let h, _ = Hashtbl.find roots.by_index e.index in
              Format.eprintf "%d children: %S@." 
                (List.length children) 
                (Hash.to_string h)
        ) [e]
        
