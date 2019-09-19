(*

   Scan the root hashes and prints out branches.

   count_branches ~/.tezos-node/plebeia.context
*)
open Plebeia.Impl

let (//) = Filename.concat

let () =
  let path = Sys.argv.(1) in
  let ctxt = Vc.open_ ~shared:false ~load_hashcons:false path in
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
            let es' = 
              match Hashtbl.find_opt roots.children e.index with
              | None -> []
              | Some es' -> es'
            in
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
        
