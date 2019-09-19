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
  | [genesis] ->
      let rec loop f = function
        | [] -> ()
        | e::es ->
            f e;
            let es' = Roots.children roots e in
            loop f (es @ es')
      in
      loop (fun e -> 
          match Roots.children roots e with
          | [] -> ()
          | [_] -> ()
          | children  ->
              let { Roots.hash = h ; _ } = Utils.from_Some @@ Roots.find_by_index roots e.index in
              Format.eprintf "%d children: %S@." 
                (List.length children) 
                (Hash.to_hex_string h)
        ) [genesis]
        
