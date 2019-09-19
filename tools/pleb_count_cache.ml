(*
   
   Show the hashcons cache status

   count_cache ~/.tezos-node/plebeia.context
*)

open Plebeia.Impl

let (//) = Filename.concat

let () =
  let path = Sys.argv.(1) in
  let vc = Vc.open_ ~shared:false path in
  let ctxt = Vc.context vc in
  let hashcons = ctxt.Context.hashcons in
  Hashcons.stat hashcons


