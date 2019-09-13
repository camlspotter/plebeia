open Plebeia.Impl

let (//) = Filename.concat

let () =
  let dir = Sys.argv.(1) in
  let vc = Vc.open_ ~shared:false ~prefix:(dir // "plebeia") () in
  let ctxt = Vc.context vc in
  let hashcons = ctxt.Context.hashcons in
  Hashcons.stat hashcons


