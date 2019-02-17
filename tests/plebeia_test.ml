open Plebeia.Plebeia_impl

open Error

let test_with_context f =
  let context =
    let fn = Filename.temp_file "plebeia" "test" in
    make_context ~shared:true ~length:1024 fn
  in
  let cursor = empty context in
  f cursor
    
let () =
  ignore @@ from_Ok @@ test_with_context @@ fun cursor ->
  upsert cursor (Path.of_side_list [Path.Left; Path.Right]) (Value.of_string "fooLR") >>= fun cursor -> 
  upsert cursor (Path.of_side_list [Path.Left; Path.Left]) (Value.of_string "fooLL") 
  >>= go_below_bud >>= go_down_extender >>= go_side Path.Left >>= go_up
  >>= go_side Path.Right >>= go_up >>= go_up >>= go_up
