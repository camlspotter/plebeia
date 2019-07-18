open Cursor
    
let commit_cursor (Cursor (trail, node, context)) =
  match trail with
  | Top -> 
      let (node, i, h) = Context_storage.commit_node context node in
      _Cursor (trail, node, context), i, h
  | _ -> failwith "commit: cursor must point to the root"

