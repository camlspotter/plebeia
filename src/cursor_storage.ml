open Cursor

let commit_top_cursor (Cursor (trail, node, context)) =
  match trail with
  | Top -> 
      let (node, i, h) = Node_storage.commit_node context node in
      _Cursor (_Top, node, context), i, h
  | _ -> failwith "commit: cursor must point to the root"

let load_fully (Cursor (trail, node, context)) =
  _Cursor (trail, Node_storage.load_node_fully context node, context)
