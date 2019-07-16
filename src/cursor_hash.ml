open Cursor
    
let hash (Cursor (trail, node, context)) = 
  let v, h = Node_hash.hash context node in
  _Cursor (trail, View v, context), h

