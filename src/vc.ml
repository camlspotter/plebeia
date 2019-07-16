open Node
open Cursor

type cursor = Cursor.t

type t = 
  { roots : Roots.t
  ; context : Context.t
  }

let create ?context_pos ?context_length ~prefix () =
  let roots = Roots.create (prefix ^ ".roots") in
  let context = 
    Context.create ?pos:context_pos ?length:context_length
      (prefix ^ ".context")
  in
  { roots ; context }

let open_ ?context_pos ~prefix =
  let roots = Roots.open_ (prefix ^ ".roots") in
  let context = 
    Context.open_ ?pos:context_pos ~shared:true
      (prefix ^ ".context")
  in
  { roots ; context }

let close { roots ; context } =
  Context.close context;
  Roots.close roots

let empty_cursor { context ; _ } = Cursor.empty context

let hash = Cursor_hash.hash

let commit { roots ; context } (Cursor (_, _, context') as c) =
  assert (context == context');
  let (cur, i, h) = Cursor_storage.commit_cursor c in
  match Roots.find roots h with
  | None ->
      Roots.add roots h i;
      (cur, h)
  | Some _i' -> Pervasives.failwith "hash collision"

let checkout { roots ; context } hash =
  match Roots.find roots hash with
  | None -> None
  | Some (index, _parent) ->
      Some (_Cursor (_Top, 
                     Disk (index, Not_Extender),
                     context))

let fold ~init c f =
  Cursor.fold ~init c (fun acc c ->
      let Cursor (trail, _, _), v = Cursor.view_cursor c in
      let seg = local_seg_of_trail trail in
      match v with
      | Leaf (v, _, _) -> f acc seg (`Leaf v)
      | Bud _ -> f acc seg `Bud
      | _ -> assert false)
 
let get = Deep.get
let get' = Deep.get'
let insert = Deep.insert
let upsert = Deep.upsert
let update = Deep.update
let delete = Deep.delete
let create_subtree = Deep.create_subtree
let subtree_or_create = Deep.subtree_or_create
let deep = Deep.deep                          
let copy = Deep.copy
let stat { context ; _ } = Context.stat context
