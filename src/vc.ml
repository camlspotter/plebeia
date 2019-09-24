open Node
open Cursor

type cursor = Cursor.t

type t = 
  { roots : Roots.t
  ; context : Context.t
  }

let roots { roots ; _ } = roots
let context { context ; _ } = context

let create ?context_pos ?context_length path =
  let context = 
    Context.create ?pos:context_pos ?length:context_length
      path
  in
  let roots = Roots.create context in
  { roots ; context }

let open_ ?shared ?load_hashcons ?context_pos path =
  let context = 
    Context.open_ ?pos:context_pos ?load_hashcons ?shared path
  in
  let roots = Roots.create context in
  { roots ; context }

let close { context ; _ } = Context.close context

let empty_cursor { context ; _ } = Cursor.empty context

let hash = Cursor_hash.hash

let commit { roots ; context } ~parent ~meta1 ~meta2 (Cursor (_, _, context') as c) =
  assert (context == context');
  let (c, i, h) = Cursor_storage.commit_top_cursor c in
  Format.eprintf "Vc.commit meta2:%S  hash:%S  parent:%S@." 
    meta2
    (Hash.to_string h)
    (match parent with None -> "none" | Some h -> Hash.to_string h);
  match Roots.find roots h with
  | None ->
      let parent = match parent with
        | None -> None
        | Some h -> 
            match Roots.find roots h with
            | None -> Utils.failwithf "No such parent: %S@." (Hash.to_string h)
            | Some { Roots.index ; _ } -> Some index
      in
      Roots.add roots ?parent h i ~meta1 ~meta2;
      Storage.Header.commit context.Context.storage;
      (c, h)
  | Some _i' -> Utils.failwithf "hash collision %S" (Hash.to_string h)

let checkout { roots ; context ; _ } hash =
  match Roots.find roots hash with
  | None -> None
  | Some { Roots.index ; _ } ->
      Some (_Cursor (_Top, 
                     Disk (index, Not_Extender),
                     context))

let ls c =
  let open Result in
  go_below_bud c >>= function
  | None -> Ok []
  | Some c ->
      let res = 
        fold ~init:[] c (fun acc c ->
            let _, v = Cursor.view c in
            let path = Cursor.local_seg_of_cursor c in
            match v with
            | Bud _ -> `Up ((`Dir, path, c) :: acc)
            | Leaf _ -> `Up ((`File, path, c) :: acc)
            | _ -> `Continue acc)
      in
      Ok res
 
let get = Deep.get
let get' = Deep.get'
let insert = Deep.insert
let upsert = Deep.upsert
let update = Deep.update
let delete = Deep.delete
let create_subtree = Deep.create_subtree
let subtree = Deep.subtree
let subtree_or_create = Deep.subtree_or_create
let deep = Deep.deep                          
let copy = Deep.copy
let stat { context = { stat ; _ } ; _ } = stat

