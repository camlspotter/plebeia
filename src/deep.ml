open Node
open Error
open Utils
open Cursor

(** Multi Bud level interface *)
let deep ~go_up ~create_subtrees cur segs f =
  let rec ups cur = function
    | [] -> Ok cur
    | _seg::segs -> parent cur >>= fun cur -> ups cur segs
  in
  let rec aux hist cur = function
    | [] -> assert false 
    | [seg] -> 
        f cur seg >>= fun (cur, res) -> 
        if go_up then 
          ups cur hist >>= fun cur -> 
          Ok (cur, res)
        else
          Ok (cur, res)
    | seg::segs ->
        (if create_subtrees then subtree_or_create else subtree) cur seg >>= fun cur ->
        aux (seg::hist) cur segs
  in
  aux [] cur segs
  
(* Read only version of deep *)
let deep_ro cur segs f =
  deep ~go_up:false ~create_subtrees:false cur segs (fun cur seg -> 
      f cur seg >>| fun res -> (cur, res)) >>= fun (_, res) -> 
  Ok res

let deep_get cur segs = deep_ro cur segs get

let deep_upsert cur segs v = 
  deep ~go_up:true ~create_subtrees:true cur segs (fun cur seg ->
      upsert cur seg v >>| fun cur -> cur, ()) >>| fst
  
let deep_delete cur segs = 
  deep ~go_up:true ~create_subtrees:true cur segs (fun cur seg ->
      match delete cur seg with
      | Ok cur -> Ok (cur, ())
      | Error _ -> Ok (cur, ())) >>| fst
  
let deep_create_subtree cur segs = 
  deep ~go_up:true ~create_subtrees:true cur segs (fun cur seg ->
      subtree_or_create cur seg >>| fun cur -> (cur, ())) >>| fst

let copy ~create_subtrees cur segs1 segs2 =
  let rec is_prefix segs1 segs2 = match segs1, segs2 with
    | [], _ -> true
    | seg1::segs1, seg2::segs2 when seg1 = seg2 -> is_prefix segs1 segs2
    | _ -> false
  in
  if is_prefix segs1 segs2 then Error "copy: it creates a loop!"
  else
    deep ~go_up:false ~create_subtrees:false cur segs1 
      (fun cur seg -> access_gen cur seg >>= function
         | Reached (cur, (Bud _ as bud)) -> Ok (cur, bud)
         | res -> error_access res) >>= fun (_, bud) ->
    deep ~go_up:true ~create_subtrees cur segs2
      (fun cur seg -> 
         alter cur seg (function
             | None -> Ok (View bud)
             | Some _ -> Error "a node already presents for this segment") >>= fun cur ->
         Ok (cur, ())) >>| fst
      
  
type where_from =
  | From_above of dir
  | From_below of dir

and dir =
  | Left
  | Right
  | Center

type position = where_from list * cursor
  
(* Traversal step.  By calling this function repeatedly
   against the result of the function, we can traverse all the nodes.
   Note that this loads all the nodes in the memory in the end.
*)
let traverse (log, Cursor (trail, n, context)) = 
  let v = match n with
    | Disk (i, ewit) -> load_node context i ewit
    | View v -> v
  in
  let c = _Cursor (trail, View v, context) in
  match v, log with
  | _, From_below _ :: From_below _ :: _ -> assert false

  | Leaf _, From_below _ :: _ -> assert false
  | Leaf _, [] -> None
  | Leaf _, From_above d :: log -> 
      let c = from_Ok @@ go_up c in
      Some (From_below d :: log, c)

  | Bud (None, _, _), From_below _ :: _ -> assert false
  | Bud (None, _, _), [] -> None
  | Bud (None, _, _), From_above d :: log -> 
      let c = from_Ok @@ go_up c in
      Some (From_below d :: log, c)

  | Bud (Some _, _, _), From_below _ :: [] -> None
  | Bud (Some _, _, _), From_below _ :: From_above d :: log ->
      let c = from_Ok @@ go_up c in
      Some (From_below d :: log, c)
  | Bud (Some _, _, _), (From_above _ :: _ | []) ->
      let c = from_Some @@ from_Ok @@ go_below_bud c in
      Some (From_above Center :: log, c)

  | Internal _, From_below Center :: _ -> assert false
  | Internal _, (From_above _ :: _ | []) ->
      let c = from_Ok @@ go_side Segment.Left c in
      Some (From_above Left :: log, c)
  | Internal _, From_below Left :: log ->
      let c = from_Ok @@ go_side Segment.Right c in
      Some (From_above Right :: log, c)
  | Internal _, From_below Right :: [] -> None
  | Internal _, From_below Right :: From_above d :: log ->
      let c = from_Ok @@ go_up c in
      Some (From_below d :: log, c)

  | Extender (_seg, _, _, _), (From_above _ :: _ | []) ->
      let c = from_Ok @@ go_down_extender c in
      Some (From_above Center :: log, c)
  | Extender (_, _, _, _), From_below _ :: [] -> None
  | Extender (_seg, _, _, _), From_below _ :: From_above d :: log ->
      let c = from_Ok @@ go_up c in
      Some (From_below d :: log, c)

  
  
