open Node
open Result
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

let get cur segs = deep_ro cur segs get

let get' cur segs = deep_ro cur segs get'

let upsert cur segs v = 
  deep ~go_up:true ~create_subtrees:true cur segs (fun cur seg ->
      upsert cur seg v >>| fun cur -> cur, ()) >>| fst
  
let insert cur segs v = 
  deep ~go_up:true ~create_subtrees:true cur segs (fun cur seg ->
      insert cur seg v >>| fun cur -> cur, ()) >>| fst
  
let update cur segs v =
  deep ~go_up:true ~create_subtrees:false cur segs (fun cur seg ->
      Cursor.get cur seg >>= fun _ -> update cur seg v >>| fun cur -> cur, ()) >>| fst

let delete cur segs = 
  deep ~go_up:true ~create_subtrees:true cur segs (fun cur seg ->
      match delete cur seg with
      | Ok cur -> Ok (cur, ())
      | Error _ -> Ok (cur, ())) >>| fst
  
let create_subtree ~create_subtrees cur segs = 
  deep ~go_up:true ~create_subtrees cur segs (fun cur seg ->
      create_subtree cur seg >>| fun cur -> (cur, ())) >>| fst

let subtree cur segs =
  deep ~go_up:false ~create_subtrees:false cur segs (fun cur seg ->
      subtree cur seg >>| fun cur -> (cur, ())) >>| fst

let subtree_or_create ~create_subtrees cur segs =
  deep ~go_up:false ~create_subtrees cur segs (fun cur seg ->
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
    (* bud is copied. Therefore this will not break the internal node's
       invariant.  (If we share the bud for further optmization, 
       then it breaks the invariant.)
    *)
    deep ~go_up:true ~create_subtrees cur segs2
      (fun cur seg -> 
         alter cur seg (function
             | None -> Ok (View bud)
             | Some _ -> Error "a node already presents for this segment") >>= fun cur ->
         Ok (cur, ())) >>| fst

