(** Cursor (zipper) based tree operations *)

open Utils
open Error
open Node

type t = cursor
  
let dot_of_cursor_ref = ref (fun _ -> assert false)
    
let attach trail node context =
  (* Attaches a node to a trail even if the indexing type and hashing type is incompatible with
     the trail by tagging the modification. Extender types still have to match. *)
  match trail with
  | Top -> _Cursor (_Top, node, context)
  | Left (prev_trail, right, _) ->
      _Cursor (_Left (prev_trail, right, Modified_Left), node, context)
  | Right (left, prev_trail, _) ->
      _Cursor (_Right (left, prev_trail, Modified_Right), node, context)
  | Budded (prev_trail, _) ->
      _Cursor (_Budded (prev_trail, Modified_Left), node, context)
  | Extended (prev_trail, segment, _) ->
      _Cursor (_Extended (prev_trail, segment, Modified_Left), node, context)

(* Tools to create Not_Indexed and Not_Hashed nodes *)
module NotHashed : sig
  val leaf : Value.t -> node
  val extend : Segment.segment -> node -> node
  val bud : node option -> node
  val internal : node -> node -> indexing_rule -> node
end = struct
  let leaf v = View (_Leaf (v, Not_Indexed, Not_Hashed))

  let extend : Segment.segment -> node -> node = fun segment node ->
    if segment = Segment.empty then node
    else 
      match node with
      | View (Extender (seg, n, _, _)) ->
          View (_Extender (Segment.concat segment seg, n, Not_Indexed, Not_Hashed))
      | _ ->
          View (_Extender (segment, node, Not_Indexed, Not_Hashed))
  let bud no = View (_Bud (no, Not_Indexed, Not_Hashed))

  let internal n1 n2 i = 
    View (_Internal (n1, n2, i, Not_Hashed))
end

let go_below_bud (Cursor (trail, n, context)) =
  (* This function expects a cursor positionned on a bud and moves it one step below. *)
  match view context n with
  | Bud (None,  _, _) -> Ok None
  | Bud (Some below, indexing_rule, hashed_is_transitive) ->
      Ok (Some (_Cursor (
          _Budded (trail, Unmodified (indexing_rule, hashed_is_transitive)), below,  context)))
  | _ -> Error "Attempted to navigate below a bud, but got a different kind of node."

let go_side side (Cursor (trail, n, context)) =
  (* Move the cursor down left or down right in the tree, assuming we are on an internal node. *)
  match view context n with
  | Internal (l, r, indexing_rule, hashed_is_transitive) ->
      Ok (match side with
          | Segment.Right ->
              _Cursor (_Right (l, trail,
                             Unmodified (indexing_rule, hashed_is_transitive)),
                       r, context)
          | Segment.Left ->
              _Cursor (_Left (trail, r,
                            Unmodified (indexing_rule, hashed_is_transitive)),
                       l, context))
  | _ -> Error "Attempted to navigate right or left of a non internal node"

let go_down_extender (Cursor (trail, n, context)) =
  (* Move the cursor down the extender it points to. *)
  match view context n with
  | Extender (segment, below, indexing_rule, hashed_is_transitive) ->
      Ok (_Cursor (_Extended (trail, segment,
                            Unmodified (indexing_rule, hashed_is_transitive)),
                   below, context))
  | _ -> Error "Attempted to go down an extender but did not find an extender"

(* Go up 1 level of tree.  
   Note that this can be more than 1 levels in segments,
   because of the extenders
*)
let go_up (Cursor (trail, node, context))  = match trail with
  | Top -> Error "cannot go above top"
  | Left (prev_trail, right,
          Unmodified (indexing_rule, hashed_is_transitive)) ->
      let new_node =
        View (_Internal (node, right, indexing_rule, hashed_is_transitive))
      in Ok (_Cursor (prev_trail, new_node, context))

  | Right (left, prev_trail,
           Unmodified (indexing_rule, hashed_is_transitive)) ->
      let new_node =
        View (_Internal (left, node, indexing_rule, hashed_is_transitive))
      in Ok (_Cursor (prev_trail, new_node, context))

  | Budded (prev_trail,
            Unmodified (indexing_rule, hashed_is_transitive)) ->
      let new_node =
        View (_Bud (Some node, indexing_rule, hashed_is_transitive))
      in Ok (_Cursor (prev_trail, new_node, context))

  | Extended (prev_trail, segment,
              Unmodified (indexing_rule, hashed_is_transitive)) ->
    let new_node =
      View (_Extender (segment, node, indexing_rule, hashed_is_transitive))
    in Ok (_Cursor (prev_trail, new_node, context))

  (* Modified cases. *)

  | Left (prev_trail, right, Modified_Left) ->
      let internal = NotHashed.internal node right Left_Not_Indexed in
      Ok (attach prev_trail internal context)

  | Right (left, prev_trail, Modified_Right) ->
      let internal = NotHashed.internal left node Right_Not_Indexed in
      Ok (attach prev_trail internal context)

  | Budded (prev_trail, Modified_Left) ->
      let bud = NotHashed.bud @@ Some node in
      Ok (attach prev_trail bud context)

  | Extended (prev_trail, segment, Modified_Left) ->
      let extender = NotHashed.extend segment node in
      Ok (attach prev_trail extender context)

  | Left (_, _, Modified_Right)|Right (_, _, Modified_Left)|
    Budded (_, Modified_Right)|Extended (_, _, Modified_Right) -> assert false

let rec go_top (Cursor (trail, _, _) as c) =
  match trail with
  | Top -> Ok c
  | _ -> go_up c >>= go_top

let parent c =
  let rec aux c =
    match c with
    | Cursor (_, Disk _, _) -> assert false (* impossible *)
    | Cursor (_, View (Bud _), _) -> Ok c (* already at the top of subtree *)
    | _ -> go_up c >>= fun c -> aux c
  in
  aux c

let unify_extenders prev_trail node context = match node with
  | Disk (_, Is_Extender) -> Error "unify_exenders: Disk is not allowed"
  | View (Extender (seg, n, _, _)) ->
      begin match prev_trail with
        | Extended (prev_trail', seg', _mr) ->
            Ok (attach prev_trail' (NotHashed.extend (Segment.concat seg' seg) n) context)
        | _ -> Ok (attach prev_trail node context)
      end
  | _ -> Ok (attach prev_trail node context)

let rec remove_up trail context = match trail with
  | Top -> Error "cannot remove top"
  | Budded (prev_trail, _) ->
      Ok (attach prev_trail (NotHashed.bud None) context)
  | Extended (prev_trail, _, _) -> remove_up prev_trail context
  (* for Left and Right, we may need to squash Extenders in prev_trail *)
  | Left (prev_trail, right, _) ->
      let n = NotHashed.extend Segment.(of_side_list [Right]) right in
      unify_extenders prev_trail n context
  | Right (left, prev_trail, _) ->
      let n = NotHashed.extend Segment.(of_side_list [Left]) left in
      unify_extenders prev_trail n context

(* Let [c] is a cursor which points an Extender, whose segment is [common_prefix @ remaining_extender].
   [diverge c (common_prefix, remaining_extender, remaining_segment)] diverges a segment of [c] in the middle
   and create a path to [common_prefix @ remaining_segnet]. 
   It returns the newly created trail.
*)
let diverge (Cursor (trail, extender, _context)) (common_prefix, remaining_extender, remaining_segment) =
  match extender with
  | View (Extender (_seg, n, _ir, _hit)) -> (* _seg = common_prefix @ remaining_extender *)
      begin match Segment.cut remaining_segment, Segment.cut remaining_extender with
        | None, _ -> Error "diverge: remaining_segment is empty"
        | _, None -> Error "diverge: remaining_extender is empty"
        | Some (side, seg), Some (side', seg') -> 
            (* go down along common_prefix *)
            assert (side <> side');
            let trail = 
              if Segment.is_empty common_prefix then trail 
              else _Extended (trail, common_prefix, Modified_Left)
            in
            let n' = NotHashed.extend seg' n in
            match side with
            | Segment.Left -> 
                if Segment.is_empty seg then
                  Ok (_Left (trail, n', Modified_Left))
                else
                  Ok (_Extended (_Left (trail, n', Modified_Left),
                            seg, Modified_Left))
            | Segment.Right -> 
                if Segment.is_empty seg then
                  Ok (_Right (n', trail, Modified_Right))
                else
                  Ok (_Extended (_Right (n', trail, Modified_Right),
                            seg, Modified_Left))
      end
  | _ -> Error "diverge: not an Extender"

(* Follow the segment from the cursor. If the segment terminates 
  or diverges in the middle of an extender, it returns the common prefix
  information. 
*)
type access_result =
  | Empty_bud (* The bud is empty *)
  | Collide of cursor * view (* The segment was blocked by an existing leaf or bud *)
  | Middle_of_extender of cursor * Segment.t * Segment.t * Segment.t (* The segment ends or diverges at the middle of an Extender with the common prefix, the remaining extender, and the rest of segment *)
  | Reached of cursor * view (* just reached to a node *)

let access_gen cur seg =
  let access_gen_aux cur segment =
    (* returns the cursor found by following the segment from the given cursor *)
    let rec aux (Cursor (trail, n, context)) segment =
      let v = view context n in
      let cur = _Cursor (trail, View v, context) in
      match Segment.cut segment with
      | None -> Ok (Reached (cur, v))
      | Some (dir, segment_rest) ->
          match v with
          | Leaf _ | Bud _ ->  Ok (Collide (cur, v))
          | Internal (l, r,
                      internal_node_indexing_rule,
                      hashed_is_transitive) -> begin
              match dir with
              | Left ->
                let new_trail =
                  _Left (
                    trail, r,
                    Unmodified (
                      internal_node_indexing_rule,
                      hashed_is_transitive)) in
                aux (_Cursor (new_trail, l, context)) segment_rest
              | Right ->
                let new_trail = 
                  _Right (
                    l, trail,
                    Unmodified (
                      internal_node_indexing_rule, 
                      hashed_is_transitive)) in
                aux (_Cursor (new_trail, r, context)) segment_rest
            end
          | Extender (extender, node_below,
                      indexing_rule,
                      hashed_is_transitive) ->
            let (shared, remaining_extender, remaining_segment) =
              Segment.common_prefix extender segment in
            if remaining_extender = Segment.empty then
              let new_trail =
                _Extended (trail, extender,
                         Unmodified (
                           indexing_rule,
                           hashed_is_transitive)) in
              aux (_Cursor (new_trail, node_below, context)) remaining_segment
            else
              Ok (Middle_of_extender (cur, shared, remaining_extender, remaining_segment))
    in
    aux cur segment
  in
  go_below_bud cur >>= function
  | None -> Ok Empty_bud 
  | Some cur -> access_gen_aux cur seg

let error_access = function
  | Empty_bud -> Error "Nothing beneath this bud"
  | Collide _ -> Error "Reached to a leaf or bud before finishing"
  | Middle_of_extender (_, _, _, []) -> 
      Error "Finished at the middle of an Extender"
  | Middle_of_extender (_, _, _, _) -> 
      Error "Diverged in the middle of an Extender"
  | Reached (_, Bud _) -> Error "Reached to a Bud"
  | Reached (_, Leaf _) -> Error "Reached to a Leaf"

  | Reached (_, Internal _) -> Error "Reached to an Internal"
  | Reached (_, Extender _) -> Error "Reached to an Extender"
  
let subtree cur seg =
   access_gen cur seg >>= function
   | Reached (cur, Bud _) -> Ok cur
   | res -> error_access res

let get cur seg = 
  access_gen cur seg >>= function
  | Reached (_, Leaf (v, _, _)) -> Ok v
  | res -> error_access res

let empty context =
  (* A bud with nothing underneath, i.e. an empty tree or an empty sub-tree. *)
  _Cursor (_Top, NotHashed.bud None, context)

let delete cur seg =
  access_gen cur seg >>= function
  | Reached (Cursor (trail, _, context), (Bud _ | Leaf _)) -> 
      remove_up trail context 
      >>= parent
  | res -> error_access res

let alter (Cursor (trail, _, context) as cur) segment alteration =
  access_gen cur segment >>= function
  | Empty_bud -> 
      alteration None >>= fun n' ->
      let n' = NotHashed.extend segment n' in
      let n' = NotHashed.bud (Some n') in
      Ok (attach trail n' context)
  | (Reached (c, _) | Middle_of_extender (c, _, _, _::_) as res) ->
      (* XXX cleanup required *)
      let segsopt = match res with
        | Reached _ -> None
        | Middle_of_extender (_c, shared, rest_extender, rest_segment) ->
            Some (shared, rest_extender, rest_segment)
        | _ -> assert false
      in
      begin match segsopt with
        | None -> 
            (* Should we view the node? *)
            let Cursor (trail, n, context) = c in
            let v = view context n in
            Ok (trail, Some v)
        | Some segs -> diverge c segs >>| fun trail -> (trail, None)
      end >>= fun (trail, nopt) ->
      alteration nopt >>= fun n -> 
      let c = attach trail n context in
      (* XXX alter cannot dive into more than one bud, therefore
         go_ups should be much simpler *)
      (* go back along the segments to the "original" position *)
      let rec go_ups (Cursor (trail, _node, _context) as c ) ss = 
        match trail, ss with
        | Budded _, _ -> go_up c >>= fun c -> go_ups c ss
        | _, [] -> return c
        | (Left _ | Right _), _ ->
            go_up c >>= fun c -> go_ups c (List.tl ss)
        | Extended (_, segs, _), _ ->
            let ss' = 
              let rec f ss segs = match ss, segs with
                | ss, [] -> ss
                | _::ss, _::segs -> f ss segs
                | [], _ -> assert false
              in
              f ss segs
            in
            go_up c >>= fun c -> go_ups c ss'
        | Top, _ -> assert false
      in
      go_ups c segment
  | res -> error_access res

let update cur segment value =
  access_gen cur segment >>= function
  | Reached (Cursor (trail, _, context), Leaf _) -> 
      parent (attach trail (View (_Leaf (value, Not_Indexed, Not_Hashed))) context)
  | res -> error_access res

let upsert cur segment value =
  alter cur segment (fun x ->
     let y = Ok (NotHashed.leaf value) in 
     match x with
     | None -> y
     | Some (Leaf _) -> y
     | Some _ -> Error "a non Leaf node already presents for this path")

let insert cur segment value =
  alter cur segment (function
      | None -> Ok (View (_Leaf (value, Not_Indexed, Not_Hashed)))
      | Some _ -> Error "a node already presents for this path")

let create_subtree cur segment =
  alter cur segment (function
      | None -> Ok (NotHashed.bud None)
      | Some _ -> Error "a node already presents for this path")

let subtree_or_create cur segment =
  (* XXX inefficient.  create_subtree should have an option not to go back to the original position *)
  let cur = 
    match create_subtree cur segment with
    | Ok cur -> cur
    | Error _ -> cur
  in
  subtree cur segment

let snapshot cur seg1 seg2 =
  access_gen cur seg1 >>= function
  | Reached (_cur1, (Bud _ as view)) ->
      alter cur seg2 (function
          | None -> Ok (View view)
          | Some _ -> Error "a node already presents for this path")
  | res -> error_access res


(** Multi Bud level interface *)
let dive ~float cur segs f =
  let rec ups cur = function
    | [] -> Ok cur
    | _seg::segs -> 
        parent cur >>= fun cur -> ups cur segs
  in
  let rec aux hist cur = function
    | [] -> assert false
    | [seg] -> 
        f cur seg >>= fun (cur, v) ->
        if float then ups cur hist >>| fun cur -> (cur, v)
        else Ok (cur, v)
    | seg::segs ->
        subtree cur seg >>= fun cur ->
        aux (seg::hist) cur segs
  in
  aux [] cur segs
  
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

  
  

let stat (Cursor (_,_,context)) = Context.stat context

  
