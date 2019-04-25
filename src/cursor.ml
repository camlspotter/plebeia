(** Cursor (zipper) based tree operations *)

open Utils
open Error
open Types
open Node

let attach trail node context =
  (* Attaches a node to a trail even if the indexing type and hashing type is incompatible with
     the trail by tagging the modification. Extender types still have to match. *)
  match trail with
  | Top -> _Cursor (_Top, node, context)
  | Left (prev_trail, right, _, indexed_implies_hashed) ->
      _Cursor (_Left (prev_trail, right, Modified_Left, indexed_implies_hashed), node, context)
  | Right (left, prev_trail, _, indexed_implies_hashed) ->
      _Cursor (_Right (left, prev_trail, Modified_Right, indexed_implies_hashed), node, context)
  | Budded (prev_trail, _, indexed_implies_hashed) ->
      _Cursor (_Budded (prev_trail, Modified_Left, indexed_implies_hashed), node, context)
  | Extended (prev_trail, segment, _, indexed_implies_hashed) ->
      _Cursor (_Extended (prev_trail, segment, Modified_Left, indexed_implies_hashed), node, context)

(* Tools to create Not_Indexed and Not_Hashed nodes *)
module NotHashed : sig
  val leaf : Value.t -> node
  val extend : Path.segment -> node -> node
  val bud : node option -> node
  val internal : node -> node -> indexing_rule -> node
end = struct
  let leaf v = View (_Leaf (v, Not_Indexed, Not_Hashed, Not_Indexed_Any))

  let extend : Path.segment -> node -> node = fun segment node ->
    if segment = Path.empty then node
    else 
      match node with
      | View (Extender (seg, n, _, _, _)) ->
          View (_Extender (Path.concat segment seg, n, Not_Indexed, Not_Hashed, Not_Indexed_Any))
      | _ ->
          View (_Extender (segment, node, Not_Indexed, Not_Hashed, Not_Indexed_Any))
  let bud no = View (_Bud (no, Not_Indexed, Not_Hashed, Not_Indexed_Any))

  let internal n1 n2 i = 
    View (_Internal (n1, n2, i, Not_Hashed, Not_Indexed_Any))
end

let go_below_bud (Cursor (trail, n, context)) =
  (* This function expects a cursor positionned on a bud and moves it one step below. *)
  match view context n with
  | Bud (None,  _, _, _) -> Ok None
  | Bud (Some below, indexing_rule, hashed_is_transitive, indexed_implies_hashed) ->
      Ok (Some (_Cursor (
          _Budded (trail, Unmodified (indexing_rule, hashed_is_transitive), indexed_implies_hashed), below,  context)))
  | _ -> Error "Attempted to navigate below a bud, but got a different kind of node."

let go_side side (Cursor (trail, n, context)) =
  (* Move the cursor down left or down right in the tree, assuming we are on an internal node. *)
  match view context n with
  | Internal (l, r, indexing_rule, hashed_is_transitive, indexed_implies_hashed) ->
      Ok (match side with
          | Path.Right ->
              _Cursor (_Right (l, trail,
                             Unmodified (indexing_rule, hashed_is_transitive),
                             indexed_implies_hashed), r, context)
          | Path.Left ->
              _Cursor (_Left (trail, r,
                            Unmodified (indexing_rule, hashed_is_transitive),
                            indexed_implies_hashed), l, context))
  | _ -> Error "Attempted to navigate right or left of a non internal node"

let go_down_extender (Cursor (trail, n, context)) =
  (* Move the cursor down the extender it points to. *)
  match view context n with
  | Extender (segment, below, indexing_rule, hashed_is_transitive, indexed_implies_hashed) ->
      Ok (_Cursor (_Extended (trail, segment,
                            Unmodified (indexing_rule, hashed_is_transitive),
                            indexed_implies_hashed), below, context))
  | _ -> Error "Attempted to go down an extender but did not find an extender"

let go_up (Cursor (trail, node, context))  = match trail with
  | Top -> Error "cannot go above top"
  | Left (prev_trail, right,
          Unmodified (indexing_rule, hashed_is_transitive), indexed_implies_hashed) ->
      let new_node =
        View (_Internal (node, right, indexing_rule, hashed_is_transitive, indexed_implies_hashed))
      in Ok (_Cursor (prev_trail, new_node, context))

  | Right (left, prev_trail,
           Unmodified (indexing_rule, hashed_is_transitive), indexed_implies_hashed) ->
      let new_node =
        View (_Internal (left, node, indexing_rule, hashed_is_transitive, indexed_implies_hashed))
      in Ok (_Cursor (prev_trail, new_node, context))

  | Budded (prev_trail,
            Unmodified (indexing_rule, hashed_is_transitive), indexed_implies_hashed) ->
      let new_node =
        View (_Bud (Some node, indexing_rule, hashed_is_transitive, indexed_implies_hashed))
      in Ok (_Cursor (prev_trail, new_node, context))

  | Extended (prev_trail, segment,
              Unmodified (indexing_rule, hashed_is_transitive), indexed_implies_hashed) ->
    let new_node =
      View (_Extender (segment, node, indexing_rule, hashed_is_transitive, indexed_implies_hashed))
    in Ok (_Cursor (prev_trail, new_node, context))

  (* Modified cases. *)

  | Left (prev_trail, right, Modified_Left, _) ->
      let internal = NotHashed.internal node right Left_Not_Indexed in
      Ok (attach prev_trail internal context)

  | Right (left, prev_trail, Modified_Right, _) ->
      let internal = NotHashed.internal left node Right_Not_Indexed in
      Ok (attach prev_trail internal context)

  | Budded (prev_trail, Modified_Left, _) ->
      let bud = NotHashed.bud @@ Some node in
      Ok (attach prev_trail bud context)

  | Extended (prev_trail, segment, Modified_Left, _) ->
      let extender = NotHashed.extend segment node in
      Ok (attach prev_trail extender context)

  | Left (_, _, Modified_Right, _)|Right (_, _, Modified_Left, _)|
    Budded (_, Modified_Right, _)|Extended (_, _, Modified_Right, _) -> assert false

(* not used
let rec go_top (Cursor (trail, _, _) as c) =
  match trail with
  | Top -> Ok c
  | _ -> go_up c >>= go_top
*)

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
  | View (Extender (seg, n, _, _, _)) ->
      begin match prev_trail with
        | Extended (prev_trail', seg', _mr, _iih) ->
            Ok (attach prev_trail' (NotHashed.extend (Path.concat seg' seg) n) context)
        | _ -> Ok (attach prev_trail node context)
      end
  | _ -> Ok (attach prev_trail node context)

let rec remove_up trail context = match trail with
  | Top -> Error "cannot remove top"
  | Budded (prev_trail, _, _) ->
      Ok (_Cursor (prev_trail, NotHashed.bud None, context))
  | Extended (prev_trail, _, _, _) -> remove_up prev_trail context
  (* for Left and Right, we may need to squash Extenders in prev_trail *)
  | Left (prev_trail, right, _, _) ->
      let n = NotHashed.extend Path.(of_side_list [Right]) right in
      unify_extenders prev_trail n context
  | Right (left, prev_trail, _, _) ->
      let n = NotHashed.extend Path.(of_side_list [Left]) left in
      unify_extenders prev_trail n context

(* Let [c] is a cursor which points an Extender, whose segment is [common_prefix @ remaining_extender].
   [diverge c (common_prefix, remaining_extender, remaining_segment)] diverges a segment of [c] in the middle
   and create a path to [common_prefix @ remaining_segnet]. 
   It returns the newly created trail.
*)
let diverge (Cursor (trail, extender, _context)) (common_prefix, remaining_extender, remaining_segment) =
  match extender with
  | View (Extender (_seg, n, _ir, _hit, _iih)) -> (* _seg = common_prefix @ remaining_extender *)
      begin match Path.cut remaining_segment, Path.cut remaining_extender with
        | None, _ -> Error "diverge: remaining_segment is empty"
        | _, None -> Error "diverge: remaining_extender is empty"
        | Some (side, seg), Some (side', seg') -> 
            (* go down along common_prefix *)
            assert (side <> side');
            let trail = 
              if Path.is_empty common_prefix then trail 
              else _Extended (trail, common_prefix, Modified_Left, Not_Indexed_Any)
            in
            let n' = NotHashed.extend seg' n in
            match side with
            | Path.Left -> 
                if Path.is_empty seg then
                  Ok (_Left (trail, n', Modified_Left, Not_Indexed_Any))
                else
                  Ok (_Extended (_Left (trail, n', Modified_Left, Not_Indexed_Any),
                            seg, Modified_Left, Not_Indexed_Any))
            | Path.Right -> 
                if Path.is_empty seg then
                  Ok (_Right (n', trail, Modified_Right, Not_Indexed_Any))
                else
                  Ok (_Extended (_Right (n', trail, Modified_Right, Not_Indexed_Any),
                            seg, Modified_Left, Not_Indexed_Any))
      end
  | _ -> Error "diverge: not an Extender"

(* Follow the segment from the cursor. If the segment terminates 
  or diverges in the middle of an extender, it returns the common prefix
  information. 
*)
let access_gen cur segment =
  (* returns the cursor found by following the segment from the given cursor *)
  let rec aux (Cursor (trail, n, context) as cur) segment =
    let v = view context n in
    match Path.cut segment with
    | None -> Ok (cur, None)
    | Some (dir, segment_rest) ->
        match v with
        | Leaf _ -> Error "Reached a leaf before finishing"
        | Bud _ ->  Error "Reached a bud before finishing"
        | Internal (l, r,
                    internal_node_indexing_rule,
                    hashed_is_transitive,
                    indexed_implies_hashed) -> begin
            match dir with
            | Left ->
              let new_trail =
                _Left (
                  trail, r,
                  Unmodified (
                    internal_node_indexing_rule,
                    hashed_is_transitive),
                  indexed_implies_hashed) in
              aux (_Cursor (new_trail, l, context)) segment_rest
            | Right ->
              let new_trail = 
                _Right (
                  l, trail,
                  Unmodified (
                    internal_node_indexing_rule, 
                    hashed_is_transitive),
                  indexed_implies_hashed) in
              aux (_Cursor (new_trail, r, context)) segment_rest
          end
        | Extender (extender, node_below,
                    indexing_rule,
                    hashed_is_transitive,
                    indexed_implies_hashed) ->
          let (_, remaining_extender, remaining_segment) as common_prefix =
            Path.common_prefix extender segment in
          if remaining_extender = Path.empty then
            let new_trail =
              _Extended (trail, extender,
                       Unmodified (
                         indexing_rule,
                         hashed_is_transitive),
                       indexed_implies_hashed) in
            aux (_Cursor (new_trail, node_below, context)) remaining_segment
          else
            if remaining_segment = Path.empty then
              Error "Finished in the middle of an Extender"
            else
              (* diverge *)
              Ok (cur, Some common_prefix)
  in
  aux cur segment

let subtree cur segment =
  go_below_bud cur >>= function
  | None -> Error "Nothing beneath this bud"
  | Some cur ->
      access_gen cur segment >>= function 
      | (_, Some _) -> Error "Terminated or diverged in the middle of an Extender"
      | (Cursor (trail, n, context), None) -> 
          match view context n with
          | Bud _ as v -> Ok (_Cursor (trail, View v, context))
          | _ -> Error "Reached to a non Bud"

let get cur segment = 
  go_below_bud cur >>= function
  | None -> Error "Nothing beneath this bud"
  | Some cur ->
      access_gen cur segment >>= function 
      | (_, Some _) -> Error "Terminated or diverged in the middle of an Extender"
      | (Cursor (_, n, context), None) -> 
          match view context n with
          | Leaf (v, _, _, _) -> Ok v
          | _ -> Error "Reached to a non Leaf"

let empty context =
  (* A bud with nothing underneath, i.e. an empty tree or an empty sub-tree. *)
  _Cursor (_Top, NotHashed.bud None, context)

let delete cur segment =
  go_below_bud cur >>= function
  | None -> Error "Nothing beneath this bud"
  | Some cur ->
      access_gen cur segment >>= function 
      | (_, Some _) -> Error "Terminated or diverged in the middle of an Extender"
      | (Cursor (trail, _n, context), None) -> 
          remove_up trail context 
          >>= parent 

let alter (Cursor (trail, _, context) as cur) segment alteration =
  (* XXX 2 cases. not cool *)
  go_below_bud cur >>= function
  | None ->
      alteration None >>= fun n' ->
      let n' = NotHashed.extend segment n' in
      let n' = NotHashed.bud (Some n') in
      Ok (_Cursor (trail, n', context))
  | Some cur -> 
      access_gen cur segment >>= fun (c,segsopt) ->
      begin match segsopt with
        | None -> 
            (* Should we view the node? *)
            let Cursor (trail, n, context) = c in
            let v = view context n in
            Ok (trail, Some v)
        | Some segs -> diverge c segs >>| fun trail -> (trail, None)
      end >>= fun (trail, nopt) ->
      alteration nopt >>= fun n -> 
      parent @@ attach trail n context

let upsert cur segment value =
  alter cur segment (fun x ->
     let y = Ok (NotHashed.leaf value) in 
     match x with
     | None -> y
     | Some (Leaf _) -> y
     | Some _ -> Error "a non Leaf node already present for this path")

let insert cur segment value =
  alter cur segment (function
      | None -> Ok (View (_Leaf (value, Not_Indexed, Not_Hashed, Not_Indexed_Any)))
      | Some _ -> Error "a node already present for this path")

let create_subtree cur segment =
  alter cur segment (function
      | None -> Ok (NotHashed.bud None)
      | Some _ -> Error "a node already present for this path")

let root context h = 
  match Hashtbl.find_opt context.Context.roots_table h with
  | None -> Error "The root is not found in the roots_table"
  | Some i -> Ok (_Cursor( _Top, Disk(i, Not_Extender), context))

type where_from =
  | Down_to of dir
  | Up_from of dir

and dir =
  | Left
  | Right
  | Center

let rec traverse log rev_path (Cursor (trail, n, context)) = 
  let v = match n with
    | Disk (i, ewit) -> load_node context i ewit
    | View v -> v
  in
  let c = _Cursor (trail, View v, context) in
  match v, log, rev_path with
  | Leaf _, [], _ -> c
  | Leaf _, Down_to d :: log', _ :: rev_path -> 
      let c = from_Ok @@ go_up c in
      traverse (Up_from d :: log') rev_path c
  | Bud (None, _, _, _), [], _ -> c
  | Bud (None, _, _, _), Down_to d :: log', rev_path -> 
      let c = from_Ok @@ go_up c in
      traverse (Up_from d :: log') rev_path c
  | Bud (Some _, _, _, _), Up_from _ :: [], _ -> c
  | Bud (Some _, _, _, _), Up_from _ :: Down_to d :: log', _ :: rev_path ->
      let c = from_Ok @@ go_up c in
      traverse (Up_from d :: log') rev_path c
  | Bud (Some _, _, _, _), (Down_to _ :: _ | []), rev_path ->
      let c = from_Some @@ from_Ok @@ go_below_bud c in
      traverse (Down_to Center :: log) rev_path c
  | Internal _, (Down_to _ :: _ | []), rev_path ->
      let c = from_Ok @@ go_side Path.Left c in
      traverse (Down_to Left :: log) (Path.Left :: rev_path) c
  | Internal _, Up_from Left :: log', rev_path ->
      let c = from_Ok @@ go_side Path.Right c in
      traverse (Down_to Right :: log') (Path.Right :: rev_path) c
  | Internal _, Up_from Right :: [], _ -> c
  | Internal _, Up_from Right :: Down_to d :: log', _ :: rev_path ->
      let c = from_Ok @@ go_up c in
      traverse (Up_from d :: log') rev_path c
  | Extender (seg, _, _, _, _), (Down_to _ :: _ | []), rev_path ->
      let c = from_Ok @@ go_down_extender c in
      traverse (Down_to Center :: log) (List.rev_append (seg :> Path.side list) rev_path) c
  | Extender (_, _, _, _, _), Up_from _ :: [], _ -> c
  | Extender (seg, _, _, _, _), Up_from _ :: Down_to d :: log', rev_path ->
      let rev_path =
        let rec f seg rev_path = match seg, rev_path with
          | s::seg, s'::rev_path -> assert (s = s');  f seg rev_path
          | [], _ -> rev_path
          | _, [] -> assert false
        in
        f (seg :> Path.side list) rev_path
      in
      let c = from_Ok @@ go_up c in
      traverse (Up_from d :: log') rev_path c
  | _ -> assert false

let snapshot _ _ _ = failwith "not implemented"
